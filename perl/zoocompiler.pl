#!/usr/bin/perl -w

use Getopt::Long;
use Pod::Usage;
use XML::Writer;

# parse options
my $man = 0;
my $help = 0;
my $debug = 0;

GetOptions('help|?' => \$help, man => \$man, debug => \$debug) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

pod2usage(2) unless @ARGV == 1;

# data structures
my %typeindex;  # $typeindex{$type}
my %pvar;    # $pvar{$type} = [$var1,$var2,$var3,...]
my %pvbits;   # $pvbits{$type}->{$var}
my %pvoffset;   # $pvoffset{$type}->{$var}
my (%hue, %sat, %bri);   # $hue{$type} = [[$const1,$var1,$mul1], [$const2,$var2,$mul2], ...]   etc
my %ruleTags;  # $ruleTags{$type}->[$ruleIndex] = { "rate" => $rate, "overload" => $overload, ... }
my %test;  # $test{$type}->[$ruleIndex]->[$testIndex] = [$x,$y,$type,$var,$opcode,$rhs,\%otherTags]
my %op;  # $op{$type}->[$ruleIndex]->[$opIndex] = [$xSrc,$ySrc,$typeSrc,$varSrc,$xDest,$yDest,$typeDest,$varDest,$offset,\%otherTags]

# parse input file
my ($zgfilename) = @ARGV;
local *ZG;
open ZG, "<$zgfilename" or die "Couldn't open $zgfilename: $!";
my @zg = <ZG>;
close ZG;

# strip off irrelevant crud
grep (s/\/\/.*$//, @zg);  # C++-style comments
grep (s/;\s*$//, @zg);   # semicolons at ends of lines
grep (s/^\s*(.*?)\s*$/$1/, @zg);  # whitespace at beginning/end of line
grep (s/\s+/ /, @zg);  # multiple whitespace

# loop through file
my ($type, $nRule);
local $_;
my $types = 0;
while (@zg) {
    $_ = shift @zg;
    if (/^type (\S+) (.*)$/) {
	my $varstr;
	($type, $varstr) = ($1, $2);
	$typeindex{$type} = $types++;
	$nRule = 0;
	my $offset = 0;
	while ($varstr =~ /([A-Za-z_][A-Za-z_\d]*)\s?\(\s?(\d+)\s?\)/g) {
	    my ($varname, $varbits) = ($1, $2);
	    die "You cannot use 'type' as a var name for $type" if $varname eq "type";
	    push @{$pvar{$type}}, $varname;
	    $pvbits{$type}->{$varname} = $varbits;
	    $pvoffset{$type} = $offset;
	    $offset += $varbits;
	    die "More than 64 bits in type $type" if $offset > 64;
	}
    } elsif (defined $type) {
	if (/^hue\s?=\s?(.*?)$/) {
	    $hue{$type} = parseColor($1);
	} elsif (/^sat\s?=\s?(.*?)$/) {
	    $sat{$type} = parseColor($1);
	} elsif (/^bri\s?=\s?(.*?)$/) {
	    $bri{$type} = parseColor($1);
	} elsif (/^\{$/) {
	    # rule block
	    my (%tag, %loc, %loctype, @test, @op);
	    while (@zg) {
		$_ = shift @zg;
		last if /^\}$/;
		if (/^loc ([A-Za-z_][A-Za-z_\d]*)\s?\(\s?([\+\-\d]+)\s?[\s,]\s?([\+\-\d]+)\s?\)$/) {
		    my ($locid, $x, $y) = ($1, $2, $3);
		    die "Duplicate loc" if defined $loc{$locid};
		    $loc{$locid} = [$x,$y];
		    warn "loc=$locid x=$x y=$y" if $debug;
		} elsif (/^if ([A-Za-z_\d\.\s]+)\s?(=|\!=|>|>=|<|<=)\s?([^\s\(]+)\s?(.*)$/) {
		    my ($lhs, $op, $rhs, $ignore) = ($1, $2, $3, $4);
		    my ($loc, $var) = getLocVar ($lhs, "type", \%loc);
		    if ($var eq "type") {
			$loctype{$loc} = $rhs;
		    }

		    if (defined($var) && $var ne "type" && $var ne "*" && !defined($loctype{$loc})) {
			die "Can't access $loc.$var because type of $loc is not bound\n";
		    }

		    push @{$test{$type}->[$nRule]}, [@{$loc{$loc}}, $loctype{$loc}, $var, $op, $rhs, parseTags($ignore)];

		    if ($debug) {
			warn "loc=$loc var=$var op='$op' rhs=$rhs ignore=$ignore";
		    }

		} elsif (/^do ([A-Za-z_\d\.\s]+)\s?=\s?([^\(]+)(.*)$/) {
		    my ($lhs, $rhs, $fail) = ($1, $2, $3);
		    my ($lhsLoc, $lhsVar) = getLocVar ($lhs, "*", \%loc);
		    my ($rhsLoc, $rhsVar);
		    my $offset = 0;
		    if ($rhs =~ /(.*)\+\s?(\d+)/) {
			$offset = $2;
			($rhsLoc, $rhsVar) = getLocVar ($1, undef, \%loc);
		    } elsif ($rhs =~ /(.*)\-\s?(\d+)/) {
			$offset = -$2;
			($rhsLoc, $rhsVar) = getLocVar ($1, undef, \%loc);
		    } elsif ($rhs =~ /\s?\d+\s?/) {
			$offset = $rhs;
		    } else {
			($rhsLoc, $rhsVar) = getLocVar ($rhs, "*", undef);
			if (!defined ($loc{$rhsLoc})) {   # is this an actual location ID? if not, it must be a literal type
			    ($rhsLoc, $rhsVar) = (undef, $lhsVar);   # set rhsVar=lhsVar to avoid errors when lhsVar="*"
			    $offset = $rhs;   # have to turn this literal type into an index later
			}
		    }

		    if ((defined($lhsVar) && $lhsVar eq "*") xor (defined($rhsVar) && $rhsVar eq "*")) {
			die "If one side of a 'do' expression involves the whole state, then both sides must.\nOffending line:\n$_\n";
		    }

		    if (defined($lhsVar) && $lhsVar eq "type") {
			$loctype{$lhsLoc} = (defined($rhsLoc) && $rhsVar eq "type") ? $loctype{$rhsLoc} : undef;
		    }

		    if (defined($lhsVar) && $lhsVar ne "type" && $lhsVar ne "*" && !defined($loctype{$lhsLoc})) {
			die "Can't access $lhsLoc.$lhsVar because type of $lhsLoc is not bound\n";
		    }

		    if (defined($rhsVar) && $rhsVar ne "type" && $rhsVar ne "*" && !defined($loctype{$rhsLoc})) {
			die "Can't access $rhsLoc.$rhsVar because type of $rhsLoc is not bound\n";
		    }

		    push @{$op{$type}->[$nRule]}, [@{$loc{$lhsLoc}}, $loctype{$lhsLoc}, $lhsVar, defined($rhsLoc) ? (@{$loc{$rhsLoc}}, $loctype{$rhsLoc}) : (undef,undef,undef), $rhsVar, $offset, parseTags($fail)];

		    if ($debug) {
			$rhsLoc = "" unless defined $rhsLoc;
			$rhsVar = "" unless defined $rhsVar;
			warn "lhsLoc=$lhsLoc lhsVar=$lhsVar rhsLoc=$rhsLoc rhsVar=$rhsVar offset=$offset fail=$fail";
		    }

		} elsif (/\(.*\)/) {
		    parseTags ($_, \%tag);

		} elsif (/\S/) {
		    die "Syntax error: $_\n";
		}
	    }

	    $ruleTags{$type}->[$nRule] = \%tag;
	    $test{$type}->[$nRule] = \@test;
	    $op{$type}->[$nRule] = \@op;

	    ++$nRule;

	} elsif (/\S/) {
	    die "Syntax error: $_\n";
	}
    } elsif (/\S/) {
	die "Syntax error: $_\n";
    }
}

# TODO: generate XML

exit;

sub parseTags {
    my ($line, $tagRef) = @_;
    $tagRef = {} unless defined $tagRef;
    while ($line =~ /\(\s?(\S+)\s([^\)]+|\"[^\"]+\")\s?\)/g) {
	my ($tag, $val) = ($1, $2);
	die "Duplicate tag $tag\n" if exists $tagRef->{$tag};
	$tagRef->{$tag} = $val;
    }
    return $tagRef;
}

sub parseColor {
    my ($colexpr) = @_;
    my @term = split /\s*\+\s*/, $colexpr;
    my $const = 0;
    my @varmul;
    for my $term (@term) {
	if ($term =~ /^\-?\s*\d+$/) {
	    $const += $term;
	} elsif ($term =~ /^(\S+)\s?\*\s?(\-?\d+)$/) {
	    my ($var, $mul) = ($1, $2);
	    push @varmul, [$var, $mul];
	} elsif ($term =~ /^(\-?\d+)\s?\*\s?(\S+)$/) {
	    my ($mul, $var) = ($1, $2);
	    push @varmul, [$var, $mul];
	} else {
	    push @varmul, [$term, 1];
	}
    }
    return [[$const, @varmul==0 ? (undef,undef) : @{shift @varmul}], map ([0,@$_], @varmul)];
}

sub getLocVar {
    my ($expr, $defaultVar, $locRef) = @_;
    $expr =~ s/\s//g;
    my ($loc, $var);
    if ($expr =~ /(.+)\.(.+)/) {
	($loc, $var) = ($1, $2);
    } else {
	die "In '$expr': no default var in that context" unless defined $defaultVar;
	($loc, $var) = ($expr, $defaultVar);
    }
    if (defined($loc) && defined($locRef) && !defined($locRef->{$loc})) {
	die "In '$expr': no location identifier '$loc' has been bound\n";
    }
    return ($loc, $var);
}

__END__

=head1 NAME
zoocompiler.pl - compile PixelZoo .zg files to XML grammars

=head1 SYNOPSIS

zoocompiler.pl [options] <.zg file>

 Options:
  -help            brief help message
  -man             full documentation

=head1 OPTIONS

=over 8

=item B<-help>

Prints a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=back

=head1 DESCRIPTION

B<This program> will compile a PixelZoo .zg file into an XML grammar.

=cut
