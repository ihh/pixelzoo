#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use XML::Twig;

# parse options
my $man = 0;
my $help = 0;
my $debug = 0;

my $cpp = "gcc -x c -E";

GetOptions('help|?' => \$help, man => \$man, debug => \$debug, 'preprocessor|cpp' => \$cpp) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

pod2usage(2) unless @ARGV == 1;

# data structures
my @type;
my %typeindex;  # $typeindex{$type}
my %pvar;    # $pvar{$type} = [$var1,$var2,$var3,...]
my %pvbits;   # $pvbits{$type}->{$var}
my %pvoffset;   # $pvoffset{$type}->{$var}
my %ptags;     # $ptags{$type} = [ ... ]
my (%hue, %sat, %bri);   # $hue{$type} = [[$const1,$var1,$mul1], [$const2,$var2,$mul2], ...]   etc
my %ruleTags;  # $ruleTags{$type}->[$ruleIndex] = [ "rate" => $rate, "overload" => $overload, ... ]
my %test;  # $test{$type}->[$ruleIndex]->[$testIndex] = [$x,$y,$type,$var,$opcode,$rhs,\%otherTags]
my %op;  # $op{$type}->[$ruleIndex]->[$opIndex] = [$xSrc,$ySrc,$typeSrc,$varSrc,$xDest,$yDest,$typeDest,$varDest,$accumFlag,$offset,\%otherTags]

# empty type
my $emptyType = "empty";
push @type, $emptyType;
$typeindex{$emptyType} = 0;

# parse input file
my ($zgfilename) = @ARGV;
local *ZG;
open ZG, "$cpp $zgfilename |" or die "Couldn't open $zgfilename: $!";
my @zg = <ZG>;
close ZG;

# strip off irrelevant crud
grep (s/(\/\/|#).*$//, @zg);  # C++-style comments, preprocessor directives
grep (s/[;\s]+\s*$//, @zg);   # semicolons at ends of lines
grep (s/^\s*(.*?)\s*$/$1/, @zg);  # whitespace at beginning/end of line
grep (s/\s+/ /, @zg);  # multiple whitespace

# loop through file
my ($type, $nRule);
local $_;
while (@zg) {
    $_ = shift @zg;
    if (/^type (\S+) (.*)$/) {
	my $varstr;
	($type, $varstr) = ($1, $2);
	die "Type '$type' is already defined" if exists $typeindex{$type};
	$typeindex{$type} = @type;
	push @type, $type;
	$nRule = 0;
	my $offset = 0;
	while ($varstr =~ /([A-Za-z_][A-Za-z_\d]*)\s?\(\s?(\d+)\s?\)/g) {
	    my ($varname, $varbits) = ($1, $2);
	    die "You cannot use 'type' as a var name for $type" if $varname eq "type";
	    push @{$pvar{$type}}, $varname;
	    $pvbits{$type}->{$varname} = $varbits;
	    $pvoffset{$type}->{$varname} = $offset;
	    $offset += $varbits;
	    die "More than 48 bits of vars for type $type" if $offset > 48;
	}
	$ptags{$type} = [];

    } elsif (defined $type) {
	if (/^hue\s?=\s?(.*?)$/) {
	    # hue
	    $hue{$type} = parseColor($1);

	} elsif (/^sat\s?=\s?(.*?)$/) {
	    # saturation
	    $sat{$type} = parseColor($1);

	} elsif (/^bri\s?=\s?(.*?)$/) {
	    # brightness
	    $bri{$type} = parseColor($1);

	} elsif (/^\{$/) {

	    # rule block
	    my (@tag, %loc, %loctype, @test, @op);
	    while (@zg) {
		$_ = shift @zg;
		last if /^\}$/;

		if (/^loc ([A-Za-z_][A-Za-z_\d]*)\s?\(\s?([\+\-\d]+)\s?[\s,]\s?([\+\-\d]+)\s?\)$/) {
		    # test
		    my ($locid, $x, $y) = ($1, $2, $3);
		    die "Duplicate loc" if defined $loc{$locid};
		    $loc{$locid} = [$x,$y];
		    warn "loc=$locid x=$x y=$y" if $debug;
		} elsif (/^if ([A-Za-z_\d\.\s]+)\s?(=|\!=|>|>=|<|<=)\s?([^\s\(]+)\s?(.*)$/) {
		    my ($lhs, $op, $rhs, $ignore) = ($1, $2, $3, $4);
# commented out because it's unnecessary: xmlboard parser should catch this
#		    $op = "==" if $op eq "=";

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

		} elsif (/^do ([A-Za-z_\d\.\s]+)\s?=\s?([^<]+)(.*)$/) {
		    # exec
		    my ($lhs, $rhs, $fail) = ($1, $2, $3);
		    my ($lhsLoc, $lhsVar) = getLocVar ($lhs, "*", \%loc);
		    my ($rhsLoc, $rhsVar, $accumFlag);
		    my $offset = 0;
		    if ($rhs =~ /(.*)\+\s?(\d+)/) {   # something + numeric constant
			$offset = $2;
			($rhsLoc, $rhsVar) = getLocVar ($1, undef, \%loc);
			$accumFlag = 1;

		    } elsif ($rhs =~ /(.*)\-\s?(\d+)/) {   # something - numeric constant
			$offset = -$2;
			($rhsLoc, $rhsVar) = getLocVar ($1, undef, \%loc);
			$accumFlag = 1;

		    } elsif ($rhs =~ /\s?\d+\s?/) {   # positive numeric constant
			$offset = $rhs;
			$accumFlag = 0;

		    } else {    # might be a location ID, might be a literal type
			($rhsLoc, $rhsVar) = getLocVar ($rhs, "*", undef);
			if (!defined ($loc{$rhsLoc})) {   # is this an actual location ID? if not, it must be a literal type
			    ($rhsLoc, $rhsVar) = (undef, $lhsVar);   # set rhsVar=lhsVar to avoid errors when lhsVar="*"
			    $offset = $rhs;   # have to turn this literal type into an index later
			}
			$accumFlag = 0;
		    }

		    if ((defined($lhsVar) && $lhsVar eq "*") xor (defined($rhsVar) && $rhsVar eq "*")) {
			die "If one side of a 'do' expression involves the whole state, then both sides must.\nOffending line:\n$_\n";
		    }

		    if (defined($lhsVar) && ($lhsVar eq "type" || $lhsVar eq "*")) {
			$loctype{$lhsLoc} = (defined($rhsLoc) && ($rhsVar eq "type" || $rhsVar eq "*")) ? $loctype{$rhsLoc} : undef;
		    }

		    if (defined($lhsVar) && $lhsVar ne "type" && $lhsVar ne "*" && !defined($loctype{$lhsLoc})) {
			die "Can't access $lhsLoc.$lhsVar because type of $lhsLoc is not bound\n";
		    }

		    if (defined($rhsVar) && $rhsVar ne "type" && $rhsVar ne "*" && !defined($loctype{$rhsLoc})) {
			die "Can't access $rhsLoc.$rhsVar because type of $rhsLoc is not bound\n";
		    }

		    push @{$op{$type}->[$nRule]}, [defined($rhsLoc) ? (@{$loc{$rhsLoc}}, $loctype{$rhsLoc}) : (undef,undef,undef), $rhsVar,
						   @{$loc{$lhsLoc}}, $loctype{$lhsLoc}, $lhsVar,
						   $accumFlag, $offset, parseTags($fail)];

		    if ($debug) {
			$rhsLoc = "" unless defined $rhsLoc;
			$rhsVar = "" unless defined $rhsVar;
			warn "lhsLoc=$lhsLoc lhsVar=$lhsVar rhsLoc=$rhsLoc rhsVar=$rhsVar accumFlag=$accumFlag offset=$offset fail=$fail";
		    }

		} elsif (/^<.*>/) {
		    # misc tags (rate, overload, ...)
		    parseTags ($_, \@tag);

		} elsif (/\S/) {
		    # unrecognized line within rule block
		    die "Syntax error: $_\n";
		}
	    }

	    # end of rule block
	    $ruleTags{$type}->[$nRule] = \@tag;
	    ++$nRule;

	} elsif (/^<.*>/) {
	    parseTags ($_, $ptags{$type});

	} elsif (/\S/) {
	    # unrecognized line
	    die "Syntax error: $_\n";

	}
    } elsif (/\S/) {
	# unrecognized line & no type defined
	die "Syntax error: $_\n";
    }
}

# generate XML
my @gram;
for my $typeindex (1 .. @type - 1) {   # skip the empty type
    my $type = $type[$typeindex];
    my @particle = ("name" => $type,
		    "type" => $typeindex{$type},
		    @{$ptags{$type}}
	);

    # color rules
    my $colBase = 0;
    my @maskshiftmul;

    for my $cvm (@{$hue{$type}}) {
	$colBase += $cvm->[0] << 16;
	push @maskshiftmul, [getMask($type,$cvm->[1]), getShift($type,$cvm->[1]), defined($cvm->[2]) ? ($cvm->[2] << 16) : 0];
    }

    for my $cvm (@{$sat{$type}}) {
	$colBase += $cvm->[0] << 8;
	push @maskshiftmul, [getMask($type,$cvm->[1]), getShift($type,$cvm->[1]), defined($cvm->[2]) ? ($cvm->[2] << 8) : 0];
    }

    for my $cvm (@{$bri{$type}}) {
	$colBase += $cvm->[0];
	push @maskshiftmul, [getMask($type,$cvm->[1]), getShift($type,$cvm->[1]), defined($cvm->[2]) ? $cvm->[2] : 0];
    }

    if (@maskshiftmul == 0) {
	@maskshiftmul = [0, 0, 0];
    }

    my $doneConst = 0;
    for (my $n = 0; $n < @maskshiftmul; ++$n) {
	my ($mask, $shift, $mul) = @{$maskshiftmul[$n]};
	next if @maskshiftmul > 0 && $mask eq "0" && $shift==0 && $mul==0;
	my @colorRule = ("hexmask" => $mask,
			 "rshift" => $shift,
			 "hexmul" => hexv($mul));
	if (!$doneConst) { push @colorRule, ("hexinc" => hexv($colBase)); $doneConst = 1 }
	push @particle, "color" => \@colorRule;
    }

    # production rules
    for (my $nRule = 0; $nRule < @{$ruleTags{$type}}; ++$nRule) {
	my @rule = @{$ruleTags{$type}->[$nRule]};

	# tests
	for my $test (@{$test{$type}->[$nRule]}) {
	    my ($tx, $ty, $ttype, $tvar, $top, $trhs, $other) = @$test;

	    # resolve any dangling literal typenames
	    if ($tvar eq "type") {
		$trhs = getType($trhs);
	    }

	    # build the test hash
	    my @t = (@$other,
		     '@op' => $top,
		     length("$tx$ty") ? ("loc" => [ "x" => $tx, "y" => $ty ]) : (),
		     "mask" => getMask($ttype,$tvar),
		     "hexval" => hexv($trhs << getShift($ttype,$tvar)));

	    # store
	    push @rule, "test" => \@t;
	}

	# operations
	for my $op (@{$op{$type}->[$nRule]}) {
	    my ($sx, $sy, $stype, $svar, $dx, $dy, $dtype, $dvar, $accumFlag, $offset, $other) = @$op;

	    # resolve any dangling literal typenames
	    if ($dvar eq "type" || $dvar eq "*") {
		$offset = getType($offset);
		if ($dvar eq "*") {
		    $offset = $offset << 48;
		}
	    }

	    # build the exec hash
	    my @x = (@$other,
		     defined($sx) && length("$sx$sy") ? ("src" => [ "x" => $sx, "y" => $sy ]) : (),
		     "srcmask" => ($accumFlag ? getMask($stype,$svar) : 0),
		     $accumFlag ? ("rshift" => getShift($stype,$svar)) : (),
		     $offset != 0 ? ("hexinc" => hexv($offset)) : (),
		     ($offset != 0 || $accumFlag) ? ("lshift" => getShift($dtype,$dvar)) : (),
		     "destmask" => getMask($dtype,$dvar),
		     defined($dx) && length("$dx$dy") ? ("dest" => [ "x" => $dx, "y" => $dy ]) : ());

	    # store
	    push @rule, "exec" => \@x;
	}

	# save rule
	if (@rule) {
	    push @particle, "rule" => \@rule;
	}
    }

    # .....aaaaand, save the particle.
    push @gram, "particle" => \@particle;
}

my $elt = newElt( "grammar" => \@gram );
my $twig = XML::Twig->new(pretty_print => 'indented');
$twig->set_root($elt);

$twig->print;

exit;

sub getType {
    my ($type) = @_;
    if ($type =~ /^\-?\d+$/) {
	return $type;
    }
    die "Type '$type' unknown" unless defined $typeindex{$type};
    return $typeindex{$type};
}

sub getMask {
    my ($type, $var) = @_;
    return 0 unless defined($var);
    return "ffffffffffffffff" if $var eq "*";
    return "ffff000000000000" if $var eq "type";
    die "Undefined type" unless defined($type);
    die "Type '$type' unknown" unless defined $typeindex{$type};
    die "Var '$var' unknown for type '$type'" unless defined $pvbits{$type}->{$var};
    my $mask = ((1 << ($pvbits{$type}->{$var} + 1)) - 1) << $pvoffset{$type}->{$var};
    return hexv($mask);
}

sub getShift {
    my ($type, $var) = @_;
    return 0 unless defined($var);
    return 48 if $var eq "type";
    return 0 if $var eq "*";
    die "Undefined type" unless defined($type);
    die "Type '$type' unknown" unless defined $typeindex{$type};
    die "Var '$var' unknown for type '$type'" unless defined $pvbits{$type}->{$var};
    return $pvoffset{$type}->{$var};
}

sub parseTags {
    my ($line, $tagRef) = @_;
    $tagRef = [] unless defined $tagRef;
    my %tagHash = @$tagRef;
    while ($line =~ /<\s?(\S+)\s?([^>]*|\"[^\"]*\")\s?>/g) {
	my ($tag, $val) = ($1, $2);
	die "Duplicate tag $tag\n" if exists $tagHash{$tag};
	if ($val =~ /^=(.*)/) {
	    $val = eval($1);
	}
	push @$tagRef, ($tag => $val);
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
    my @constvarmul = ([$const, @varmul==0 ? (undef,undef) : @{shift @varmul}], map ([0,@$_], @varmul));
    return \@constvarmul;
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

sub hexv {
    my ($val) = @_;
    my $hex = sprintf ("%x", $val);
    return sprintf ("%x", $val);
}

sub newElt {
    my $gi   = shift;
    my $data = shift;

    my $t = XML::Twig::Elt->new($gi);

    my @child;
    if (ref($data) eq "HASH") {
	@child = sortHash ($data);
    } elsif (ref($data) eq "ARRAY") {
	@child = @$data;
    }

    if (@child) {
	while (@child) {
	    my $k = shift @child;
	    my $v = shift @child;
	    if ($k =~ s/^\@//) {
		$t->set_att ($k => $v);
	    } else {
		newElt($k, $v)->paste(last_child => $t);
	    }
	}
    } else {
        $t->set_text($data);
    }

    return $t;
}

sub sortHash {
    my ($hashRef) = @_;
    return map (($_ => $hashRef->{$_}), sort keys %$hashRef);
}

__END__

=head1 NAME
zoocompiler.pl - compile PixelZoo .zg files to XML grammars

=head1 SYNOPSIS

zoocompiler.pl [options] <.zg file>

 Options:
  -help               brief help message
  -man                full documentation
  -preprocessor,-cpp  preprocessor to use

=head1 OPTIONS

=over 8

=item B<-help>

Prints a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=item B<-preprocessor>

Specify the preprocessor to use, plus options.

Default is "gcc -x c -E".

=back

=head1 DESCRIPTION

B<This program> will compile a PixelZoo .zg file into an XML grammar.

=cut
