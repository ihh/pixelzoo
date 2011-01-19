#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use XML::Twig;
use Carp;

# parse options
my $man = 0;
my $help = 0;
my $debug = 0;
my $verbose = 0;
my $ppfile;

my $cpp = "gcc -x c -E";

GetOptions('help|?' => \$help, man => \$man, verbose => \$verbose, debug => \$debug, 'preprocessor|cpp=s' => \$cpp, 'savepp=s' => \$ppfile) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

pod2usage(2) unless @ARGV == 1;

$verbose = 1 if $debug;

# game data structures
my @type;
my %typeindex;  # $typeindex{$type}
my %typesize;  # number of actual Particle's represented by this "type"
my %typemask;  # mask for recognizing this type
my %pvar;    # $pvar{$type} = [$var1,$var2,$var3,...]
my %pvbits;   # $pvbits{$type}->{$var}
my %pvoffset;   # $pvoffset{$type}->{$var}
my %ptags;     # $ptags{$type} = [ ... ]
my (%hue, %sat, %bri);   # $hue{$type} = [[$const1,$var1,$mul1], [$const2,$var2,$mul2], ...]   etc
my %ruleTags;  # $ruleTags{$type}->[$ruleIndex] = [ "rate" => $rate, "overload" => $overload, ... ]
my %test;  # $test{$type}->[$ruleIndex]->[$testIndex] = [$x,$y,$type,$var,$opcode,$rhs,\%otherTags]
my %op;  # $op{$type}->[$ruleIndex]->[$opIndex] = [$xSrc,$ySrc,$typeSrc,$varSrc,$xDest,$yDest,$typeDest,$varDest,$accumFlag,$offset,\%otherTags]
my @tool;  # $tool[$n] = [$name, $size, $type, $reserve, $recharge, $sprayRate, \@overwriteType]
my @init;  # $init[$n] = [$x, $y, $type]

# empty type
my $emptyType = "empty";
push @type, $emptyType;
$typesize{$emptyType} = 1;
$typemask{$emptyType} = 0xffff;
$pvar{$emptyType} = [qw(hue saturation value)];
$pvbits{$emptyType} = { 'hue' => 6, 'saturation' => 3, 'value' => 3 };
$pvoffset{$emptyType} = { 'hue' => 6, 'saturation' => 3, 'value' => 0 };
$ptags{$emptyType} = [];
$hue{$emptyType} = $sat{$emptyType} = $bri{$emptyType} = [];
$ruleTags{$emptyType} = $test{$emptyType} = $op{$emptyType} = [];

# other game data
my $boardRate = 240;
my $boardSize = 128;
my %entrancePort = ("x" => 0, "y" => 0, "count" => 0, "rate" => 1, "width" => 1, "height" => 1);
my %exitPort = ("pos" => { "x" => 0, "y" => 0 }, "count" => 0, "radius" => 6);
my ($entranceType, $exitType) = ($emptyType, $emptyType);
my @gameXML;

# compiler data
my %compiler_warnings;

# parse input file
my ($zgfilename) = @ARGV;
local *ZG;
open ZG, "$cpp $zgfilename |" or die "Couldn't open $zgfilename: $!";
my @zg = <ZG>;
close ZG;

# savepp
if (defined $ppfile) {
    local *PP;
    open PP, ">$ppfile" or die "Couldn't open $ppfile: $!";
    print PP @zg;
    close PP or die "Couldn't close $ppfile: $!";
}

# strip off irrelevant crud
grep (s/(\/\/|#).*$//, @zg);  # trim C++-style comments, preprocessor directives
@zg = map ( (split(/;/,$_)), @zg );   # split lines on semicolons
grep (s/^\s*(.*?)\s*$/$1/, @zg);  # trim whitespace at beginning/end of line
grep (s/\s+/ /, @zg);  # squash multiple whitespace

# loop through file
my ($type, $nRule);
local $_;
while (@zg) {
    $_ = shift @zg;
    die unless defined;
    warn "Read line '$_'" if $debug;

    if (/^warn (.*)$/) {
	# compiler warning
	warn "$1\n";

    } elsif (/^eval ?\{(.*)\}$/) {
	# one-line eval block
	my $expr = $1;
	my @val = eval($expr);
	warn "Expression $expr evaluated to @val" if $debug;
	unshift @zg, @val;

    } elsif (/^eval ?\{(.*)$/) {
	# multi-line eval block
	my $expr = $1;
	while (@zg) {
	    $_ = shift @zg;
	    last if /^\}$/;
	    $expr .= $_;
	}
	my @val = eval($expr);
	warn "Expression $expr evaluated to @val" if $debug;
	unshift @zg, @val;

    } elsif (/^xml ?\{(.*)\}$/) {
	# one-line XML block
	my $expr = $1;
	my @val = eval($expr);
	warn "Expression $expr evaluated to @val" if $debug;
	push @gameXML, @val;

    } elsif (/^eval ?\{(.*)$/) {
	# multi-line XML block
	my $expr = $1;
	while (@zg) {
	    $_ = shift @zg;
	    last if /^\}$/;
	    $expr .= $_;
	}
	warn "Evaluating $expr" if $debug;
	my @val = eval($expr);
	warn "Expression $expr evaluated to @val" if $debug;
	push @gameXML, @val;

    } elsif (/^size (\d+)/) {
	$boardSize = $1;

    } elsif (/^init ?\( ?(\d+) ?, ?(\d+) ?\) (\S+)/) {
	push @init, [$1, $2, $3];

    } elsif (/^tool ("[^"]*"|\S+)/) {
	my $name = $1;
	$name =~ s/^"(.*)"$/$1/;
	my ($size, $type, $reserve, $recharge, $spray) = (1, $emptyType, 100, 100, 1);
	my @overwrite;
	if (/\( ?size (\S+) ?\)/) { $size = $1 }
	if (/\( ?type (\S+) ?\)/) { $type = $1 }
	if (/\( ?reserve (\S+) ?\)/) { $reserve = $1 }
	if (/\( ?recharge (\S+) ?\)/) { $recharge = $1 }
	if (/\( ?spray (\S+) ?\)/) { $spray = $1 }
	while (/\( ?overwrite (\S+) ?\)/g) { push @overwrite, $1 }

	push @tool, [$name, $size, $type, $reserve, $recharge, $spray, \@overwrite];

    } elsif (/^entrance ?\( ?(\d+) ?, ?(\d+) ?\)/) {
	$entrancePort{'x'} = $1;
	$entrancePort{'y'} = $2;
	if (/\( ?type (\S+) ?\)/) { $entranceType = $1 }
	while (/\( ?(count|rate|width|height) (\S+) ?\)/g) { $entrancePort{$1} = $2 }

    } elsif (/^exit ?\( ?(\d+) ?, ?(\d+) ?\)/) {
	$exitPort{'pos'}->{'x'} = $1;
	$exitPort{'pos'}->{'y'} = $2;
	if (/\( ?type (\S+) ?\)/) { $exitType = $1 }
	while (/\( ?(count|radius) (\S+) ?\)/g) { $exitPort{$1} = $2 }

    } elsif (/^type ("[^"]*"|\S+)(.*)$/) {
	my $varstr;
	($type, $varstr) = ($1, $2);
	$type =~ s/^"(.*)"$/$1/;
	die "Type '$type' is already defined" if exists $typemask{$type};
	push @type, $type;
	$nRule = 0;
	my ($offset, $typeoffset) = (0, 0);  # offset of next var in varbits & typebits
	while ($varstr =~ /([A-Za-z_][A-Za-z_\d]*\!?) ?\( ?(\d+) ?\)/g) {
	    my ($varname, $varbits) = ($1, $2);
	    my $intype = $varname =~ s/\!$//;
	    die "You cannot use 'type' as a var name for $type" if $varname eq "type";
	    push @{$pvar{$type}}, $varname;
	    $pvbits{$type}->{$varname} = $varbits;
	    if ($intype) {
		$pvoffset{$type}->{$varname} = $typeoffset + 48;
		$typeoffset += $varbits;
		die "More than 15 bits of type-encoded vars for type $type" if $typeoffset > 15;
	    } else {
		$pvoffset{$type}->{$varname} = $offset;
		$offset += $varbits;
		die "More than 48 bits of vars for type $type" if $offset > 48;
	    }
	}
	$typesize{$type} = 1 << $typeoffset;
	$typemask{$type} = ((1 << (16 - $typeoffset)) - 1) << $typeoffset;
	$ptags{$type} = [];
	$ruleTags{$type} = [];

    } elsif (defined $type) {

	if (/^(type|size|init|tool|entrance|exit)\b/) {
	    # end of type block
	    unshift @zg, $_;
	    last;

	} elsif (/^hue ?= ?(.*)$/) {
	    # hue
	    $hue{$type} = parseColor($1);

	} elsif (/^sat ?= ?(.*)$/) {
	    # saturation
	    $sat{$type} = parseColor($1);

	} elsif (/^bri ?= ?(.*)$/) {
	    # brightness
	    $bri{$type} = parseColor($1);

	} elsif (/^\{/) {
	    # start rule block
	    s/\{ ?//;
	    unshift @zg, $_;

	    # rule block
	    my (@tag, %loc, %loctype, %locdubious, @test, @op);
	    my $temps = 0;
	    while (@zg) {
		$_ = shift @zg;
		die unless defined;
		warn "Read line '$_'" if $debug;

		if (/^\}/) {
		    # end rule block
		    s/\} ?//;
		    unshift @zg, $_;
		    last;
		}

		if (/^loc ([A-Za-z_][A-Za-z_\d]*) ?\( ?([\+\-\d]+) ?[ ,] ?([\+\-\d]+) ?\)$/) {
		    # loc
		    my ($locid, $x, $y) = ($1, $2, $3);
		    die "Duplicate loc" if defined $loc{$locid};
		    $loc{$locid} = [$x,$y];
		    if ($x == 0 && $y == 0) {
			$loctype{$locid} = $type;
			$locdubious{$locid} = undef;
		    }
		    warn "loc=$locid x=$x y=$y" if $debug;

		} elsif (/^temp ([A-Za-z_][A-Za-z_\d]*)$/) {
		    # temp
		    my ($locid) = ($1);
		    die "Duplicate loc" if defined $loc{$locid};
		    $loc{$locid} = [-128,$temps++];
		    warn "temp=$locid" if $debug;

		} elsif (/^if ([A-Za-z_\d\. ]+) ?(==|=|\!=|>=|>|<=|<) ?([^ \(]+) ?(.*)$/) {
		    # test
		    my ($lhs, $op, $rhs, $ignore) = ($1, $2, $3, $4);
# xmlboard.c parser doesn't distinguish between "=" and "==", but Perl's eval does, so...
		    $op = "==" if $op eq "=";

		    my ($loc, $var) = getLocVar ($lhs, "type", \%loc);
		    my $ignoreTags = parseTags($ignore);

		    if ($var eq "type") {
			$loctype{$loc} = $rhs;
			$locdubious{$loc} = firstNonzeroTag ($ignoreTags, "ignore", "overload");
		    }

		    if (defined($var) && $var ne "type" && $var ne "*") {
			assertLocTypeBound (\%loctype, \%locdubious, $loc, $var, "In type $type, rule \%d: ", $nRule);
		    }

		    push @{$test{$type}->[$nRule]}, [@{$loc{$loc}}, $loctype{$loc}, $var, $op, $rhs, $ignoreTags];

		    if ($debug) {
			warn "loc=$loc var=$var op='$op' rhs=$rhs ignore=$ignore";
		    }

		} elsif (/^do ([A-Za-z_\d\. ]+) ?= ?([^<]+)(.*)$/) {
		    # exec
		    my ($lhs, $rhs, $fail) = ($1, $2, $3);
		    my ($lhsLoc, $lhsVar) = getLocVar ($lhs, "*", \%loc);
		    my $failTags = parseTags($fail);
		    my ($rhsLoc, $rhsVar, $accumFlag);
		    my $offset = 0;
		    $rhs =~ s/ $//;
		    if ($rhs =~ /([^\+]*)\+ ?([\+\-]?\d+)/) {   # something + numeric constant
			$offset = $2;
			($rhsLoc, $rhsVar) = getLocVar ($1, undef, \%loc);
			$accumFlag = 1;

		    } elsif ($rhs =~ /([^\-]*)\- ?([\+\-]?\d+)/) {   # something - numeric constant
			$offset = -$2;
			($rhsLoc, $rhsVar) = getLocVar ($1, undef, \%loc);
			$accumFlag = 1;

		    } elsif ($rhs =~ / ?\d+ ?/) {   # positive numeric constant
			$offset = $rhs;
			$accumFlag = 0;

		    } else {    # might be a location ID, might be a literal type
			($rhsLoc, $rhsVar) = getLocVar ($rhs, "*", undef);
			if (!defined ($loc{$rhsLoc})) {   # is this an actual location ID? if not, it must be a literal type
			    ($rhsLoc, $rhsVar) = (undef, $lhsVar);   # set rhsVar=lhsVar to avoid errors when lhsVar="*"
			    $offset = $rhs;   # have to turn this literal type into an index later
			    $accumFlag = 0;
			} else {
			    $accumFlag = 1;
			}
		    }

		    if ((defined($lhsVar) && $lhsVar eq "*") xor (defined($rhsVar) && $rhsVar eq "*")) {
			die "If one side of a 'do' expression involves the whole state, then both sides must.\nOffending line:\n$_\n";
		    }

		    if (defined($lhsVar) && ($lhsVar eq "type" || $lhsVar eq "*")) {
			my $rhsType = (defined($rhsLoc) && ($rhsVar eq "type" || $rhsVar eq "*")) ? $loctype{$rhsLoc} : $offset;
			$loctype{$lhsLoc} = $rhsType;
			$locdubious{$lhsLoc} = firstNonzeroTag ($failTags, "fail", "overload");
		    }

		    if (defined($lhsVar) && $lhsVar ne "type" && $lhsVar ne "*") {
			assertLocTypeBound (\%loctype, \%locdubious, $lhsLoc, $lhsVar, "In type $type, rule \%d: ", $nRule);
		    }

		    if (defined($rhsVar) && $rhsVar ne "type" && $rhsVar ne "*") {
			assertLocTypeBound (\%loctype, \%locdubious, $rhsLoc, $rhsVar, "In type $type, rule \%d: ", $nRule);
		    }

		    push @{$op{$type}->[$nRule]}, [defined($rhsLoc) ? (@{$loc{$rhsLoc}}, $loctype{$rhsLoc}) : (undef,undef,undef), $rhsVar,
						   @{$loc{$lhsLoc}}, $loctype{$lhsLoc}, $lhsVar,
						   $accumFlag, $offset, $failTags];

		    if ($debug) {
			$rhsLoc = "" unless defined $rhsLoc;
			$rhsVar = "" unless defined $rhsVar;
			warn "lhsLoc=$lhsLoc lhsVar=$lhsVar rhsLoc=$rhsLoc rhsVar=$rhsVar accumFlag=$accumFlag offset=$offset fail=$fail";
		    }

		} elsif (/^text ("[^"+]"|\S+)/) {
		    my $text = $1;
		    $text =~ s/^"(.*)"$/$1/;
		    my %balloon = ("text" => $text);
		    if (/\((\d+) ?, ?(\d+)\)/) { $balloon{"pos"} = ["x" => $1, "y" => $2] }
		    parseTags ($_, \%balloon);
		    if (exists $balloon{"hue"}) {
			$balloon{"hexcolor"} = hexv(0xffff | ($balloon{"hue"} << 16));
			delete $balloon{"hue"};
		    }
		    push @tag, "goal" => ['@type' => "and",  # "and"ing result with 0 means this goal will persist
					  "lazy" => "",
					  "goal" => ['@type' => "maybe",
						     "prob" => $balloon{"rate"}],
					  "goal" => ['@type' => "balloon",
						     "balloon" => \%balloon],
					  "goal" => ['@type' => "false"]];

		} elsif (/^<.*>/) {
		    # misc tags (rate, overload, ...)
		    parseTags ($_, \@tag);

		} elsif (/^warn (.*)$/) {
		    # compiler warning
		    warn "$1\n";

		} elsif (/warn/) {
		    die $_;

		} elsif (/\S/) {
		    # unrecognized line within rule block
		    die "Syntax error in rule block (type $type): '$_'\n";
		}
	    }

	    # end of rule block
	    $ruleTags{$type}->[$nRule] = \@tag;
	    ++$nRule;

	} elsif (/^<.*>/) {
	    parseTags ($_, $ptags{$type});

	} elsif (/\S/) {
	    # unrecognized line
	    die "Syntax error in file (type $type): '$_'\n";

	}
    } elsif (/\S/) {
	# unrecognized line & no type defined
	die "Syntax error in file (no type defined): '$_'\n";
    }
}

# assign type indices (TODO: optimize packing)
my $idx = 0;
for my $type (@type) {
    my $typeEncodedVarMask = 0xffff ^ $typemask{$type};
    while ($idx & $typeEncodedVarMask) { ++$idx }
    $typeindex{$type} = $idx;
    $idx += $typesize{$type};
}
die "Too many types - maybe implement optimized packing?" if $idx > 0x10000;

# generate XML
my @gram;
for my $type (@type) {
    warn "Generating XML for base type '$type'\n" if $verbose;

    my $baseindex = $typeindex{$type};
    for my $typeindex ($baseindex .. $baseindex + $typesize{$type} - 1) {

	my $state = $typeindex << 48;
	my $name = $type;
	for my $var (@{$pvar{$type}}) {
	    my $shift = getShift($type,$var);
	    if ($shift >= 48) {
		my $val = ($state & decv(getMask($type,$var))) >> $shift;
		$name .= " $var:$val";
	    }
	}

	my @particle = ("name" => $name,
			"type" => $typeindex,
			@{$ptags{$type}}
	    );

	# color rules
	my $colBase = 0;
	my @maskshiftmul;

	for my $cvm (@{$hue{$type}}) {
	    $colBase += $cvm->[0] << 16;
	    push @maskshiftmul, [getMask($type,$cvm->[1]), getShift($type,$cvm->[1]), defined($cvm->[2]) ? ($cvm->[2] << 16) : 0];
	}
	# default hue = 0

	for my $cvm (@{$sat{$type}}) {
	    $colBase += $cvm->[0] << 8;
	    push @maskshiftmul, [getMask($type,$cvm->[1]), getShift($type,$cvm->[1]), defined($cvm->[2]) ? ($cvm->[2] << 8) : 0];
	}
	$colBase += 0x8000 unless @{$sat{$type}};  # default saturation = 128

	for my $cvm (@{$bri{$type}}) {
	    $colBase += $cvm->[0];
	    push @maskshiftmul, [getMask($type,$cvm->[1]), getShift($type,$cvm->[1]), defined($cvm->[2]) ? $cvm->[2] : 0];
	}
	$colBase += 0xff unless @{$bri{$type}};  # default brightness = 255

	if (@maskshiftmul == 0) {
	    @maskshiftmul = [0, 0, 0];   # add a dummy entry to ensure $colBase gets a look in
	}

	my $doneConst = 0;
	for (my $n = 0; $n < @maskshiftmul; ++$n) {
	    my ($mask, $shift, $mul) = @{$maskshiftmul[$n]};
	    next if $doneConst && $mask eq "0" && $shift==0 && $mul==0;
	    my @colorRule = ("mask" => $mask,
			     "rshift" => $shift,
			     "hexmul" => hexv($mul));
	    if (!$doneConst) { push @colorRule, ("hexinc" => hexv($colBase)); $doneConst = 1 }
	    push @particle, "color" => \@colorRule;
	}

	# production rules
      RULE:
	for (my $nRule = 0; $nRule < @{$ruleTags{$type}}; ++$nRule) {
	    my @rule = @{$ruleTags{$type}->[$nRule]};

	    # tests
	    my %compare;
	  TEST:
	    for my $test (@{$test{$type}->[$nRule]}) {
		my ($tx, $ty, $ttype, $tvar, $top, $trhs, $other) = @$test;

		# intercept tests of type-encoded vars
		if ($tx == 0 && $ty == 0 && getShift($ttype,$tvar) >= 48) {
		    my $tlhs = (($state & decv(getMask($ttype,$tvar))) >> getShift($ttype,$tvar));
		    my $expr = "$tlhs $top $trhs";
		    warn "Evaluating test ($tvar $top $trhs) for var-encoded type ($name), test expression is ($expr)" if $debug;
		    if (eval ($expr)) {
			warn "Test passed; skipping to next test" if $debug;
			next TEST;
		    } else {
			warn "Test failed; skipping to next rule" if $debug;
			next RULE;
		    }
		}

		# resolve any dangling literal typenames
		if ($tvar eq "type") {
		    $trhs = getType($trhs);
		}

		# consolidate repeated tests of different fields at the same location
		if ($top eq "=" || $top eq "==") {
		    if (exists $compare{"$tx $ty"}) {
			my $ruleRef = $rule[$compare{"$tx $ty"}];
			my %prevTest = @$ruleRef;
			my $prevMask = $prevTest{"mask"};
			my $thisMask = getMask($ttype,$tvar);
			if (defined($prevMask) && defined($thisMask)) {
			    if ((decv($thisMask) & decv($prevMask)) == 0) {
				my $prevRhs = exists($prevTest{"hexval"}) ? decv($prevTest{"hexval"}) : 0;
				my $thisRhs = $trhs << getShift($ttype,$tvar);
				$prevTest{"hexval"} = hexv ($prevRhs | $thisRhs);
				$prevTest{"mask"} = hexv (decv($prevMask) | decv($thisMask));
				@$ruleRef = %prevTest;
				warn "Consolidated rule test: prevMask=$prevMask prevRhs=$prevRhs thisMask=$thisMask thisRhs=$thisRhs ", map(" $_=>$prevTest{$_}",keys %prevTest) if $debug;
				next;
			    } else {
				compiler_warn ("Couldn't consolidate repeated tests of ($tx,$ty) in type $type, rule \%d, as masks $prevMask and $thisMask overlap", $nRule);
			    }
			}
		    }

		    unless (defined (firstNonzeroTag ($other, "ignore", "overload"))) {
			$compare{"$tx $ty"} = @rule + 1;   # this should pick out the "test" child. very hacky
		    }
		}

		# build the test hash
		my @t = (@$other,
			 '@op' => $top,
			 length("$tx$ty") ? ("pos" => [ "x" => $tx, "y" => $ty ]) : (),
			 "mask" => getMask($ttype,$tvar),
			 "hexval" => hexv($trhs << getShift($ttype,$tvar)));

		# store
		push @rule, "test" => \@t;
	    }

	    # operations
	    my (%assign, %locvarsunset);
	    for my $op (@{$op{$type}->[$nRule]}) {
		my ($sx, $sy, $stype, $svar, $dx, $dy, $dtype, $dvar, $accumFlag, $offset, $other) = @$op;

		# flag potentially uninitialized vars
		my $locid = "($dx,$dy)";
		if ($dvar eq "type") {
		    $locvarsunset{$locid} = { map (($_ => 1), @{$pvar{$offset}}) };
		} elsif ($dvar eq "*") {
		    $locvarsunset{$locid} = {};
		} else {
		    delete $locvarsunset{$locid}->{$dvar};
		}

		# resolve any dangling literal typenames
		if ($dvar eq "type" || $dvar eq "*") {
		    $offset = getType($offset);
		    if ($dvar eq "*") {
			$offset = $offset << 48;
		    }
		}

		# consolidate repeated writes of a constant to the same location
		if (!$accumFlag) {
		    if (exists $assign{"$dx $dy"}) {
			my $ruleRef = $rule[$assign{"$dx $dy"}];
			my %prevExec = @$ruleRef;
			my $prevDestmask = $prevExec{"destmask"};
			my $thisDestmask = getMask($dtype,$dvar);
			if (defined($prevDestmask) && defined($thisDestmask)) {
			    if ((decv($thisDestmask) & decv($prevDestmask)) == 0) {
				my $prevLeftShift = $prevExec{"lshift"};
				$prevLeftShift = 0 unless defined $prevLeftShift;
				my $prevOffset = (exists($prevExec{"hexinc"}) ? decv($prevExec{"hexinc"}) : 0) << $prevLeftShift;
				my $thisLeftShift = getShift($dtype,$dvar);
				$prevExec{"hexinc"} = hexv ($prevOffset | ($offset << $thisLeftShift));
				$prevExec{"lshift"} = 0;
				$prevExec{"destmask"} = hexv (decv($prevDestmask) | decv($thisDestmask));
				@$ruleRef = %prevExec;
				warn "Consolidated rule operation: prevDestmask=$prevDestmask prevLeftShift=$prevLeftShift prevOffset=$prevOffset thisDestmask=$thisDestmask thisLeftShift=$thisLeftShift thisOffset=$offset ", map(" $_=>$prevExec{$_}",keys %prevExec) if $debug;
				next;
			    } else {
				compiler_warn ("Couldn't consolidate repeated writes to ($dx,$dy) in type $type, rule \%d, as masks $prevDestmask and $thisDestmask overlap", $nRule);
			    }
			}
		    }

		    unless (defined (firstNonzeroTag ($other, "fail", "overload"))) {
			$assign{"$dx $dy"} = @rule + 1;   # this should pick out the "exec" child. very hacky
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
		push @rule, "exec" => \@x;  # %assign requires that the second element is the arrayref containing the rule op... yuck
	    }

	    # check for unset vars
	    if (grep (%$_, values %locvarsunset)) {
		compiler_warn ("Type $type, rule \%d: the following location(s) had their type set, but var(s) left uninitialized: "
			       . join(" ", map(%{$locvarsunset{$_}} ? ("$_\[@{[keys %{$locvarsunset{$_}}]}\]") : (), keys %locvarsunset)), $nRule);
	    }

	    # save rule
	    if (@rule) {
		push @particle, "rule" => \@rule;
	    }
	}

	# .....aaaaand, save the particle.
	push @gram, "particle" => \@particle;
    }
}

my @toolxml;
for my $tool (@tool) {
    my ($name, $size, $type, $reserve, $recharge, $spray, $overwriteType) = @$tool;
    push @toolxml, ("tool" => ["name" => $name,
			       "size" => $size,
			       "spray" => $spray,
			       "hexstate" => getTypeAsHexState($type),
			       "reserve" => $reserve,
			       "recharge" => $recharge,
			       @$overwriteType ? ("overwrite" => [map (("state" => getTypeAsHexState($_)), @$overwriteType)]) : ()]);
}

$entrancePort{"hexstate"} = getTypeAsHexState($entranceType);
$exitPort{"type"} = getType($exitType);

my (@exitLoc, @exitColor);
my $exitRadius = $exitPort{"radius"};
my $exitx = $exitPort{'pos'}->{"x"};
my $exity = $exitPort{'pos'}->{"y"};
for (my $x = -$exitRadius; $x <= $exitRadius; ++$x) {
    for (my $y = -$exitRadius; $y <= $exitRadius; ++$y) {
	my $r = sqrt($x*$x+$y*$y);
	if ($r < $exitRadius) {
	    my $col = 0x007;  # white
	    if ($r < $exitRadius/3 || $r > $exitRadius*2/3) {
		$col = 0x03f;  # red
	    }
	    my $ex = $exitx + $x;
	    my $ey = $exity + $y;
	    push @exitLoc, "pos" => ["x" => $ex, "y" => $ey] if $ex!=$exitx || $ey!=$exity;  # don't count the centre twice
	    push @exitColor, "init" => ["x" => $ex, "y" => $ey, "hexval" => hexv($col)];
	}
    }
}

my @entranceLoc;
my $entranceWidth = $entrancePort{"width"};
my $entranceHeight = $entrancePort{"height"};
for (my $w = 0; $w < $entranceWidth; ++$w) {
    for (my $h = 0; $h < $entranceHeight; ++$h) {
	push @entranceLoc, "pos" => ["x" => $w, "y" => $h];
    }
}

my @game = (@gameXML,
	    "goal" => ['@type' => "and",
		       "lazy" => "",
		       "cached" => "",

# place entrance and exit balloons
		       "goal" => ['@type' => "area",
				  "pos" => ["x" => $entrancePort{"x"}, "y" => $entrancePort{"y"}],
				  "goal" => ['@type' => "balloon",
					     "balloon" => ["text" => "ENTRANCE",
							   "persist" => '']]],

		       "goal" => ['@type' => "area",
				  "pos" => $exitPort{'pos'},
				  "goal" => ['@type' => "balloon",
					     "balloon" => ["text" => "EXIT (closed)",
							   "persist" => '']]],

# print hello message
		       "goal" => ['@type' => "print",
				  "text" => "Welcome to level 1!\n" .
				  "Guide " . $exitPort{'count'} . " guests from the entrance to the exit.\n" .
				  "The exit will open when all " . $entrancePort{'count'} . " guests have entered."],


# introduce the guests
		       "goal" => ['@type' => "area",
				  "pos" => ["x" => $entrancePort{"x"}, "y" => $entrancePort{"y"}],
				  "goal" => ['@type' => "spray",
					     "tool" => ["name" => "entrance",
							"size" => minPowerOfTwo (max ($entranceWidth, $entranceHeight)),
							"brush" => ["center" => ["x" => int($entranceWidth/2), "y" => int($entranceHeight/2)],
								    "intensity" => \@entranceLoc],
							"spray" => 1,
							"hexstate" => getTypeAsHexState($entranceType),
							"reserve" => $entrancePort{'count'},
							"recharge" => 0]]],

# delete entrance balloon
		       "goal" => ['@type' => "area",
				  "pos" => ["x" => $entrancePort{"x"}, "y" => $entrancePort{"y"}],
				  "goal" => ['@type' => "balloon"]],

# print status message
		       "goal" => ['@type' => "print",
				  "text" => "All guests have now entered."],


# more goals here... (e.g., require the player to meet the minimum population level)


# open the guest exit (currently the only exit)
		       "goal" => ['@type' => "setexit",
				  "state" => "PortalCounting"],

# delete exit balloon
		       "goal" => ['@type' => "area",
				  "pos" => $exitPort{'pos'},
				  "goal" => ['@type' => "balloon"]],

# place "EXIT (open)" balloon at exit
		       "goal" => ['@type' => "area",
				  "pos" => $exitPort{'pos'},
				  "goal" => ['@type' => "balloon",
					     "balloon" => ["text" => "EXIT (open)",
							   "persist" => '']]],

# print status message
		       "goal" => ['@type' => "print",
				  "text" => "The exit is now open.\n" .
				  "Guide " . $exitPort{'count'} . " guests to the exit."],


# more goals here (e.g. fend off challenges during the guest evacuation)


# wait for player to reach the guest exit count
		       "goal" => ['@type' => "exit",
				  "state" => "PortalCounting",
				  "count" => ["min" => $exitPort{"count"}]],

# delete exit balloon
		       "goal" => ['@type' => "area",
				  "pos" => $exitPort{'pos'},
				  "goal" => ['@type' => "balloon"]],

# place "UNLOCKED" balloon at exit
		       "goal" => ['@type' => "area",
				  "pos" => $exitPort{'pos'},
				  "goal" => ['@type' => "balloon",
					     "balloon" => ["text" => "UNLOCKED!"]]],

# print "UNLOCKED" message
		       "goal" => ['@type' => "print",
				  "text" => "Exit unlocked! You could try the next level, if there was one."],

# "unlock" the guest exit achievment
		       "goal" => ['@type' => "setexit",
				  "state" => "PortalUnlocked"],


# more goals here... (e.g., steal the owner's HQ exit)


# print win message
		       "goal" => ['@type' => "print",
				  "text" => "YOU WIN! Congratulations."],

# set the game state to WIN!!!
		       "goal" => ['@type' => "setgame",
				  "state" => "GameWon"],

# that's all, folks

],
	    @toolxml,
	    "rate" => $boardRate,
	    "entrance" => \%entrancePort,
	    "exit" => [%exitPort, @exitLoc],
	    "board" => ["size" => $boardSize,
			"grammar" => \@gram,
			@exitColor,
			@init > 0
			? map (("init" => ["x" => $$_[0],
					   "y" => $$_[1],
					   "type" => getType($$_[2])]),
			       @init)
			: ()]);

warn "Writing XML\n" if $verbose;
my $elt = newElt("xml" => ["game" => \@game]);
my $twig = XML::Twig->new(pretty_print => 'indented');
$twig->set_root($elt);

$twig->print;

exit;

sub compiler_warn {
    my ($fmt, @args) = @_;
    my $warning = sprintf ($fmt, @args);
    my $nw = ++$compiler_warnings{$fmt};
    if ($nw == 1) { warn "$warning\n" }
    elsif ($nw == 2) { warn "(suppressing further warnings of the form \"", substr($fmt,0,20), "...\")\n" }
}

sub assertLocTypeBound {
    my ($loctype, $locdubious, $loc, $var, $fmt, @args) = @_;
    if (!defined($loctype->{$loc})) {
	die sprintf($fmt,@args) . "Can't access $loc.$var because type of $loc is not bound\n";
    }
    if (defined $locdubious->{$loc}) {
	compiler_warn ("${fmt}Accessing $loc.$var: type of $loc may be dubious due to previous $locdubious->{$loc} clause", @args);
    }
}

sub firstNonzeroTag {
    my ($arrayRef, @tag) = @_;
    my %hash = @$arrayRef;
    for my $tag (@tag) {
	return $tag if exists($hash{$tag}) && $hash{$tag} > 0;
    }
    return undef;
}

sub getTypeAsHexState {
    my ($type) = @_;
    return hexv(getType($type)).("0"x12);
}

sub getType {
    my ($type) = @_;
    if ($type =~ /^\-?\d+$/) {
	return $type;
    }
    croak "Type '$type' unknown" unless defined $typeindex{$type};
    return $typeindex{$type};
}

sub getMask {
    my ($type, $var) = @_;
    return 0 unless defined($var);
    return "ffffffffffffffff" if $var eq "*";
    return hexv($typemask{$type}) . "000000000000" if $var eq "type";
    croak "Undefined type" unless defined($type);
    croak "Type '$type' unknown" unless defined $typeindex{$type};
    croak "Var '$var' unknown for type '$type'" unless defined $pvbits{$type}->{$var};
    my $mask = ((1 << $pvbits{$type}->{$var}) - 1) << $pvoffset{$type}->{$var};
    return hexv($mask);
}

sub getShift {
    my ($type, $var) = @_;
    return 0 unless defined($var);
    return 48 if $var eq "type";
    return 0 if $var eq "*";
    croak "Undefined type" unless defined($type);
    croak "Type '$type' unknown" unless defined $typeindex{$type};
    croak "Var '$var' unknown for type '$type'" unless defined $pvbits{$type}->{$var};
    return $pvoffset{$type}->{$var};
}

sub parseTags {
    my ($line, $tagRef) = @_;
    $tagRef = [] unless defined $tagRef;
    my $isArray = ref($tagRef) eq "ARRAY";
    my %tagHash = $isArray ? @$tagRef : %$tagRef;
    while ($line =~ /<\s?(\S+)\s?([^>]*|\"[^\"]*\")\s?>/g) {
	my ($tag, $val) = ($1, $2);
	croak "Duplicate tag $tag\n" if exists $tagHash{$tag};
	# evaluate parenthesized expressions
	while ($val =~ /(.*)\(([^\)]*)\)(.*)/) {
	    my ($left, $expr, $right) = ($1, $2, $3);
	    warn "Evaluating $expr" if $debug;
	    my @val = eval($expr);
	    warn "In tag $tag: expression $expr evaluated to @val" if $debug;
	    $val = "$left@val$right";
	}
	if ($isArray) {
	    push @$tagRef, ($tag => $val);
	} else {
	    $tagRef->{$tag} = $val;
	}
    }
    return $tagRef;
}

sub parseColor {
    my ($colexpr) = @_;
    $colexpr =~ s/\- ?(\S+) ?\* ?(\d+)/+ $1 * -$2/g;
    $colexpr =~ s/\- ?(\d+) ?\* ?(\S+)/+ -$1 * $2/g;
    $colexpr =~ s/\- ?([^\d]\S*)/+ -1 * $1/g;
    my @term = split (/ ?\+ ?/, $colexpr);
    warn "Parsing color terms: ", join(" + ", map("($_)", @term)) if $debug;
    my $const = 0;
    my @varmul;
    for my $term (@term) {
	if ($term =~ /^\-? ?\d+$/) {
	    $const += $term;
	} elsif ($term =~ /^(\S+) ?\* ?(\-?\d+)$/) {
	    my ($var, $mul) = ($1, $2);
	    push @varmul, [$var, $mul];
	} elsif ($term =~ /^(\-?\d+) ?\* ?(\S+)$/) {
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
	croak "In '$expr': no default var in that context" unless defined $defaultVar;
	($loc, $var) = ($expr, $defaultVar);
    }
    if (defined($loc) && defined($locRef) && !defined($locRef->{$loc})) {
	croak "In '$expr': no location identifier '$loc' has been bound\n";
    }
    return ($loc, $var);
}

sub hexv {
    my ($val) = @_;
    croak "Undefined value" unless defined $val;
    my $hex = sprintf ("%x", $val);
    return $hex;
}

sub decv {
    my ($val) = @_;
    if (length($val) > 8) {
	# this is pretty disgusting, should really be using pack/unpack or Bit::Vector
	return hex(substr($val,length($val)-8,8)) | (hex(substr($val,0,length($val)-8)) << 32);
    }
    return hex ($val);
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

sub min {
    my ($min, @x) = @_;
    for my $x (@x) { $min = $x if $x < $min }
    return $min;
}

sub max {
    my ($max, @x) = @_;
    for my $x (@x) { $max = $x if $x > $max }
    return $max;
}

sub minPowerOfTwo {
    my ($x) = @_;
    my $p = 1;
    while ($p < $x) { $p *= 2 }
    return $p;
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
  -savepp             file to save after preprocessing
  -verbose            report progress
  -debug              more info than you want

=head1 OPTIONS

=over 8

=item B<-help>

Prints a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=item B<-preprocessor>

Specify the preprocessor to use, plus options.

Default is "gcc -x c -E".

=item B<-savepp>

Specify a filename to save the intermediate file generated by running the input through the preprocessor.

=back

=head1 DESCRIPTION

B<This program> will compile a PixelZoo .zg file into an XML grammar.

=cut
