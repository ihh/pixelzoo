#!/usr/bin/perl -w

package Grammar;

use strict;
use vars '@ISA';

use Exporter;
use Carp;
use XML::Twig;

use AutoHash;
@ISA = qw (Exporter AutoHash);
@EXPORT = qw (new from_fasta seq_ids segment AUTOLOAD);
@EXPORT_OK = @EXPORT;


# game data structures
sub new_grammar {
    my $emptyType = "empty";
    my $self = { 

# type info
	'empty' => $emptyType,
	'type' => [ $emptyType ],
	'typesize' => { $emptyType => 1 },  # number of actual Particle's represented by each type
	'typemask' => { $emptyType => 0xffff },  # mask for recognizing each type
	'pvar' => { $emptyType => [qw(hue saturation value)] },    # $pvar{$type} = [$var1,$var2,$var3,...]
	'pvbits' => { $emptyType => { 'hue' => 6, 'saturation' => 3, 'value' => 3 } },   # $pvbits{$type}->{$var}
	'pvoffset' => {},   # $pvoffset{$type}->{$var}
	'ptags' => { $emptyType => [] },     # $ptags{$type} = [ ... ]
	'hue' => [ $emptyType => [] ], 'sat' => [ $emptyType => [] ], 'bri' => [ $emptyType => [] ],   # $hue{$type} = [[$const1,$var1,$mul1], [$const2,$var2,$mul2], ...]   etc

	'typeindex' => {},  # $typeindex{$type}

# other game data
	'boardRate' => 240,
	'boardSize' => 128,
	'exitPort' => ["pos" => { "x" => 0, "y" => 0 }, "count" => 0, "radius" => 6],
	'exitType' => $emptyType,

	'tool' => [],  # $tool[$n] = [$name, $size, $type, $reserve, $recharge, $sprayRate, \@overwriteType]
	'init' => [],  # $init[$n] = [$x, $y, $type]

	'entrancePort' => ["pos" => { "x" => 0, "y" => 0 }, "count" => 0, "rate" => 1, "width" => 1, "height" => 1],
	'entranceType' => $emptyType,

# compiler stuff
	'compiler_warnings' => {},
	'trans' => initial_transform_hash(),

	'scope' => AutoHash->new ('tag' => [],
				  'type' => undef,
				  'loc' => { 'o' => [0,0],
					     'n' => [0,-1],
					     'e' => [+1,0],
					     's' => [0,+1],
					     'w' => [-1,0],
					     'nw' => [-1,-1],
					     'sw' => [-1,+1],
					     'ne' => [+1,-1],
					     'se' => [+1,+1] },
				  'loctype' => { }),
	'scopeStack' => [],
	
    };

    bless $self, $class;


    return $self;
}






# example proto-XML (before transformation)
my $blah = "
 huffman => [ .3, { ... },
              .5, { ... },
              .2, { loc => { name => newLoc, x => 0, y => +1, type = whateverType,
                    rule => { switch => { loc => newLoc, var => whateverVar,
                               case => { val => 0, modify => { loc => newLoc, var => whateverVar, inc => 1 } },
                               default => { ... }

";

# compiler scope
sub push_scope {
    my ($self) = @_;
    push @{$self->scopeStack}, $self->scope->deepcopy;
}

sub pop_scope {
    my ($self) = @_;
    $self->scope (pop @{$self->scopeStack});
}

# the original parse/generate lines can be reimagined as transformations on a proto-XML tree
# the two parts in each case need to be stitched together

# transform a list of (key,value) pairs
sub transform_list {
    my ($self, @kv_in) = @_;
    my @kv_out;
    my $trans = $self->transform_hash;

    while (@kv_in) {
	# fetch key
	my $k = shift @kv_in;

	# if key is a coderef, don't fetch value, just expand key
	if (ref($k) && ref($k) eq 'CODE') {
	    my @k = &$k ($self);
	    unshift @kv_in, @k;

	} else {
	    # fetch value
	    my $v = shift @kv_in;

	    # if key is a transformation, apply the transformation to value
	    if (exists ($trans->{$k})) {
		unshift @kv_in, &($trans->{$k}) ($self, $v);

		# if value is a coderef, expand it
	    } elsif (ref($v) && ref($v) eq 'CODE') {
		my @v = &$v ($self);
		croak "Value coderef returned list of length != 1" unless @v == 1;
		unshift @kv_in, $k, $v[0];

		# neither key nor value represents code or transformation, so just copy value
	    } else {
		push @kv_out, ($k, $self->transform_value ($v));
	    }
	}
    }

    return @kv_out;
}

# transform a single value
sub transform_value {
    my ($self, $v) = @_;
    if (ref($v) && ref($v) eq 'HASH') {
	return $self->transform_list (%$v);
    } elsif (ref($v) && ref($v) eq 'ARRAY') {
	return $self->transform_list (@$v);
    } elsif (ref($v)) {
	croak "Illegal reference type: $v";
    }
    # default: $v is a scalar
    return $v;
}


# initial set of transformation function keywords
sub initial_transform_hash {

    # main hash
    return {
	'.type' => sub { my ($self, $n) = @_; return ($n->{'tag'}, $self->getType($n->{'type'})) },
	'.tstate' => sub { my ($self, $n) = @_; return ($n->{'tag'}, $self->getTypeAsHexState($n->{'type'})) },
	'.tmask' => sub { my ($self, $n) = @_; return ($n->{'tag'}, $self->getMask($n->{'type'},'type')) },
	'.vmask' => sub { my ($self, $n) = @_; return ($n->{'tag'}, $self->getMask($n->{'type'},$n->{'var'})) },
	'.vshift' => sub { my ($self, $n) = @_; return ($n->{'tag'}, $self->getShift($n->{'type'},$n->{'var'})) },
	'.state' => sub { my ($self, $n) = @_; return ($n->{'tag'}, $self->typeAndVars($n)) },
	'.hexstate' => sub { my ($self, $n) = @_; return ($n->{'tag'}, hexv ($self->typeAndVars($n))) },

	# now come the cunning transformations...
	# recursive transformations (for rules) that call transform_list or transform_value on their subtrees
	# aided & abetted by expansion into further-expanded (but simpler) macros of the above form (.type, .tmask, etc)

	# begin particle
	'.particle' => sub {
	    my ($self, $n) = @_;
	    my ($name, $type, $rate, $rule) = map ($n->{$_}, qw(name type rate rule));

	    $self->push_scope;
	    $self->scope->loctype->{"o"} = $self->scope->type = $type;
	    
	    my $transformed_rule = $self->transform_value ($rule);
	    $self->pop_scope;

	    return ('particle' => ['name' => $name,
				   'type' => $type,
				   'rate' => $rate,
				   map (exists($n->{$_}) ? ($_ => $n->{$_}) : (), qw(sync period phase)),
				   @$transformed_rule
		    ]);
	},

	# location identifier & type switch
	'.bind' => sub {
	    my ($self, $n) = @_;
	    my ($locid, $alias, $x, $y, $case, $default) = map ($n->{$_}, qw(id alias x y case default));

	    my $locid_test = $locid;
	    if (defined($self->scope->loc->{$locid})) {
		if (defined($alias) || defined($x) || defined($y)) {
		    croak "Duplicate location identifier $locid";
		}
		($x, $y) = @{$self->scope->loc->{$locid}};

	    } elsif (defined $alias) {
		croak "In $locid: can't specify (x,y) and alias at the same time" if defined($x) || defined($y);
		($x, $y) = @{$self->scope->loc->{$alias}};
		$locid_test = $locid;
	    }

	    if (defined($self->scope->loctype->{$locid_test})) {
		carp "Redundant/clashing bind: location identifier $alias already bound to type ", $self->scope->loctype->{$locid_test};
	    }

	    $self->push_scope;
	    $self->scope->loc->{$locid} = [$x, $y];

	    my $typemask = 0;
	    my %transformed_case;
	    while (($name, $rule) = each %$case) {
		$typemask = $typemask | $self->typemask->{$name};
		$self->push_scope;
		$self->scope->loctype->{$locid} = $name;
		$transformed_case{$name} = [ $self->transform_value ($rule) ];
		$self->pop_scope;
	    }
	    my $transformed_default = [ $self->transform_value ($default) ];

	    $self->pop_scope;

	    return ('rule' => [ '@type' => 'switch',
				'pos' => { 'x' => $x, 'y' => $y },
				'mask' => hexv($typemask),
				'rshift' => $self->getShift(undef,"type"),
				map (('case' => ['.tstate' => ['tag' => 'state', 'type' => $_], 
						 @{$transformed_case{$_}}]),
				     keys %$case),
				'default' => $transformed_default ] );
	},


	# location var switch
	'.test' => sub {
	    my ($self, $n) = @_;
	    my ($locid, $varid, $case, $default) = map ($n->{$_}, qw(id var case default));

	    if (!defined($self->scope->loc->{$locid})) {
		croak "Undefined location identifier $locid";
	    }

	    my ($x, $y) = @{$self->scope->loc->{$locid}};

	    if (!defined($self->scope->loctype->{$locid})) {
		croak "Type of location $locid is not bound, so cannot access vars";
	    }

	    my $loctype = $self->scope->loctype->{$locid};

	    my %transformed_case;
	    while (($name, $rule) = each %$case) {
		$transformed_case{$name} = [ $self->transform_value ($rule) ];
	    }
	    my $transformed_default = [ $self->transform_value ($default) ];
	    
	    return ('rule' => [ '@type' => 'switch',
				'pos' => { 'x' => $x, 'y' => $y },
				'mask' => $self->getMask($loctype,$varid),
				'rshift' => $self->getShift($loctype,$varid),
				map (('case' => ['tstate' => $_,
						 @{$transformed_case{$_}}]),
				     keys %$case),
				'default' => $transformed_default ] );
	},


	# modify


	# random (build a Huffman tree)

    };
}


# exit (part 2)
sub make_exit {
    my ($self) = @_;
    my (@exitLoc, @exitInit);
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
		push @exitInit, "init" => ["x" => $ex, "y" => $ey, "hexval" => hexv($col)];
	    }
	}
    }
    return (\@exitLoc, \@exitInit);
}

# entrance
sub make_entrance {
    my ($self) = @_;
    my @entranceLoc;
    my $entranceWidth = $self->entrancePort->{"width"};
    my $entranceHeight = $self->entrancePort->{"height"};
    for (my $w = 0; $w < $entranceWidth; ++$w) {
	for (my $h = 0; $h < $entranceHeight; ++$h) {
	    push @entranceLoc, "pos" => ["x" => $w, "y" => $h];
	}
    }
    return \@entranceLoc;
}

# new type declaration w/ vars
sub new_type {
    my ($self, $type, @varname_varbits) = @_;
    croak "Type '$type' is already defined" if exists $self->typemask->{$type};
    push @{$self->type}, $type;
    my ($offset, $typeoffset) = (0, 0);  # offset of next var in varbits & typebits
    while (@varname_varbits) {
	my $varname = shift @varname_varbits;
	my $varbits = shift @varname_varbits;
	my $intype = $varname =~ s/\!$//;
	croak "You cannot use 'type' as a var name for $type" if $varname eq "type";
	push @{$pvar{$type}}, $varname;
	$pvbits{$type}->{$varname} = $varbits;
	if ($intype) {
	    $pvoffset{$type}->{$varname} = $typeoffset + 48;
	    $typeoffset += $varbits;
	    croak "More than 15 bits of type-encoded vars for type $type" if $typeoffset > 15;
	} else {
	    $pvoffset{$type}->{$varname} = $offset;
	    $offset += $varbits;
	    croak "More than 48 bits of vars for type $type" if $offset > 48;
	}
    }
    $typesize{$type} = 1 << $typeoffset;
    $typemask{$type} = ((1 << (16 - $typeoffset)) - 1) << $typeoffset;
    $ptags{$type} = [];
    $ruleTags{$type} = [];

    
}

sub set_type_hsb {
    my ($self, $type, $hue, $sat, $bri) = @_;
    $self->hue->{$type} = parseColor($hue);
    $self->sat->{$type} = parseColor($sat);
    $self->bri->{$type} = parseColor($bri);
}



# here is where we transform a node in the rule tree from locs/names into coords/types
# the first parts of the "transformation" are actually just about defining names for locs, and binding (recognizing) types at those locs


# TODO: automatically declare 'o' at (0,0), 'w' at (-1,0), 'nw' at (-1,-1), etc.





# modify (part 1)
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
	croak "If one side of a 'do' expression involves the whole state, then both sides must.\nOffending line:\n$_\n";
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


# old loop over operations (refactor into part 2 of modify)
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





# text balloons (goals)

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
    croak $_;

} elsif (/\S/) {
    # unrecognized line within rule block
    croak "Syntax error in rule block (type $type): '$_'\n";
}
	    }


	    # end of rule block
	    $ruleTags{$type}->[$nRule] = \@tag;
	    ++$nRule;



	} elsif (/^<.*>/) {
	    parseTags ($_, $ptags{$type});



	} elsif (/\S/) {
	    # unrecognized line
	    croak "Syntax error in file (type $type): '$_'\n";


	}
    } elsif (/\S/) {
	# unrecognized line & no type defined
	croak "Syntax error in file (no type defined): '$_'\n";
    }
}




# here is where we finalize the type names

# assign type indices (TODO: optimize packing)
my $idx = 0;
for my $type (@type) {
    my $typeEncodedVarMask = 0xffff ^ $typemask{$type};
    while ($idx & $typeEncodedVarMask) { ++$idx }
    $typeindex{$type} = $idx;
    $idx += $typesize{$type};
}
croak "Too many types - maybe implement optimized packing?" if $idx > 0x10000;



# here are the second parts of the transformations, where we convert locs/names into coords/types

my @gram;
for my $type (@type) {
    warn "Generating XML for base type '$type'\n" if $verbose;



# loop over all types in a type block, encoding the vars into the name for quick debug in client
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








    } # end loop over type-encoded vars

# .....aaaaand, save the particle.
push @gram, "particle" => \@particle;
}



# tools

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




# top-level proto-XML

my @game = ("goal" => ['@type' => "and",
		       "lazy" => "",
		       "cached" => "",

# place entrance and exit balloons
		       "goal" => ['@type' => "area",
				  "pos" => $self->entrancePort->{"pos"},
				  "goal" => ['@type' => "balloon",
					     "balloon" => ["text" => "ZOO ENTRANCE",
							   "persist" => '']]],

		       "goal" => ['@type' => "area",
				  "pos" => $exitPort{'pos'},
				  "goal" => ['@type' => "balloon",
					     "balloon" => ["text" => "GIFT SHOP",
							   "persist" => '']]],

# print hello message
		       "goal" => ['@type' => "print",
				  "text" => "Welcome to level 1!\n" .
				  "Guide guests safely to the Gift Shop.\n"],

# open the guest exit (currently the only exit)
		       "goal" => ['@type' => "setexit",
				  "state" => "PortalCounting"],


# introduce the guests
		       "goal" => ['@type' => "area",
				  "pos" => $self->entrancePort->{"pos"},
				  "goal" => ['@type' => "spray",
					     "tool" => ["name" => "entrance",
							"size" => minPowerOfTwo (max ($entranceWidth, $entranceHeight)),
							"brush" => ["center" => ["x" => int($entranceWidth/2), "y" => int($entranceHeight/2)],
								    "intensity" => \@entranceLoc],
							"spray" => 1,
							"hexstate" => getTypeAsHexState($entranceType),
							"reserve" => $self->entrancePort->{'count'},
							"recharge" => 0]]],

# delete entrance balloon
		       "goal" => ['@type' => "area",
				  "pos" => $self->entrancePort->{"pos"},
				  "goal" => ['@type' => "balloon"]],

# print status message
		       "goal" => ['@type' => "print",
				  "text" => "The zoo is now closed to further guests.\nGuide all remaining guests to the gift shop."],


# more goals here... e.g.,
# generic:
#    require the player to maintain the min/max bounds on population levels (guests, animals, expensive particles e.g. fire)

# specific:
#  fend off attacks to the guests, or by animals on other animals
#  grow food for the guests
#  hurry the guests along with stimulant
#  put out fires
#  protect the guests from avalanches, lasers, poison gas
#  place signposts
#  build cages, place animals in cages

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
    "exit" => [%exitPort, @exitLoc],
    "board" => ["size" => $boardSize,
		"grammar" => \@gram,
		@exitInit,
		@init > 0
		? map (("init" => ["x" => $$_[0],
				   "y" => $$_[1],
				   "type" => getType($$_[2])]),
    @init)
    : ()]);





# code to actually generate & print real XML from proto-XML

warn "Writing XML\n" if $verbose;
my $elt = new_XML_element("xml" => ["game" => \@game]);
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
	croak sprintf($fmt,@args) . "Can't access $loc.$var because type of $loc is not bound\n";
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

# fully-qualified particle state: .state => { 'tag' => tag, 'type' => name, 'var' => { 'varname1' => val1, 'varname2' => val2, ... } }
sub typeAndVars {
    my ($self, $n) = @_;
    my $type = $n->{'type'};
    my $state = $self->getType($type) << $self->getShift($type,"type");
    while (my ($var, $val) = each %{$n->{'var'}}) {
	$state = $state | ($val << $self->getShift($type,$var));
	}
    return $state;
};

sub getTypeAsHexState {
    my ($self, $type) = @_;
    return hexv($self->getType($type)).("0"x12);
}

sub getType {
    my ($self, $type) = @_;
    if ($type =~ /^\-?\d+$/) {
	return $type;
    }
    croak "Type '$type' unknown" unless defined $self->typeindex->{$type};
    return $self->typeindex->{$type};
}

sub getMask {
    my ($self, $type, $var) = @_;
    return 0 unless defined($var);
    return "ffffffffffffffff" if $var eq "*";
    return hexv($self->typemask->{$type}) . "000000000000" if $var eq "type";
    croak "Undefined type" unless defined($type);
    croak "Type '$type' unknown" unless defined $self->typeindex->{$type};
    croak "Var '$var' unknown for type '$type'" unless defined $self->pvbits->{$type}->{$var};
    my $mask = ((1 << $self->pvbits->{$type}->{$var}) - 1) << $self->pvoffset->{$type}->{$var};
    return hexv($mask);
}

sub getShift {
    my ($self, $type, $var) = @_;
    return 0 unless defined($var);
    return 48 if $var eq "type";
    return 0 if $var eq "*";
    croak "Undefined type" unless defined($type);
    croak "Type '$type' unknown" unless defined $self->typeindex->{$type};
    croak "Var '$var' unknown for type '$type'" unless defined $self->pvbits->{$type}->{$var};
    return $self->pvoffset->{$type}->{$var};
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

sub new_XML_element {
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
	@child = map (ref($_) eq "CODE" ? &$_() : $_, @child);
	while (@child) {
	    my $k = shift @child;
	    my $v = shift @child;
	    if ($k =~ s/^\@//) {
		$t->set_att ($k => $v);
	    } else {
		new_XML_element($k, $v)->paste(last_child => $t);
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
