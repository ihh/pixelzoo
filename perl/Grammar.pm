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

# constants
	'typebits' => 16,
	'typeshift' => 48,
	'typemask' => 0xffff000000000000,
	'statemask' => 0xffffffffffffffff,
	'palette' => { 'white' => 0x007,
		       'red' => 0x03f },

# type/var dictionary
	'empty' => $emptyType,
	'type' => [ $emptyType ],
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
	# fetch key, value
	my $k = shift @kv_in;
	my $v = shift @kv_in;

	if (exists ($trans->{$k})) {
	    # if key is a transformation, apply the transformation to value
	    push @kv_out, &($trans->{$k}) ($self, $v);

	} else {
	    # neither key nor value represents code or transformation, so just copy value
	    push @kv_out, ($k, $self->transform_value ($v));
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
	    my %n = @$n;
	    my ($type, $rate, $rule, $hue, $sat, $bri) = map ($n{$_}, qw(name rate rule));
	    my @particle;

	    $self->push_scope;
	    $self->scope->loctype->{"o"} = $type;

	    my $transformed_rule = $self->transform_value ($rule);
	    $self->pop_scope;

	    push @particle, ('particle' => ['name' => $type,
					    'type' => getType($type),
					    'rate' => $rate,
					    map (exists($n->{$_}) ? ($_ => $n->{$_}) : (), qw(sync period phase)),
					    $self->parse_color ($n, $type, "hue", 1 << 16),
					    $self->parse_color ($n, $type, "sat", 1 << 8),
					    $self->parse_color ($n, $type, "bri", 1),
					    @$transformed_rule
			     ]);

	    return @particle;
	},

	# location identifier & type switch
	'.bind' => sub {
	    my ($self, $n) = @_;
	    my ($locid, $x, $y, $case, $default) = map ($n->{$_}, qw(loc x y case default));

	    if (defined($self->scope->loctype->{$locid})) {
		carp "Redundant/clashing bind: location identifier $locid already bound to type ", $self->scope->loctype->{$locid};
	    }

	    if (defined($self->scope->loc->{$locid})) {
		if (defined($x) || defined($y)) {
		    croak "Duplicate location identifier $locid";
		}
		($x, $y) = @{$self->scope->loc->{$locid}};
	    }

	    $self->push_scope;
	    $self->scope->loc->{$locid} = [$x, $y];

	    my %transformed_case;
	    while (($name, $rule) = each %$case) {
		$self->push_scope;
		$self->scope->loctype->{$locid} = $name;
		$transformed_case{$name} = [ $self->transform_value ($rule) ];
		$self->pop_scope;
	    }
	    my $transformed_default = [ $self->transform_value ($default) ];

	    $self->pop_scope;

	    return ('rule' => [ '@type' => 'switch',
				'pos' => { 'x' => $x, 'y' => $y },
				'mask' => hexv($self->typemask),
				'rshift' => $self->typeshift,
				map (('case' => ['.tstate' => ['tag' => 'state', 'type' => $_], 
						 @{$transformed_case{$_}}]),
				     keys %$case),
				'default' => $transformed_default ] );
	},


	# location var switch
	'.test' => sub {
	    my ($self, $n) = @_;
	    my ($locid, $varid, $case, $default) = map ($n->{$_}, qw(loc var case default));

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
	'.modify' => sub {
	    my ($self, $n) = @_;
	    my ($src, $dest, $set, $inc, $next) = map ($n->{$_}, qw(src dest set inc next));

	    # SOURCE
	    # check whether src specified
	    my ($srcloc, $srcvar, $srcmask, $srcshift);
	    if (defined $src) {
		($srcloc, $srcvar) = map ($src->{$_}, qw(loc var));
		$srcloc = "o" unless defined $srcloc;
		$srcvar = "*" unless defined $srcvar;

		# check src location defined
		if (!defined($self->scope->loc->{$srcloc})) {
		    croak "Undefined location identifier $locid";
		}

		# get source mask & shift
		my $srctype = $self->scope->loctype->{$srcloc};
		$srcmask = $self->getMask ($srctype, $srcvar);
		$srcshift = $self->getShift ($srctype, $srcvar);

	    } else {
		# no src specified, so zero before inc'ing
		$srcmask = $srcshift = 0;
	    }

	    # get src location
	    my ($srcx, $srcy) = @{$self->scope->loc->{$srcloc}};


	    # DESTINATION
	    # check whether dest specified
	    my ($destloc, $destvar, $destmask, $destshift);
	    if (defined $dest) {
		($destloc, $destvar) = map ($dest->{$_}, qw(loc var));
	    }
	    $destloc = "o" unless defined $destloc;
	    $destvar = $srcvar unless defined $destvar;

	    # check dest location defined
	    if (!defined($self->scope->loc->{$destloc})) {
		croak "Undefined location identifier $locid";
	    }

	    # get dest mask & shift
	    my $desttype = $self->scope->loctype->{$destloc};
	    $destmask = $self->getMask ($desttype, $destvar);
	    $destshift = $self->getShift ($desttype, $destvar);

	    # get dest location
	    my ($destx, $desty) = @{$self->scope->loc->{$destloc}};

	    # push scope
	    $self->push_scope;

	    # how to convert $inc to $offset, and update %{$self->scope->loctype}, depends on $destvar, $srcmask and $srcvar
	    my $offset;
	    if ($destvar eq "type") {
		if ($srcmask != 0) {
		    if (defined $inc) { croak "Can't do arithmetic on types" }
		    $offset = 0;
		    if ($srcvar eq "type") {
			$self->scope->loctype->{$destloc} = $self->scope->loctype->{$srcloc};
		    } else {
			carp "Warning: copying from a var to a type";
			$self->scope->loctype->{$destloc} = undef;
		    }
		} else {  # $destvar eq "type", $srcmask == 0
		    if (defined $inc) {
			$offset = $self->getType ($inc);
			$self->scope->loctype->{$destloc} = $inc;
		    } else {
			$offset = $self->getType ($self->emptyType);
			$self->scope->loctype->{$destloc} = $self->emptyType;
		    }
		}
	    } elsif ($destvar eq "*") {
		if ($srcmask != 0) {
		    if (defined $inc) { croak "Can't do arithmetic on type-vars tuples" }
		    $offset = 0;
		    if ($srcvar eq "*") {
			$self->scope->loctype->{$destloc} = $self->scope->loctype->{$srcloc};
		    } else {
			carp "Warning: copying from a var to a type-vars tuple";
			$self->scope->loctype->{$destloc} = undef;
		    }
		} else {  # $destvar eq "*", $srcmask == 0
		    if (defined $inc) {
			$offset = $self->typeAndVars ($inc);
			$offset = $self->getType ($inc);
			$self->scope->loctype->{$destloc} = $inc->{'type'};
		    } else {
			$offset = $self->getType ($self->emptyType);
			$self->scope->loctype->{$destloc} = $self->emptyType;
		    }
		}
	    } else {
		$offset = defined($inc) ? $inc : 0;
	    }

	    # transform next rule & pop scope
	    my @transformed_next = defined($next) ? ('next' => [ $self->transform_value ($next) ]) : ();
	    $self->pop_scope;

	    # return
	    return ('rule' => [ '@type' => 'modify',
				'src' => { 'x' => $srcx, 'y' => $srcy },
				'srcmask' => $srcmask,
				'rshift' => $srcshift,
				'inc' => $inc,
				'lshift' => $destshift,
				'destmask' => $destmask,
				'dest' => { 'x' => $destx, 'y' => $desty },
				@transformed_next ] );
	},


	# random (build a Huffman tree)
	'.huff' => sub {
	    my ($self, $n) = @_;
	    my @n = @$n;
	    my @node;
	    while (@n) {
		my $prob = shift @n;
		my $rule = shift @n;
		push @node, { 'prob' => $prob, 'rule' => $self->transform_value($rule) };
	    }

	    croak "Can't build a Huffman tree with no nodes" unless @node > 0;

	    while (@node > 1) {
		@node = sort { $b->{'prob'} <=> $a->{'prob'} } @node;
		my $l = shift @node;
		my $r = shift @node;
		unshift @node, { 'prob' => $l->{'prob'} + $r->{'prob'}, 'rule' => undef, 'l' => $l, 'r' => $r };
	    }

	    return huff_to_rule ($node[0]);
	},

	# overload
	'.load' => sub {
	    my ($self, $n) = @_;
	    return ('rule' => [ '@type' => 'overload',
				map (($_ => transform_value ($n->{$_})), qw(slow fast)) ] );
	},


	# text balloon
	'.text' => sub {
	    my ($self, $n) = @_;
	    return ("goal" => ['@type' => "and",  # "and"ing result with 0 means this goal will persist
			       "lazy" => "",
			       "goal" => ['@type' => "maybe",
					  "prob" => $n->{'rate'}],
			       "goal" => ['@type' => "balloon",
					  "balloon" => $n],
			       "goal" => ['@type' => "false"]]);
	}

    };
}

# helper to convert Huffman tree to random rule tree
sub huff_to_rule {
    my ($node) = @_;
    if (defined ($node->{'rule'})) {
	return ("rule" => $node->{'rule'});
    }
    return ('rule' => [ '@type' => 'random',
			'prob' => $node->{'l'}->{'prob'} / ($node->{'l'}->{'prob'} + $node->{'r'}->{'prob'}),
			'pass' => huff_to_rule ($node->{'l'}),
			'fail' => huff_to_rule ($node->{'r'}) ] );
}


# color helper
sub parse_color {
    my ($self, $n, $type, $tag, $mul) = @_;
    my @n = @$n;
    my @col;
    while (@n) {
	my $k = shift @n;
	my $v = shift @n;
	if ($k eq $tag) {
	    push @col, 'color' => [ exists ($v->{'var'})
				    ? ('mask' => getMask($type,$v->{'var'}),
							    'rshift' => getShift($type,$v->{'var'}),
							    'hexmul' => hexv ($mul * $v->{'mul'}))
				    : (),
				    exists ($v->{'inc'}) ? ('inc' => $v->{'inc'}) : () ];
	}
    }
    return @col;
}

# check if type or var name is illegal
sub illegal_name {
    my ($name) = @_;
    return $name eq "type" || $name eq "*" || $name =~ /^[\.\@]/;
}

# new type declaration w/ vars
sub new_type {
    my ($self, $type, @varname_varbits) = @_;
    croak "'$type' is an illegal type name" if illegal_name($type);
    croak "Type '$type' is already defined" if exists $self->typeindex->{$type};
    $self->typeindex->{$type} = @{$self->type};
    push @{$self->type}, $type;
    my ($offset, $typeoffset) = (0, 0);  # offset of next var in varbits & typebits
    while (@varname_varbits) {
	my $varname = shift @varname_varbits;
	my $varbits = shift @varname_varbits;
	croak "In type $type, '$varname' is an illegal var name" if illegal_name($varname);
	push @{$self->pvar->{$type}}, $varname;
	$self->pvbits->{$type}->{$varname} = $varbits;
	$self->pvoffset->{$type}->{$varname} = $offset;
	$offset += $varbits;
	croak "More than ", $self->typeshift, " bits of vars for type $type" if $offset > $self->typeshift;
    }
}

# entrance "brush"
sub make_entrance_brush {
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



# exit
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
		my $col = $self->palette->{'white'};  # white
		if ($r < $exitRadius/3 || $r > $exitRadius*2/3) {
		    $col = $self->palette->{'red'};  # red
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


# top-level proto-XML

my @game = ("goal" => ['@type' => "and",
		       "lazy" => "",
		       "cached" => "",

# place entrance and exit balloons
		       "goal" => ['@type' => "area",
				  "pos" => $self->entrancePort->{"pos"},
				  "goal" => ['@type' => "balloon",
					     "balloon" => ["text" => "ENTRANCE",
							   "persist" => '']]],

		       "goal" => ['@type' => "area",
				  "pos" => $exitPort{'pos'},
				  "goal" => ['@type' => "balloon",
					     "balloon" => ["text" => "EXIT",
							   "persist" => '']]],

# print hello message
		       "goal" => ['@type' => "print",
				  "text" => "Welcome to level 1!\n" .
				  "Guide guests safely to the Exit.\n"],

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
								    "intensity" => $self->make_entrance_brush()],
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
				  "text" => "The zoo is now closed to further guests.\nGuide all remaining guests to the Exit."],


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

sub print_xml {
    my $elt = new_XML_element("xml" => ["game" => \@game]);
    my $twig = XML::Twig->new(pretty_print => 'indented');
    $twig->set_root($elt);

    $twig->print;
}


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
    return hexv($self->statemask) if $var eq "*";
    return hexv($self->typemask) if $var eq "type";
    croak "Undefined type" unless defined($type);
    croak "Type '$type' unknown" unless defined $self->typeindex->{$type};
    croak "Var '$var' unknown for type '$type'" unless defined $self->pvbits->{$type}->{$var};
    my $mask = ((1 << $self->pvbits->{$type}->{$var}) - 1) << $self->pvoffset->{$type}->{$var};
    return hexv($mask);
}

sub getShift {
    my ($self, $type, $var) = @_;
    return 0 unless defined($var);
    return $self->typeshift if $var eq "type";
    return 0 if $var eq "*";
    croak "Undefined type" unless defined($type);
    croak "Type '$type' unknown" unless defined $self->typeindex->{$type};
    croak "Var '$var' unknown for type '$type'" unless defined $self->pvbits->{$type}->{$var};
    return $self->pvoffset->{$type}->{$var};
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
