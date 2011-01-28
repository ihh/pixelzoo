#!/usr/bin/perl -w

package Grammar;

use strict;
use vars ('@ISA', '@EXPORT', '@EXPORT_OK');

use Exporter;
use Carp qw(carp croak cluck confess);
use XML::Twig;
use Data::Dumper;
use File::Temp;

use AutoHash;

@ISA = qw (AutoHash);
@EXPORT = qw (hexv decv min max minPowerOfTwo sortHash forceHash AUTOLOAD);
@EXPORT_OK = @EXPORT;


# game data structures
sub newGrammar {
    my ($class) = @_;
    my $emptyType = 'empty';
    my $orig = 'o';
    my $self = { 

# constants
	'typebits' => 16,
	'typeshift' => 48,
	'typemask' => 'ffff000000000000',
	'statemask' => 'ffffffffffffffff',
	'palette' => { 'white' => 0x007,
		       'red' => 0x03f },

# type/var dictionary
	'type' => [ ],
	'pvar' => { },    # $pvar{$type} = [$var1,$var2,$var3,...]
	'pvbits' => { },   # $pvbits{$type}->{$var}
	'pvoffset' => { },   # $pvoffset{$type}->{$var}
	'ptags' => { },     # $ptags{$type} = [ ... ]
	'typeindex' => {},  # $typeindex{$type}

# other game data
	'boardRate' => 240,
	'boardSize' => 128,

# XML
	'xml' => AutoHash->new ('game' => [],   # allow designer/tester to override entire default Game structure (almost certainly not advisable)
				'goal' => [],   # allow designer to override default Goal structure (probably not advisable)
				'init' => [],   # allow designed to initialize Board
				'tool' => [],   # to add tools, use addTool helper
				'type' => []),

# compiler stuff
	'debug' => 0,
	'verbose' => 0,
	'compiler_warnings' => {},
	'trans' => transform_hash(),

	'scope' => AutoHash->new ('type' => undef,
				  'loc' => { $orig => [0,0],
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

	'xmllint' => 'xmllint --noout --dtdvalid',
	'gameDTD' => pixelzoo_dir('dtd/game.dtd'),

# other helpers
	'empty' => $emptyType,
	'origin' => $orig,
	'dir' => AutoHash->new ('n' => AutoHash->new ( 'x' => 0,  'y' => -1 ),
				'e' => AutoHash->new ( 'x' => +1, 'y' => 0 ),
				's' => AutoHash->new ( 'x' => 0,  'y' => +1 ),
				'w' => AutoHash->new ( 'x' => -1, 'y' => 0 ),
				'nw' => AutoHash->new ( 'x' => -1, 'y' => -1 ),
				'ne' => AutoHash->new ( 'x' => +1, 'y' => -1 ),
				'se' => AutoHash->new ( 'x' => +1, 'y' => +1 ),
				'sw' => AutoHash->new ( 'x' => -1, 'y' => +1 )),

# end of $self
    };

    bless $self, $class;

    $self->addType ('name' => $emptyType,
		    'var' => { 'hue' => 6, 'saturation' => 3, 'brightness' => 3 },
		    'hue' => { 'var' => 'hue' },
		    'sat' => { 'var' => 'saturation' },
		    'bri' => { 'var' => 'brightness' },
		    'rate' => 0,
		    'rule' => ['nop']);

    return $self;
}


# new type helper
sub addType {
    my ($self, @type_proto_xml) = @_;
    push @{$self->xml->type}, 'particle' => { @type_proto_xml };
}

# new tool helper
sub addTool {
    my ($self, @tool_proto_xml) = @_;
    push @{$self->xml->tool}, 'tool' => [ sortHash ({@tool_proto_xml}, qw(name size brush gstate gtype overwrite spray reserve recharge maxreserve hide)) ];
}


# top-level method to generate, compile & print XML
sub print {
    my ($self) = @_;

    warn "Parsing types...\n" if $self->verbose;
    $self->parse_types;

    warn "Generating proto-XML...\n" if $self->verbose;
    my $game_proto = $self->make_game;
    my $wrapped_proto = ["game" => $game_proto];

    if ($self->debug) {
	warn "Proto-XML passed to Grammar::transform_proto:\n", Data::Dumper->Dump($wrapped_proto);
    }

    warn "Compiling proto-XML...\n" if $self->verbose;
    my $transformed_proto = $self->transform_proto ($wrapped_proto);
    $self->print_proto_xml ($transformed_proto);
}

# dummy methods subclasses should override
sub make_goal {
    return ();
}

sub make_exit {
    return ();
}

sub make_exit_init {
    return ();
}

# top-level proto-XML
sub make_game {
    my ($self) = @_;
    if (@{$self->xml->game} > 0) {
	return $self->xml->game;
    }

    my @game = ("board" => ["size" => $self->boardSize,
			    "grammar" => $self->xml->type,
			    $self->make_exit_init,
			    @{$self->xml->init}],

		"rate" => $self->boardRate,

		@{$self->xml->tool},

		$self->make_exit,

		@{$self->xml->goal} > 0
		? @{$self->xml->goal}   # allow designer to override default Goal structure
		: $self->make_goal,

	);

    return \@game;
}

# parse_types: parse type declarations
sub parse_types {
    my ($self) = @_;
    $self->parse_node ($self->xml->type);
}

# transform_proto: take proto-XML input, parse type declarations, apply transformations
sub transform_proto {
    my ($self, $proto) = @_;
    return $self->transform_value ($proto);
}

sub parse_node {
    my ($self, $node) = @_;
    my @node;
    if (ref($node) && ref($node) eq 'HASH') {
	@node = %$node;
    } elsif (ref($node) && ref($node) eq 'ARRAY') {
	@node = @$node;
    } elsif (ref($node)) {
	confess "Illegal reference type: $node";
    } else {
	warn "Parsing $node";
    }
    while (@node) {
	my $k = shift @node;
	my $v = shift @node;
	if ($k eq 'particle') {
	    my $vhash = forceHash($v);
	    $self->declare_type ($vhash->{'name'}, defined($vhash->{'var'}) ? %{forceHash($vhash->{'var'})} : ());
	} else {
	    $self->parse_node ($v);
	}
    }
}

# compiler scope
sub push_scope {
    my ($self) = @_;
#    warn "Pushing scope" if $self->debug;
    my $scope = $self->scope->deepcopy;
    push @{$self->scopeStack}, $scope;
}

sub pop_scope {
    my ($self) = @_;
#    warn "Popping scope" if $self->debug;
    my $scope = pop @{$self->scopeStack};
    $self->scope ($scope);
}

sub indent {
    my ($self) = @_;
    return " " x @{$self->scopeStack};
}

# transform a list of (key,value) pairs
sub transform_list {
    my ($self, @kv_in) = @_;

    warn "Transforming list (@kv_in)" if $self->debug;

    my @kv_out;
    my $trans = $self->transform_hash;

    while (@kv_in) {
	# fetch key, value
	my $k = shift @kv_in;
	my $v = shift @kv_in;

	# if key is a transformation, apply the transformation to value
	if (exists ($trans->{$k})) {
	    if ($self->debug) {
		if (ref($v) && ref($v) eq 'ARRAY') {
		    warn "Evaluating $k(@$v)";
		} elsif (ref($v) && ref($v) eq 'HASH') {
		    warn "Evaluating $k(", join(" ",%$v), ")";
		} else {
		    warn "Evaluating $k($v)";
		}
	    }
	    push @kv_out, &{$trans->{$k}} ($self, $v);

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

    warn "Transforming value $v" if $self->debug;

    if (ref($v) && ref($v) eq 'HASH') {
	return { $self->transform_list (%$v) };
    } elsif (ref($v) && ref($v) eq 'ARRAY') {
	return [ $self->transform_list (@$v) ];
    } elsif (ref($v)) {
	confess "Illegal reference type: $v";
    }
    # default: $v is a scalar
    return $v;
}

# transformation function helper
sub forceHash {
    my ($n) = @_;
    if (ref($n) && ref($n) eq 'HASH') {
	return $n;
    } elsif (ref($n) && ref($n) eq 'ARRAY') {
	return {@$n};
    }
    confess "Not a HASH";
}

# transformation functions
sub transform_hash {

    # main hash
    return {
	'gtype' => sub { my ($self, $n) = @_; return ('type' => $self->getType($n)) },
	'gstate' => sub { my ($self, $n) = @_; return ('hexstate' => $self->getTypeAsHexState($n)) },
	'gvars' => sub { my ($self, $n) = @_; $n = forceHash($n); return ($n->{'@tag'} || 'hexstate', hexv($self->typeAndVars($n))) },
	'tmask' => sub { my ($self, $n) = @_; return ('mask', $self->getMask($n,'type')) },
	'vmask' => sub { my ($self, $n) = @_; $n = forceHash($n); return ($n->{'@tag'} || 'mask', $self->getMask($n->{'type'},$n->{'var'})) },
	'vshift' => sub { my ($self, $n) = @_; $n = forceHash($n); return ($n->{'@tag'} || 'rshift', $self->getShift($n->{'type'},$n->{'var'})) },

	# now come the cunning transformations...
	# recursive transformations (for rules) that call transform_list or transform_value on their subtrees
	# aided & abetted by expansion into further-expanded (but simpler) macros of the above form (.type, .tmask, etc)

	# particle declaration
	'particle' => sub {
	    my ($self, $n) = @_;
	    $n = forceHash ($n);

	    my ($type, $rate, $rule, $hue, $sat, $bri) = map ($n->{$_}, qw(name rate rule));
	    my @particle;

	    warn "Transforming particle '$type'\n" if $self->verbose;

	    $self->push_scope;
	    $self->scope->loctype->{$self->origin} = $type;
	    $self->scope->type($type);

	    my $transformed_rule = $self->transform_value ($rule);
	    $self->pop_scope;

	    push @particle, ('particle' => ['name' => $type,
					    'type' => $self->getType($type),
					    $self->parse_color ($n, $type, "hue", 1 << 16),
					    $self->parse_color ($n, $type, "sat", 1 << 8),
					    $self->parse_color ($n, $type, "bri", 1),
					    map (exists($n->{$_}) ? ($_ => $n->{$_}) : (), qw(sync period phase)),
					    'rate' => $rate,
					    @$transformed_rule
			     ]);

	    return @particle;
	},

	# tool
	'tool' => sub {
	    my ($self, $n) = @_;
	    $n = forceHash ($n);

	    return ('tool' => [ $self->transform_list (sortHash ($n, qw(name size brush state hexstate gstate gtype overwrite spray reserve recharge maxreserve hide))) ]);
	},

	# location identifier & type switch
	'bind' => sub {
	    my ($self, $n) = @_;
	    $n = forceHash($n);

	    my ($locid, $x, $y, $case, $default) = map ($n->{$_}, qw(loc x y case default));
	    $case = {} unless defined $case;

	    if (defined($self->scope->loctype->{$locid})) {
		cluck "Redundant/clashing bind: location identifier $locid already bound to type ", $self->scope->loctype->{$locid};
	    }

	    if (defined($self->scope->loc->{$locid})) {
		if (defined($x) || defined($y)) {
		    confess "Duplicate location identifier $locid";
		}
		($x, $y) = @{$self->scope->loc->{$locid}};
	    }

	    $self->push_scope;
	    $self->scope->loc->{$locid} = [$x, $y];

	    my %case_hash = %{forceHash($case)};
	    my %transformed_case;
	    while (my ($name, $rule) = each %case_hash) {
		$self->push_scope;
		$self->scope->loctype->{$locid} = $name;
		$transformed_case{$name} = $self->transform_value ($rule);
		$self->pop_scope;
	    }
	    my $transformed_default = $self->transform_value ($default);

	    $self->pop_scope;

	    return ('rule' => [ 'switch' => ['pos' => { 'x' => $x, 'y' => $y },
					     'mask' => $self->typemask,
					     'rshift' => $self->typeshift,
					     map (('case' => ['state' => $self->getType($_),
							      @{$transformed_case{$_}}]),
						  keys %case_hash),
					     defined($default) ? ('default' => $transformed_default) : () ] ] );
	},


	# location var switch
	'switch' => sub {
	    my ($self, $n) = @_;
	    $n = forceHash($n);

	    my ($locid, $varid, $case, $default) = map ($n->{$_}, qw(loc var case default));
	    $case = {} unless defined $case;

	    if (!defined($self->scope->loc->{$locid})) {
		confess "Undefined location identifier $locid";
	    }

	    my ($x, $y) = @{$self->scope->loc->{$locid}};

	    $self->assertLocTypeBound ($locid, $varid);
	    my $loctype = $self->scope->loctype->{$locid};

	    my %case_hash = %{forceHash($case)};
	    my %transformed_case;
	    while (my ($name, $rule) = each %case_hash) {
		$transformed_case{$name} = $self->transform_value ($rule);
	    }
	    my $transformed_default = $self->transform_value ($default);
	    
	    return ('rule' => [ 'switch' => [ 'pos' => { 'x' => $x, 'y' => $y },
					      'mask' => $self->getMask($loctype,$varid),
					      'rshift' => $self->getShift($loctype,$varid),
					      map (('case' => ['state' => $_,
							       @{$transformed_case{$_}}]),
						   keys %case_hash),
					      defined($default) ? ('default' => $transformed_default) : () ] ] );
	},


	# modify
	'modify' => sub {
	    my ($self, $n) = @_;
	    $n = forceHash ($n);

	    my ($src, $dest, $set, $inc, $next) = map ($n->{$_}, qw(src dest set inc next));
	    confess "In .modify: can't specify 'set' together with 'src' or 'inc'" if defined($set) && (defined($src) || defined($inc));
	    $inc = $set if defined $set;

	    # SOURCE
	    # check whether src specified
	    my ($srcloc, $srcvar, $srcmask, $srcshift);
	    if (defined $src) {
		my $srchash = forceHash($src);
		($srcloc, $srcvar) = map ($srchash->{$_}, qw(loc var));

		$srcloc = $self->origin unless defined $srcloc;
		$srcvar = "*" unless defined $srcvar;

		# check src location defined
		if (!defined($self->scope->loc->{$srcloc})) {
		    confess "Undefined location identifier $srcloc";
		}

		# get source mask & shift
		$self->assertLocTypeBound ($srcloc, $srcvar);
		my $srctype = $self->scope->loctype->{$srcloc};
		$srcmask = $self->getMask ($srctype, $srcvar);
		$srcshift = $self->getShift ($srctype, $srcvar);

	    } else {
		$srcloc = $self->origin;
		$srcvar = "*";

		# no src specified, so zero before inc'ing
		$srcmask = $srcshift = 0;
	    }

	    # get src location
	    my ($srcx, $srcy) = @{$self->scope->loc->{$srcloc}};

	    # DESTINATION
	    # check whether dest specified
	    my ($destloc, $destvar, $destmask, $destshift);
	    if (defined $dest) {
		my $desthash = forceHash($dest);
		($destloc, $destvar) = map ($desthash->{$_}, qw(loc var));
	    }
	    $destloc = $self->origin unless defined $destloc;
	    $destvar = $srcvar unless defined $destvar;

	    # check dest location defined
	    if (!defined($self->scope->loc->{$destloc})) {
		confess "Undefined location identifier $destloc";
	    }

	    # get dest mask & shift
	    $self->assertLocTypeBound ($destloc, $destvar);
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
		    if (defined $inc) { confess "Can't do arithmetic on types" }
		    $offset = 0;
		    if ($srcvar eq "type") {
			$self->scope->loctype->{$destloc} = $self->scope->loctype->{$srcloc};
		    } else {
			cluck "Warning: copying from a var to a type";
			$self->scope->loctype->{$destloc} = undef;
		    }
		} else {  # $destvar eq "type", $srcmask == 0
		    if (defined $inc) {
			$offset = $self->getType ($inc);
			$self->scope->loctype->{$destloc} = $inc;
		    } else {
			$offset = $self->getType ($self->empty);
			$self->scope->loctype->{$destloc} = $self->empty;
		    }
		}
	    } elsif ($destvar eq "*") {
		if (decv($srcmask) != 0) {
		    if (defined $inc) { confess "Can't do arithmetic on type-vars tuples" }
		    $offset = 0;
		    if ($srcvar eq "*") {
			$self->scope->loctype->{$destloc} = $self->scope->loctype->{$srcloc};
		    } else {
			cluck "Warning: copying from a var to a type-vars tuple";
			$self->scope->loctype->{$destloc} = undef;
		    }
		} else {  # $destvar eq "*", $srcmask == 0
		    if (defined $inc) {
			$offset = $self->typeAndVars ($inc);
			$self->scope->loctype->{$destloc} = forceHash($inc)->{'type'};
		    } else {
			$offset = $self->getType ($self->empty);
			$self->scope->loctype->{$destloc} = $self->empty;
		    }
		}
	    } else {
		$offset = defined($inc) ? $inc : 0;
	    }

	    # transform next rule & pop scope
	    my @transformed_next = defined($next) ? ('next' => $self->transform_value ($next)) : ();
	    $self->pop_scope;

	    # return
	    return ('rule' => [ 'modify' => [ 'src' => { 'x' => $srcx, 'y' => $srcy },
					      'srcmask' => $srcmask,
					      'rshift' => $srcshift,
					      defined($offset) ? ('hexinc' => hexv($offset)) : (),
					      'lshift' => $destshift,
					      'destmask' => $destmask,
					      'dest' => { 'x' => $destx, 'y' => $desty },
					      @transformed_next ] ] );
	},


	# nop (no operation)
	'nop' => sub {
	    return ('rule' => [ 'modify' => [ 'destmask' => 0 ] ]);
	},

	# random (build a Huffman tree)
	'huff' => sub {
	    my ($self, $n) = @_;
	    my @n = @$n;
	    my @node;
	    while (@n) {
		my $prob = shift @n;
		my $rule = shift @n;
		push @node, { 'prob' => $prob, 'rule' => $self->transform_value($rule) };
	    }

	    confess "Can't build a Huffman tree with no nodes" unless @node > 0;

	    while (@node > 1) {
		@node = sort { $a->{'prob'} <=> $b->{'prob'} } @node;
		my $l = shift @node;
		my $r = shift @node;
		unshift @node, { 'prob' => $l->{'prob'} + $r->{'prob'}, 'rule' => undef, 'l' => $l, 'r' => $r };
	    }

	    return huff_to_rule ($node[0]);
	},

	# overload
	'overload' => sub {
	    my ($self, $n) = @_;
	    $n = forceHash ($n);
	    return ('rule' => [ 'overload' => [ map (($_ => $self->transform_value ($n->{$_})), qw(slow fast)) ] ] );
	},


	# text balloon
	'ball' => sub {
	    my ($self, $n) = @_;
	    $n = forceHash ($n);
	    return ("goal" => ["and" => ["lazy" => "",
					 "goal" => ["and" => ["lazy" => "",
							      "goal" => ["maybe" => ["prob" => $n->{'rate'}]],
							      "goal" => ["place" => ["balloon" => [ sortHash ($n, qw(rate pos color hexcolor text size ttl rise zoom fade)) ]]]]],
					 "goal" => ["false" => ""]]]);  # "and"ing result with 0 means this goal will persist & not automatically be deleted the first time the balloon is placed (this behavior can still be triggered by tweaking balloon pr
	}

    };
}

# helper to convert Huffman tree to random rule tree
sub huff_to_rule {
    my ($node) = @_;
    if (defined ($node->{'rule'})) {
	my $rule = $node->{'rule'};
	return ref($rule) eq 'HASH' ? %$rule : @$rule;
    }
    return ('rule' => [ 'random' => [ 'prob' => $node->{'l'}->{'prob'} / ($node->{'l'}->{'prob'} + $node->{'r'}->{'prob'}),
				      'pass' => [ huff_to_rule ($node->{'l'}) ],
				      'fail' => [ huff_to_rule ($node->{'r'}) ] ] ] );
}


# color helper
sub parse_color {
    my ($self, $n, $type, $tag, $mul) = @_;
    my @n = ref($n) eq 'ARRAY' ? @$n : %$n;
    my @col;
    while (@n) {
	my $k = shift @n;
	my $v = shift @n;
	if ($k eq $tag) {
	    $v = { 'inc' => $v } unless ref($v);
	    my $vhash = forceHash($v);
	    my ($var, $vmul, $inc) = map ($vhash->{$_}, qw(var mul inc));
	    push @col, 'colrule' => [ defined($var)
				      ? ('mask' => $self->getMask($type,$var),
					 'rshift' => $self->getShift($type,$var),
					 'hexmul' => hexv ($mul * (defined($vmul) ? $vmul : 1)))
				      : ('mask' => 0),
				      defined($inc) ? ('hexinc' => hexv($mul * $inc)) : () ];
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
sub declare_type {
    my ($self, $type, @varname_varbits) = @_;
    warn "Declaring particle '$type'\n" if $self->verbose;
    confess "'$type' is an illegal type name" if illegal_name($type);
    confess "Type '$type' is already defined" if exists $self->typeindex->{$type};
    $self->typeindex->{$type} = @{$self->type};
    push @{$self->type}, $type;
    my ($offset, $typeoffset) = (0, 0);  # offset of next var in varbits & typebits
    while (@varname_varbits) {
	my $varname = shift @varname_varbits;
	my $varbits = shift @varname_varbits;
	confess "In type $type, '$varname' is an illegal var name" if illegal_name($varname);
	push @{$self->pvar->{$type}}, $varname;
	$self->pvbits->{$type}->{$varname} = $varbits;
	$self->pvoffset->{$type}->{$varname} = $offset;
	$offset += $varbits;
	confess "More than ", $self->typeshift, " bits of vars for type $type" if $offset > $self->typeshift;
    }
}

# code to actually generate & print real XML from proto-XML

sub print_proto_xml {
    my ($self, $proto) = @_;

    if ($self->debug) {
	warn "Proto-XML passed to XML::Twig:\n", Data::Dumper->Dump($proto);
    }

    warn "Generating game XML...\n" if $self->verbose;
    my $xml = $self->generate_xml ($proto);

    warn "Validating generated game XML...\n" if $self->verbose;
    $self->validate_xml ($xml, $self->gameDTD);

    warn "Printing game XML...\n" if $self->verbose;
    print $xml;
}

sub validate_xml {
    my ($self, $xml, $dtd) = @_;
    my $xmllint = $self->xmllint;
    my $tmp = File::Temp->new();
    print $tmp $xml;
    my $lint = `$xmllint $dtd $tmp`;
    if ($lint =~ /\S/) {
	cluck "DTD validation errors";
	warn $lint;
    } else {
	warn "The XML is valid according to the DTD ($dtd)\n" if $self->verbose;
    }
}

sub generate_xml {
    my ($self, $proto) = @_;
    my $elt = new_XML_element(@$proto);
    my $twig = XML::Twig->new(pretty_print => 'indented');
    $twig->set_root($elt);
    return $twig->sprint;
}

sub compiler_warn {
    my ($self, $fmt, @args) = @_;
    my $warning = sprintf ($fmt, @args);
    my $nw = ++$self->compiler_warnings->{$fmt};
    if ($nw == 1) { warn "$warning\n" }
    elsif ($nw == 2) { warn "(suppressing further warnings of the form \"", substr($fmt,0,20), "...\")\n" }
}

sub assertLocTypeBound {
    my ($self, $loc, $var) = @_;
    if (defined($var) && $var ne "type" && $var ne "*") {
	if (!defined($self->scope->loctype->{$loc})) {
	    confess "In switch rule for ", $self->scope->type, ": Can't access $loc.$var because type of $loc is not bound\n";
	}
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

# fully-qualified particle state: .state => { '@tag' => tag, 'type' => name, 'varname1' => val1, 'varname2' => val2, ... }
sub typeAndVars {
    my ($self, $n) = @_;
    $n = forceHash($n);
    my $type = $n->{'type'};
    my $state = $self->getType($type) << $self->getShift($type,"type");
    while (my ($var, $val) = each %$n) {
	if ($var ne 'type') {
	    $state = $state | ($val << $self->getShift($type,$var));
	}
    }
    return $state;
};

sub getTypeAsHexState {
    my ($self, $type) = @_;
    return hexv($self->getType($type)).("0"x12);
}

sub getType {
    my ($self, $type) = @_;
    confess "Undefined type" unless defined $type;
    if ($type =~ /^\-?\d+$/) {
	return $type;
    }
    confess "Type '$type' unknown" unless defined $self->typeindex->{$type};
    return $self->typeindex->{$type};
}

sub getMask {
    my ($self, $type, $var) = @_;
    return 0 unless defined($var);
    return $self->statemask if $var eq "*";
    return $self->typemask if $var eq "type";
    confess "Undefined type" unless defined($type);
    confess "Type '$type' unknown" unless defined $self->typeindex->{$type};
    confess "Var '$var' unknown for type '$type'" unless defined $self->pvbits->{$type}->{$var};
    my $mask = ((1 << $self->pvbits->{$type}->{$var}) - 1) << $self->pvoffset->{$type}->{$var};
    return hexv($mask);
}

sub getShift {
    my ($self, $type, $var) = @_;
    return 0 unless defined($var);
    return $self->typeshift if $var eq "type";
    return 0 if $var eq "*";
    confess "Undefined type" unless defined($type);
    confess "Type '$type' unknown" unless defined $self->typeindex->{$type};
    confess "Var '$var' unknown for type '$type'" unless defined $self->pvbits->{$type}->{$var};
    return $self->pvoffset->{$type}->{$var};
}

sub hexv {
    my ($val) = @_;
    confess "Undefined value" unless defined $val;
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
    my ($hashRef, @key_order) = @_;
    if (@key_order) {
	return map (exists($hashRef->{$_}) ? ($_ => $hashRef->{$_}) : (), @key_order);
    }
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


# helper to find file relative to pixelzoo directory
sub pixelzoo_dir {
    my ($file) = @_;
    my $dir;
    for my $inc (@INC) {
	if (-e $inc . '/' . __PACKAGE__ . '.pm') {
	    $dir = $inc . '/..';
	    last;
	}
    }
    if (defined($dir) && defined($file)) {
	$dir .= '/' . $file;
    }
    return $dir;
}

1;
