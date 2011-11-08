#!/usr/bin/perl -w

package Grammar;

use strict;
use vars ('@ISA', '@EXPORT', '@EXPORT_OK');

use Exporter;
use Carp qw(carp croak cluck confess);
use Data::Dumper;
use File::Temp;
use Scalar::Util;
use IPC::Open3;
use Symbol qw(gensym);

use AutoHash;
use Twiggy;

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
	'playerID' => 0,
	'ownerID' => 0,

# XML
	'xml' => AutoHash->new ('game' => [],   # allow designer/tester to override entire top-level Game structure (almost certainly not advisable)
				'goal' => [],   # allow designer/tester to override default Goal structure
				'game_stash' => [],   # allow web service to populate the Game structure
				'board_stash' => [],   # allow web service to populate the Board structure
				'init' => [],   # allow designer/tester/webservice to initialize Board
				'seed' => [],   # allow designer/tester/webservice to initialize Board random number generator
				'tool' => [],   # to add tools, use addTool helper
				'type' => []),  # to add types, use addType helper

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

	'xmllint' => 'xmllint',
	'xmllint_opts' => '--noout --dtdvalid',
	'gameDTD' => pixelzoo_dir('dtd/game.dtd'),
	'protoDTD' => pixelzoo_dir('dtd/proto.dtd'),
	'protofile' => undef,
	'sane_protofile' => undef,
	'outfile' => undef,

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
		    'vars' => [ 'hue' => 6, 'saturation' => 3, 'brightness' => 3 ],
		    'hue' => [ 'var' => 'hue' ],
		    'sat' => [ 'var' => 'saturation' ],
		    'bri' => [ 'var' => 'brightness' ],
		    'rate' => 0,
		    'rule' => ['nop']);

    return $self;
}


# new type helper
sub addType {
    my ($self, @type_proto_xml) = @_;
    push @{$self->xml->type}, 'particle' => \@type_proto_xml;
}

# new tool helper
sub addTool {
    my ($self, @tool_proto_xml) = @_;
    push @{$self->xml->tool}, 'tool' => [ sortHash ({@tool_proto_xml}, $self->toolArgs) ];
}

sub toolArgs {
    return qw(name size brush state hexstate pstate ostate gstate gtype gvars overwrite spray reserve recharge maxreserve hide);
}

sub balloonArgs {
    return qw(rate pos color hexcolor text size ttl rise zoom fade);
}

# generate the "proto-game XML", i.e. a hashref representing prototype Game XML
sub proto_xml {
    my ($self) = @_;

    warn "Parsing types...\n" if $self->verbose;
    $self->parse_types;

    warn "Generating proto-game XML...\n" if $self->verbose;
    my $game_proto = $self->make_game;
    my $wrapped_proto = ["game" => $game_proto];

    if ($self->debug) {
	warn "Data structure representing proto-game XML:\n", Data::Dumper->Dump($wrapped_proto);
    }

    return $wrapped_proto;
}

# generate the "proto-game XML" as an actual XML string
sub assembled_xml {
    my ($self) = @_;
    my $proto_xml = $self->proto_xml;
    my $xml = $self->generate_xml ($proto_xml);
    return $xml;
}

# validate the proto-game XML
sub validate_proto_xml {
    my ($self, $proto_xml) = @_;
    $proto_xml = $self->proto_xml unless defined $proto_xml;

    if (defined $self->protofile) {
	warn "Generating proto-game XML to save to text file...\n" if $self->verbose;

	my $proto_xml_text = $self->generate_xml($proto_xml);

	warn "Saving proto-game XML to file...\n" if $self->verbose;
	local *PROTO;
	open PROTO, '>' . $self->protofile;
	print PROTO $proto_xml_text;
	close PROTO;
    }

    # "Sanitize" the proto-game XML, i.e. remove the quick-and-dirty shortcuts that don't translate well to XML
    # (such as using var names as hash keys).
    # Not sure this is the wisest idea if we do eventually plan to load XML back in...
    # at minimum, we would need to write an inverse transformation to go the other way.
    # We'd also need to avoid clashes between the unsanitized and sanitized tag names.
    warn "Sanitizing proto-game XML...\n" if $self->verbose;
    my $sanitized_proto = $self->sanitize_proto($proto_xml);

    if ($self->debug) {
	warn "Data structure representing proto-game XML:\n", Data::Dumper->Dump($proto_xml);
    }

    if ($self->debug) {
	warn "Data structure representing sanitized proto-game XML:\n", Data::Dumper->Dump($sanitized_proto);
    }

    warn "Generating sanitized proto-game XML for validation...\n" if $self->verbose;
    my $sanitized_proto_xml = $self->generate_xml($sanitized_proto);

    if (defined $self->sane_protofile) {
	warn "Saving sanitized proto-game XML to file...\n" if $self->verbose;
	local *SANE;
	open SANE, '>' . $self->sane_protofile;
	print SANE $sanitized_proto_xml;
	close SANE;
    }

    warn "Validating sanitized proto-game XML...\n" if $self->verbose;
    if (defined $self->sane_protofile) {
	$self->validate_xml_file ($self->sane_protofile, $self->protoDTD);
    } else {
	$self->validate_xml_string ($sanitized_proto_xml, $self->protoDTD);
    }
}

sub compiled_proto_xml {
    my ($self, $proto_xml) = @_;
    $proto_xml = $self->proto_xml unless defined $proto_xml;

    $self->validate_proto_xml ($proto_xml);

    warn "Compiling proto-game XML...\n" if $self->verbose;
    my $transformed_proto = $self->transform_proto ($proto_xml);

    if ($self->debug) {
	warn "Data structure representing game XML:\n", Data::Dumper->Dump($transformed_proto);
    }

    return $transformed_proto;
}

sub compiled_xml {
    my ($self, $compiled_proto_xml) = @_;
    $compiled_proto_xml = $self->compiled_proto_xml unless defined $compiled_proto_xml;

    warn "Generating game XML...\n" if $self->verbose;
    my $compiled_xml = $self->generate_xml ($compiled_proto_xml);

    if (defined $self->outfile) {
	warn "Saving game XML...\n" if $self->verbose;
	local *OUT;
	open OUT, ">" . $self->outfile or die "Couldn't open ", $self->outfile, ": $!";
	print OUT $compiled_xml;
	close OUT or die "Couldn't close ", $self->outfile, ": $!";
    }

    warn "Validating generated game XML...\n" if $self->verbose;
    if (defined $self->outfile) {
	$self->validate_xml_file ($self->outfile, $self->gameDTD);
    } else {
	$self->validate_xml_string ($compiled_xml, $self->gameDTD);
    }

    return $compiled_xml;
}

# top-level method to generate, compile & print XML
sub print {
    my ($self, $compiled_proto_xml) = @_;

    my $compiled_xml = $self->compiled_xml ($compiled_proto_xml);

    if (!defined $self->outfile) {
	warn "Printing game XML...\n" if $self->verbose;
	print $compiled_xml;
    } else {
	warn "Game XML printed to ", $self->outfile, "\n";
    }
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
			    @{$self->xml->board_stash},
			    @{$self->xml->seed},
			    $self->make_exit_init,
			    @{$self->xml->init}],

		"rate" => $self->boardRate,

		@{$self->xml->tool},

		$self->make_exit,

		@{$self->xml->game_stash},

		@{$self->xml->goal} > 0
		? @{$self->xml->goal}   # allow designer to override default Goal structure
		: $self->make_goal,

	);

    return \@game;
}

# parse_types: parse type declarations
sub parse_types {
    my ($self, $proto_xml) = @_;
    if (defined $proto_xml) {
	$self->parse_node ($proto_xml);
    } else {
	$self->parse_node ($self->xml->type);
    }
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
#	warn "Parsing $node";
    }
    while (@node) {
	my $k = shift @node;
	my $v = shift @node;
	if ($k eq 'particle') {
	    my $vhash = forceHash($v);
	    $self->declare_type ($vhash->{'name'}, defined($vhash->{'vars'}) ? %{forceHash($vhash->{'vars'})} : ());
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
    my $trans = $self->trans;

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
	    push @kv_out, $k, defined($v) ? $self->transform_value($v) : ();
	}
    }

    return @kv_out;
}

# transform a single value
sub transform_value {
    my ($self, $v) = @_;

    if (!defined($v)) {
	cluck "Undefined value passed to transform_value";
    }
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
	if (@$n % 2 != 0) {
	    confess "Odd number of elements in array, can't convert to hash: (@$n)";
	}
	return {@$n};
    }
    confess "Not a reference";
}

# helper to sanitize the proto-XML
# hack: temporarily overrides the transformation functions used for the main XML tree transformation
sub sanitize_proto {
    my ($self, $proto) = @_;
    my $old_trans = $self->trans;
    $self->trans ({ 'switch' => switch_sanitizer('zswitch','state',1),
		    'bind' => switch_sanitizer('zbind','type',0),
		    'huff' => \&sanitize_huff,
		    'gvars' => typevar_sanitizer('zgvars','setvar','value'),
		    'vars' => typevar_sanitizer('zvars','varsize','size'),
		    'set' => typevar_sanitizer('zset','setvar','value'),
		    map (($_ => hsv_sanitizer("z$_")), qw(hue sat bri)),
		  });

    my $sanitized_proto = $self->transform_value ($proto);
    $self->trans ($old_trans);  # restore the old transformations
    return $sanitized_proto;
}

# helpers to perform, or make subroutines that perform, sanitization transformations
sub sanitize_huff {
    my ($self, $n) = @_;
    my @vars = ref($n) eq 'HASH' ? %$n : @$n;
    my @out;
    while (@vars) {
	my $k = shift @vars;
	my $v = shift @vars;
	push @out, ('numeric' => [ '@value' => $k, @{$self->transform_value($v)} ]);
    }
    return ('zhuff' => \@out);
}

sub switch_sanitizer {
    my ($switch_tag, $case_tag, $has_var) = @_;
    return sub {
	my ($self, $n) = @_;
	$n = forceHash($n);
	my ($locid, $varid, $case, $default) = map ($n->{$_}, qw(loc var case default));
	my %case;
	if (ref($case) eq 'HASH') { %case = %$case }
	elsif (ref($case) eq 'ARRAY') { confess "Odd number of elements in (@$case)" if @$case % 2 != 0; %case = @$case }
	else { confess "Not a hash or array ref" }
	return ($switch_tag => [defined($locid) ? ('loc' => $locid) : (),
				$has_var && defined($varid) ? ('var' => $varid) : (),
				defined($case) ? (map (( 'case' => [ '@'.$case_tag => $_, @{$self->transform_value($case{$_})} ] ), keys %case) ) : (),
				defined($default) ? ('default' => $self->transform_value($default)) : ()]);
    };
}

sub typevar_sanitizer {
    my ($vars_tag, $var_tag, $value_tag) = @_;
    return sub {
	    my ($self, $n) = @_;
	    confess "Expected a {Type=>Value} hash/array reference" unless ref($n);
	    my @vars = ref($n) eq 'HASH' ? %$n : @$n;
	    my @out;
	    while (@vars) {
		my $k = shift @vars;
		my $v = shift @vars;
		if ($k eq 'type') {
		    push @out, ($k => $v);
		} else {
		    push @out, ($var_tag => { 'name' => $k, $value_tag => $v });
		}
	    }
	    return ($vars_tag => \@out);
    };
}

sub hsv_sanitizer {
    my ($tag) = @_;
    return sub {
	    my ($self, $n) = @_;
	    if (ref($n)) {
		return ($tag => $n);
	    }
	    return ($tag => [ 'add' => $n ]);
    };
}

# primary transformation functions
sub transform_hash {

    # main hash
    return {
	'gtype' => sub { my ($self, $n) = @_; return ('type' => $self->getType($n)) },
	'gstate' => sub { my ($self, $n) = @_; return ('hexstate' => $self->getTypeAsHexState($n)) },
	'pstate' => sub { my ($self, $n) = @_; return ('hexstate' => hexv($self->userState($n,$self->playerID))) },
	'ostate' => sub { my ($self, $n) = @_; return ('hexstate' => hexv($self->userState($n,$self->ownerID))) },
	'gvars' => sub { my ($self, $n) = @_; $n = forceHash($n); return ($n->{'@tag'} || 'hexstate', hexv($self->typeAndVars($n))) },
	'tmask' => sub { my ($self, $n) = @_; return ('mask', $self->getMask($n,'type')) },
	'vmask' => sub { my ($self, $n) = @_; $n = forceHash($n); return ($n->{'@tag'} || 'mask', $self->getMask($n->{'type'},$n->{'var'})) },
	'vshift' => sub { my ($self, $n) = @_; $n = forceHash($n); return ($n->{'@tag'} || 'rshift', $self->getShift($n->{'type'},$n->{'var'})) },

	# now come the cunning transformations...
	# recursive transformations (for rules) that call transform_list or transform_value on their subtrees
	# aided & abetted by expansion into further-expanded (but simpler) macros of the above form (gtype, tmask, etc)

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

	    return ('tool' => [ $self->transform_list (sortHash ($n, $self->toolArgs)) ]);
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
	    my $transformed_default = defined($default) ? $self->transform_value($default) : undef;

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
	    $locid = $self->origin unless defined $locid;
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
	    my $transformed_default = defined($default) ? $self->transform_value($default) : undef;
	    
	    return ('rule' => [ 'switch' => [ 'pos' => { 'x' => $x, 'y' => $y },
					      'mask' => $self->getMask($loctype,$varid),
					      'rshift' => $self->getShift($loctype,$varid),
					      map (('case' => ['state' => $_,
							       @{$transformed_case{$_}}]),
						   keys %transformed_case),
					      defined($default) ? ('default' => $transformed_default) : () ] ] );
	},


	# modify
	'modify' => sub {
	    my ($self, $n) = @_;
	    $n = forceHash ($n);

	    my ($src, $dest, $set, $inc, $next) = map ($n->{$_}, qw(src dest set inc next));
	    confess "In modify: can't specify 'set' together with 'src' or 'inc'" if defined($set) && (defined($src) || defined($inc));
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
	'nop' => \&make_nop,

	# random (build a Huffman tree)
	'huff' => sub {
	    my ($self, $n) = @_;
	    my @n = @$n;
	    my @node;
	    my $total = 0;
	    while (@n) {
		my $prob = shift @n;
		my $rule = shift @n;
		push @node, { 'prob' => $prob, 'rule' => $self->transform_value($rule) };
		$total += $prob;
	    }

	    if ($total < 1) {
		push @node, { 'prob' => 1 - $total, 'rule' => [ make_nop() ] };
	    } elsif ($total > 1) {
		grep ($_->{'prob'} /= $total, @node);
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
							      "goal" => ["place" => ["balloon" => [ sortHash ($n, $self->balloonArgs) ]]]]],
					 "goal" => ["false" => ""]]]);  # "and"ing result with 0 means this goal will persist & not automatically be deleted the first time the balloon is placed
	}

    };
}

# nop rule
# first the actual code for 'nop'
sub make_nop { return ('rule' => [ 'modify' => [ 'destmask' => 0 ] ]); }
# now the mnemonic
sub nop { return [ 'nop' => '' ] }

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
	    $v = { 'add' => $v } unless ref($v);
	    my $vhash = forceHash($v);
	    my ($var, $vmul, $inc) = map ($vhash->{$_}, qw(var mul add));
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

sub validate_xml_string {
    my ($self, $xml, $dtd) = @_;
    my $tmp = File::Temp->new();
    print $tmp $xml;
    my $valid = $self->validate_xml_file ($tmp->filename, $dtd);
    $tmp->unlink_on_destroy(0);
    return $valid;
}

sub validate_xml_file {
    my ($self, $filename, $dtd) = @_;
    my $xmllint = $self->xmllint . ' ' . $self->xmllint_opts;

    my $pid = open3(gensym, ">&STDERR", \*PH, "$xmllint $dtd $filename");
    my @lint = <PH>;
    waitpid($pid, 0);

    if (@lint) {
	carp "DTD validation errors";
	warn @lint;
	return 0;
    }

    warn "Validated DTD: $dtd\n" if $self->verbose;
    return 1;
}

sub generate_xml {
    my ($self, $proto) = @_;
    my $elt = new_XML_element(@$proto);
    my $twig = Twiggy->new(pretty_print => 'indented');
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

sub userState {
    my ($self, $type, $userID) = @_;
    my $shift = $self->getShift($type,"id");
    my $state = $self->getType($type) << $shift;
    $state = $state | ($userID & ((1 << $shift) - 1));
    return $state;
};

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

    my $t = Twiggy::Elt->new($gi);

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
	    } elsif ($k =~ /^([^\@+])\@/) {
		my $new_k = $1;
		my $elt = new_XML_element($new_k, $v);
		while ($k =~ /\@([^=]+)=([^\@=]+)/g) {
		    my ($attr, $val) = ($1, $2);
		    $elt->set_att ($attr, $val);
		}
		$elt->paste(last_child => $t);
	    } elsif (Scalar::Util::looks_like_number($k)) {
		my $elt = new_XML_element('numeric', $v);
		$elt->set_att ('value' => $k);
		$elt->paste(last_child => $t);
	    } else {
		my $elt = new_XML_element($k, $v);
		$elt->paste(last_child => $t);
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
