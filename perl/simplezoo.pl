#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use XML::Twig;
use Carp;

use FindBin qw($Bin);
use lib "$Bin/../Zoo/lib";

use Level;

# parse options
my $man = 0;
my $help = 0;
my $debug = 0;
my $verbose = 0;
my $xmllint;
my ($proto, $out);

GetOptions('help|?' => \$help, man => \$man, verbose => \$verbose, debug => \$debug, 'out=s' => \$out, 'proto=s' => \$proto, 'xmllint=s' => \$xmllint) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

# pod2usage(2) unless @ARGV == 1;

$verbose = 1 if $debug;


# test
my $gram = Level->newLevel;
$gram->verbose($verbose);
$gram->debug($debug);
$gram->protofile($proto) if defined $proto;
$gram->outfile($out) if defined $proto;
$gram->xmllint($xmllint) if defined $xmllint;

# add some stuff

# cement
my %cement = ('name' => 'cement',
	      'rate' => .1,
	      'step' => .02,
	      'drain' => .001,
	      'stick' => 1,
	      'sticksto' => 'wall',
	      'copies' => 1,
	      'set' => .02,
	      'setsto' => 'wall',
	      'setsvar' => 'hue',
	      'setsvals' => [0,42,84]);

make_spray_tool(%cement);

sub make_spray_tool {
    my %cement = @_;

    my ($cementName, $cementRate, $cementStep, $cementDrain, $cementStick, $cementSet, $stickType, $copyFlag, $setType, $wallVar, $wallVals)
	= map ($cement{$_}, qw(name rate step drain stick set sticksto copies setsto setsvar setsvals));

    # cement type
    my $setSub = ['huff' => [ map ((1/@$wallVals => [ 'modify' => [ 'set' => [ 'type' => $setType, $gram->var($wallVar) => $_ ] ] ]),
				   @$wallVals) ]];
    my $stickSub = $copyFlag
	? ['modify' => [ 'src' => [ 'loc' => $gram->neighbor ], 'dest' => [ 'loc' => $gram->origin ] ] ]
	: $setSub;

    $gram->addType ('name' => $cementName,
		    'hue' => ['add' => 32],
		    'sat' => ['add' => 192],
		    'bri' => ['add' => 96],
		    'rate' => $cementRate,
		    'rule' => ['huff' => [$cementDrain => $gram->suicide,
					  $cementSet => $setSub,
					  $gram->bindMoore (1 - $cementSet - $cementDrain,
							    [$gram->target($gram->empty) => ['huff' => [$cementStep => $gram->moveTo]],
							     $gram->target($stickType) => [ 'huff' => [$cementStick => $stickSub]]])]]);
    
    # cement tool
    $gram->addTool ('name' => "$cementName spray",
		    'size' => 2,
		    'gstate' => $cementName,
		    'reserve' => 1000,
		    'recharge' => 100,
		    'spray' => 1000,
		    'overwrite' => [ 'gstate' => 'empty' ]);
}

# wall
my ($wallRate, $wallMaxDecay) = (.0002,
				 15);
$gram->addType ('name' => 'wall',
		'vars' => [ $gram->var('hue') => 8, $gram->var('decay') => 4 ],
		'hue' => [ 'var' => 'hue' ],
		'sat' => ['add' => 32],
		'bri' => [ 'var' => 'decay', 'mul' => -8, 'add' => 255 ],
		'rate' => $wallRate,
		'rule' => ['switch' => ['loc' => $gram->origin,
					'var' => 'decay',
					'case' => [ $gram->target($wallMaxDecay) => $gram->suicide ($gram->balloon ("decay", 'rate' => .01, 'hexcolor' => "20ffff"))],
					'default' => [ 'modify' => [ 'src' => [ 'var' => 'decay' ],
								     'inc' => 1 ]]]]);


# acid
my ($acidRate, $acidDrain, $acidBurn) = (.1, .03, .5);
$gram->addType ('name' => 'acid',
		'hue' => ['add' => 80],
		'sat' => ['add' => 192],
		'bri' => ['add' => 64],
		'rate' => $acidRate,
		'rule' => ['huff' => [$acidDrain => $gram->suicide,
				       $gram->bindMoore (1 - $acidDrain,
							 [$gram->target($gram->empty) => $gram->moveTo],
							 ['huff' => [ $acidBurn => $gram->homicide ($gram->suicide) ] ])]]);

# acid tool
$gram->addTool ('name' => 'Acid spray',
		'size' => 16,
		'gstate' => 'acid',
		'reserve' => 1000,
		'recharge' => 100,
		'spray' => 2,
		'overwrite' => [ 'gstate' => 'empty' ]);

# plant
my %plant = ('rate' => .01, 'branch' => .2, 'die' => .0001, 'max_branches' => 3);
my $no_branch = ['modify' => ['src' => ['loc' => $gram->neighbor, 'var' => 'gens_left'],
			      'inc' => -1,
			      'dest' => ['loc' => $gram->neighbor, 'var' => 'gens_left' ]]];
$gram->addType ('name' => 'plant',
		'vars' => [ $gram->var('gens_left') => 3, $gram->var('branches') => 2 ],
		'hue' => ['var' => 'gens_left', 'mul' => +8, 'add' => 82],
		'sat' => ['add' => 240],
		'bri' =>  ['var' => 'gens_left', 'mul' => -16, 'add' => 144],
		'rate' => $plant{rate},
		'rule' => ['huff' => [$plant{'die'} => $gram->suicide,
				      (1 - $plant{'die'}) => ['switch' => ['loc' => $gram->origin,
									   'var' => 'gens_left',
									   'case' => { $gram->target('0') => $gram->nop },
									   'default' => $gram->huffNeumann
									   ({ $gram->target($gram->empty) => $gram->copyTo
										  ($gram->neighbor,
										   ['switch' => ['loc' => $gram->origin,
												 'var' => 'branches',
												 'case' => { $gram->target($plant{'max_branches'}) => $no_branch },
												 'default' => ['huff' => [(1-$plant{'branch'}) => $no_branch,
															  $plant{'branch'} => ['modify' => ['src' => ['loc' => $gram->origin, 'var' => 'branches'],
																			    'inc' => +1,
																			    'dest' => ['loc' => $gram->origin, 'var' => 'branches' ]]] ]]]]) }) ] ] ] ] );

my %seed = ('name' => 'seed',
	    'rate' => .1,
	    'step' => .05,
	    'drain' => .03,
	    'stick' => 1,
	    'sticksto' => 'wall',
	    'copies' => 0,
	    'set' => .0005,
	    'setsto' => 'plant',
	    'setsvar' => 'gens_left',
	    'setsvals' => [1,3,6]);

make_spray_tool(%seed);


# rock-paper-scissors animal
sub make_species_switch {
    my ($gram, $selfRule, $predatorRule, $preyRule) = @_;
    ($selfRule, $predatorRule, $preyRule) = map (defined($_) ? (ref($_) ? $_ : []) : $gram->nop, $selfRule, $predatorRule, $preyRule);
    my %sw;
    for my $orig (0..2) {
	my $prey = ($orig + 1) % 3;
	for my $nbr (0..2) {
	    $sw{$orig}->{$gram->target($nbr)} =
		($nbr == $orig)
		? $selfRule
		: (($nbr == $prey)
		   ? $predatorRule
		   : $preyRule);
	}
    }
    return ('switch' => ['loc' => $gram->origin,
			 'var' => 'species',
			 'case' => [map (( $gram->target($_) => ['switch' => ['loc' => $gram->neighbor,
									      'var' => 'species',
									      'case' => [%{$sw{$_}}]]] ),
					 0..2)]]);
}

my %rps = ('name' => 'cyclobs',
	   'rate' => .08,
	   'food' => 'plant',
	   'food_unripeness_var' => 'gens_left',

	   # if next to empty space...
	   'step' => .2,
	   'breed' => .005,
	   'die' => .005,

	   # if next to same species...
	   'choke' => .003,

	   # if next to prey species, or food...
	   'eat' => .2,
	   'convert' => .8,

	   # text feedback rates
	   'text' => .001,
	   'log' => 1);

my $stepOrBreed = $rps{'breed'} + $rps{'step'};
my $eatOrConvert = $rps{'convert'} + $rps{'eat'};
$gram->addType ('name' => $rps{'name'},
		'vars' => [ $gram->var('species') => 2 ],
		'hue' => [ 'var' => 'species', 'mul' => 42, 'add' => 10 ],
		'sat' => ['add' => 255],
		'bri' => ['add' => 255],
		'rate' => $rps{'rate'},
		'rule' =>
		['switch' => ['loc' => $gram->origin,
			      'var' => 'species',
			      'case' => [$gram->target(3) => ['huff' => [map ((1/3 => ['modify' => ['inc' => $_,
												    'dest' => ['var' => 'species']]]),
							       0..2)]]],
			      'default' => ['huff' => [ $gram->bindNeumann
							(1,
							 [ $gram->target($gram->empty) => ['huff' => [$rps{'die'} => $gram->suicide,
												      $stepOrBreed => $gram->moveOrSpawnTo ($rps{'breed'} / $stepOrBreed,
															     $gram->neighbor,
															     $gram->balloon("step",'rate'=>$rps{'text'}),
															     $gram->balloon("breed",'rate'=>$rps{'text'}))]],
							   $gram->target($rps{'food'}) => ['switch' => ['loc' => $gram->neighbor,
													'var' => $rps{'food_unripeness_var'},
													'case' => { $gram->target(0) => [ 'huff' => [ $eatOrConvert => $gram->moveOrSpawnTo ($rps{'convert'} / $eatOrConvert,
																							     $gram->neighbor,
																							     $gram->balloon("eat",'rate'=>$rps{'text'}),
																							     $gram->balloon("spawn",'rate'=>$rps{'text'})) ] ],
												     $gram->target(1) => [ 'huff' => [ $rps{'eat'} => $gram->moveTo ] ] }]],
							   
							   $gram->target($rps{'name'}) => [ make_species_switch
									     ($gram,
									      [ 'huff' => [ $rps{'choke'} => $gram->suicide ] ],
									      [ 'huff' => [ $eatOrConvert => $gram->moveOrSpawnTo ($rps{'convert'} / $eatOrConvert,
																   $gram->neighbor,
																   $gram->balloon("prey",'rate'=>$rps{'text'}),
																   $gram->balloon("0wn",'rate'=>$rps{'text'})) ]]) ] ])]]]]);

# rps animal tool
$gram->addTool ('name' => $rps{'name'},
		'size' => 8,
		'gvars' => [ 'type' => $rps{'name'}, $gram->var('species') => 3 ],
		'reserve' => 5,
		'recharge' => 100,
		'spray' => 100,
		'overwrite' => [ 'gstate' => 'empty' ]);

# perfume
my ($perfumeRate, $perfumeDrain, $perfumeBillow, $perfumeInduce) = (.6, .03, .03, .5);
$gram->addType ('name' => 'perfume',
		'hue' => ['add' => 192],
		'sat' => ['add' => 192],
		'bri' => ['add' => 96],
		'rate' => $perfumeRate,
		'rule' => ['huff' => [$perfumeDrain => $gram->suicide,
				      $gram->bindMoore (1 - $perfumeDrain,
							  [$gram->target($gram->empty) => $gram->moveOrSpawnTo ($perfumeBillow),
							   $gram->target($rps{'name'}) => ['huff' => [ $perfumeInduce => $gram->copyFromTo ($gram->neighbor, $gram->origin) ] ] ])]]);

# perfume tool
$gram->addTool ('name' => 'Perfume spray',
		'size' => 4,
		'gstate' => 'perfume',
		'reserve' => 1000,
		'recharge' => 100,
		'spray' => 2500,
		'overwrite' => [ 'gstate' => 'empty' ]);

# print
$gram->print;


__END__

=head1 NAME
simplezoo.pl - generate a PixelZoo XML grammar

=head1 SYNOPSIS

zoocompiler.pl [options]

 Options:
  -help               brief help message
  -man                full documentation
  -proto              file to save proto-XML
  -out                file to save output XML (otherwise sent to stdout)
  -xmllint            path to xmllint
  -verbose            report progress
  -debug              more info than you want

=head1 OPTIONS

=over 8

=item B<-help>

Prints a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=item B<-xmllint>

Specify the path to xmllint.

=item B<-proto>

Specify a filename to save the proto-XML.

=item B<-out>

Specify a filename to save the output XML.
If no filename is specified, it will be printed to standard output.

=back

=head1 DESCRIPTION

B<This program> will generate a PixelZoo XML grammar.

=cut
