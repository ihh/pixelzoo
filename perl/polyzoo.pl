#!/usr/bin/env perl -w

use strict;

use FindBin qw($Bin);
use lib "$Bin/../Zoo/lib";

use Getopt::Long;
use Pod::Usage;
use XML::Twig;
use Carp;

use Level::Simple;

# parse options
my $man = 0;
my $help = 0;
my $debug = 0;
my $verbose = 0;
my $xmllint;
my ($proto, $out);

GetOptions('help|?' => \$help, man => \$man, verbose => \$verbose, debug => \$debug,
	   'out=s' => \$out, 'proto=s' => \$proto, 'xmllint=s' => \$xmllint) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

# pod2usage(2) unless @ARGV == 1;

$verbose = 1 if $debug;


# test
my $gram = Level::Simple->newLevel;
$gram->verbose($verbose);
$gram->debug($debug);
$gram->protofile($proto) if defined $proto;
$gram->outfile($out) if defined $proto;
$gram->xmllint($xmllint) if defined $xmllint;

# polymer particle
my ($polyName, $polyRate) = ('polymer', .1);
my $cageLen = 10;
$gram->add_polymer_loop_builder ($polyName, $polyRate, $gram->moore_xy, $gram->moore_xy, 4, $gram->moore_right_angle, $cageLen);

# polymer tool
$gram->addTool ('name' => 'Polymer spray',
		'size' => 4,
		'gvars' => [ 'type' => $polyName, $gram->var('r_dir') => 2, $gram->var('edge_len') => $cageLen, $gram->var('state') => 1 ],
		'reserve' => 1,
		'recharge' => 100,
		'spray' => 2500,
		'overwrite' => [ 'gstate' => 'empty' ]);

# guest
my ($guest_name, $guest_rate, $guest_run_length) = ("guest", .1, 20);
$gram->add_guest ($guest_name, $guest_rate, $gram->neumann_xy, 1, $guest_run_length);

$gram->addTool ('name' => $guest_name,
		'size' => 8,
		'gstate' => $guest_name,
		'reserve' => 5,
		'recharge' => 100,
		'spray' => 100,
		'overwrite' => [ 'gstate' => 'empty' ]);

# add the simpletest stuff

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

$gram->make_spray_tool(%cement);


# wall
my ($wallRate, $wallMaxDecay) = (.0002, 15);
$gram->make_wall ($wallRate, $wallMaxDecay);

# acid
my ($acidRate, $acidDrain, $acidBurn) = (.1, .03, .5);
$gram->make_acid($acidRate, $acidDrain, $acidBurn);

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
$gram->make_plant (%plant);

# seed
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

$gram->make_spray_tool(%seed);


# rock-paper-scissors animal

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

$gram->make_rps_animal (%rps);

# rps animal tool
$gram->addTool ('name' => $rps{'name'},
		'size' => 8,
		'gvars' => [ 'type' => $rps{'name'}, $gram->var('species') => 3 ],
		'reserve' => 5,
		'recharge' => 100,
		'spray' => 100,
		'overwrite' => [ 'gstate' => 'empty' ]);

# perfume
my ($animalName, $perfumeRate, $perfumeDrain, $perfumeBillow, $perfumeInduce) = ($rps{'name'}, .6, .03, .03, .5);
$gram->make_perfume ($animalName, $perfumeRate, $perfumeDrain, $perfumeBillow, $perfumeInduce);

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
polyzoo.pl - generate a PixelZoo XML grammar

=head1 SYNOPSIS

polyzoo.pl [options]

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
