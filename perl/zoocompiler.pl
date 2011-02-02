#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use XML::Twig;
use Carp;

use FindBin qw($Bin); 
use lib $Bin;

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
my ($cementRate, $cementStep, $cementDrain, $cementStick, $cementSet) = (.1, .02, .001, 1, .02);
my @wallHue = (0, 42, 84);
$gram->addType ('name' => 'cement',
		'hue' => 32,
		'sat' => 192,
		'bri' => 96,
		'rate' => $cementRate,
		'rule' => ['huff' => [$cementDrain => $gram->suicide,
				      map (($cementSet/@wallHue => [ 'modify' => [ 'set' => [ 'type' => 'wall',
											       'decay' => 0,
											       'hue' => $_ ]]]),
					    @wallHue),
				       $gram->bindNeumann (1 - $cementSet - $cementDrain,
							   [$gram->empty => ['huff' => [$cementStep => $gram->moveTo]],
							    'wall' => [ 'huff' => [$cementStick => ['modify' => [ 'src' => [ 'loc' => $gram->neighbor ],
														  'dest' => [ 'loc' => $gram->origin ] ] ]]]])]]);

# cement tool
$gram->addTool ('name' => 'Cement spray',
		'size' => 2,
		'gstate' => 'cement',
		'reserve' => 1000,
		'recharge' => 100,
		'spray' => 1000,
		'overwrite' => [ 'gstate' => 'empty' ]);

# wall
my ($wallRate, $wallMaxDecay) = (.0002,
				 15);
$gram->addType ('name' => 'wall',
		'vars' => [ 'hue' => 8, 'decay' => 4 ],
		'hue' => [ 'var' => 'hue' ],
		'sat' => 32,
		'bri' => [ 'var' => 'decay', 'mul' => -8, 'add' => 255 ],
		'rate' => $wallRate,
		'rule' => ['switch' => ['loc' => $gram->origin,
					'var' => 'decay',
					'case' => [ $wallMaxDecay => $gram->suicide ($gram->balloon ("decay", 'rate' => .01, 'hexcolor' => "20ffff"))],
					'default' => [ 'modify' => [ 'src' => [ 'var' => 'decay' ],
								     'inc' => 1 ]]]]);


# acid
my ($acidRate, $acidDrain, $acidBurn) = (.1, .03, .5);
$gram->addType ('name' => 'acid',
		'hue' => 80,
		'sat' => 192,
		'bri' => 64,
		'rate' => $acidRate,
		'rule' => ['huff' => [$acidDrain => $gram->suicide,
				       $gram->bindNeumann (1 - $acidDrain,
							   [$gram->empty => $gram->moveTo],
							   ['huff' => [ $acidBurn => $gram->homicide ($gram->suicide) ] ])]]);

# acid tool
$gram->addTool ('name' => 'Acid spray',
		'size' => 16,
		'gstate' => 'acid',
		'reserve' => 1000,
		'recharge' => 100,
		'spray' => 2,
		'overwrite' => [ 'gstate' => 'empty' ]);

# rock-paper-scissors animal
sub make_species_switch {
    my ($gram, $selfRule, $predatorRule, $preyRule) = @_;
    ($selfRule, $predatorRule, $preyRule) = map (defined($_) ? (ref($_) ? $_ : []) : ['nop'], $selfRule, $predatorRule, $preyRule);
    my %sw;
    for my $orig (0..2) {
	for my $nbr (0..2) {
	    $sw{$orig}->{$nbr} =
		$nbr == $orig
		? $selfRule
		: ((($nbr + 1) % 3 == $orig)
		   ? $predatorRule
		   : $preyRule);
	}
    }
    return ('switch' => ['loc' => $gram->origin,
			 'var' => 'species',
			 map (('case' => [ $_ => ['switch' => ['loc' => $gram->neighbor,
							       'var' => 'species',
							       'case' => [%{$sw{$_}}]]]]),
			      0..2)]);
}

my %rps = ('name' => 'cyclobs',
	   'rate' => 10* .03,  # debug
	   'step' => .2,
	   'eat' => 1,
	   'breedfat' => 1,
	   'diespontaneous' => .005,
	   'breedhungry' => .006 / (1 - .005),
	   'diecrowded' => .006,
	   'text' => .001);

$gram->addType ('name' => $rps{'name'},
		'vars' => [ 'species' => 2 ],
		'hue' => [ 'var' => 'species', 'mul' => 42, 'add' => 10 ],
		'sat' => 255,
		'bri' => 255,
		'rate' => $rps{'rate'},
		'rule' =>
		['switch' => ['loc' => $gram->origin,
			      'var' => 'species',
			      'case' => [3 => ['huff' => [map ((1/3 => ['modify' => ['inc' => $_,
										     'dest' => ['var' => 'species']]]),
							       0..2)]]],
			      'default' => ['huff' => [ $rps{'diespontaneous'} => $gram->suicide,
							$gram->bindNeumann
							(1 - $rps{'diespontaneous'},
							 [ $gram->empty => ['huff' => [$rps{'step'} => $gram->moveOrSpawnTo ($rps{'breedhungry'},
															     $gram->neighbor,
															     $gram->balloon("step",'rate'=>$rps{'text'}),
															     $gram->balloon("breed",'rate'=>$rps{'text'}))]],
							   $rps{'name'} => [ make_species_switch
									     ($gram,
									      [ 'huff' => [ $rps{'diecrowded'} => $gram->suicide ] ],
									      [ 'huff' => [ $rps{'eat'} => $gram->moveOrSpawnTo ($rps{'breedfat'},
																 $gram->neighbor,
																 $gram->balloon("eat",'rate'=>$rps{'text'}),
																 $gram->balloon("spawn",'rate'=>$rps{'text'})) ]]) ] ])]]]]);

# rps animal tool
$gram->addTool ('name' => $rps{'name'},
		'size' => 1,
		'gvars' => [ 'type' => $rps{'name'}, 'species' => 3 ],
		'reserve' => 1,
		'recharge' => 100,
		'spray' => 100,
		'overwrite' => [ 'gstate' => 'empty' ]);

# perfume
my ($perfumeRate, $perfumeDrain, $perfumeBillow, $perfumeInduce) = (.6, .03, .03, .5);
$gram->addType ('name' => 'perfume',
		'hue' => 192,
		'sat' => 192,
		'bri' => 96,
		'rate' => $perfumeRate,
		'rule' => ['huff' => [$perfumeDrain => $gram->suicide,
				       $gram->bindNeumann (1 - $perfumeDrain,
							   [$gram->empty => $gram->moveOrSpawnTo ($perfumeBillow),
							    $rps{'name'} => ['huff' => [ $perfumeInduce => $gram->copyFromTo ($gram->neighbor, $gram->origin) ] ] ])]]);

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
zoocompiler.pl - generate a PixelZoo XML grammar

=head1 SYNOPSIS

zoocompiler.pl [options] <.zg file>

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
