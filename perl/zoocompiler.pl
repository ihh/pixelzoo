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
my ($proto, $out);

GetOptions('help|?' => \$help, man => \$man, verbose => \$verbose, debug => \$debug, 'out=s' => \$out, 'proto=s' => \$proto) or pod2usage(2);
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

# add some stuff

# cement
my ($cementRate, $cementDrain, $cementStick, $cementSet) = (.1, .03, .5, .01);
my @wallHue = (0, 42, 84);
$gram->addType ('name' => 'cement',
		'rate' => $cementRate,
		'rule' => ['huff' => [$cementDrain => [ 'modify' => [ 'set' => [ 'type' => $gram->empty ] ] ],
				       map (($cementSet/@wallHue => [ 'modify' => [ 'set' => [ 'type' => 'wall',
											       'decay' => 0,
											       'hue' => $_ ]]]),
					    @wallHue),
				       $gram->bindNeumann (1 - $cementSet - $cementDrain,
							   [$gram->empty => $gram->moveTo,
							    'wall' => [ 'huff' => [$cementStick => ['modify' => [ 'src' => [ 'loc' => $gram->neighbor ],
														  'dest' => [ 'loc' => $gram->origin ] ] ]]]])]]);

# cement tool
$gram->addTool ('name' => 'Cement spray',
		'size' => 2,
		'gstate' => 'cement',
		'reserve' => 1000,
		'recharge' => 100,
		'spray' => 100,
		'overwrite' => [ 'gstate' => 'empty' ]);

# wall
my $wallRate = .0002;
$gram->addType ('name' => 'wall',
		'vars' => [ 'hue' => 8, 'decay' => 4 ],
		'hue' => [ 'var' => 'hue' ],
		'sat' => 32,
		'bri' => [ 'var' => 'decay', 'mul' => -8, 'add' => 255 ],
		'rate' => $wallRate,
		'rule' => ['switch' => ['loc' => $gram->origin,
					 'var' => 'decay',
					 'case' => [ 15 => [ 'modify' => [ 'set' => [ 'type' => $gram->empty ],
									   'next' => [ 'rule' => ['ball' => [ 'rate' => .01,
													      'hexcolor' => "20ffff",
													      'text' => 'decay' ] ] ] ] ] ],
					 'default' => [ 'modify' => [ 'src' => [ 'var' => 'decay' ],
								      'inc' => 1 ]]]]);

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
  -verbose            report progress
  -debug              more info than you want

=head1 OPTIONS

=over 8

=item B<-help>

Prints a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=item B<-proto>

Specify a filename to save the proto-XML.

=item B<-out>

Specify a filename to save the output XML.
If no filename is specified, it will be printed to standard output.

=back

=head1 DESCRIPTION

B<This program> will generate a PixelZoo XML grammar.

=cut
