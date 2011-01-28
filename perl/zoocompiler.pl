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
my ($cpp, $INC_PATH, $ppfile);

GetOptions('help|?' => \$help, man => \$man, verbose => \$verbose, debug => \$debug, 'preprocessor|cpp=s' => \$cpp, 'savepp=s' => \$ppfile, 'INC=s' => \$INC_PATH) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

# pod2usage(2) unless @ARGV == 1;

unless (defined $cpp) {
    $cpp = "$Bin/convert-proc-to-define.pl | gcc -x c -E";
    $cpp .= " -I$INC_PATH" if defined $INC_PATH;
    $cpp .= " -";
}

$verbose = 1 if $debug;


# test
my $gram = Level->newLevel;
$gram->verbose($verbose);
$gram->debug($debug);

# add some stuff

# cement
my ($cementRate, $cementDrain, $cementStick, $cementSet) = (.1, .01, .5, .01);
my @wallHue = (0, 42, 84);
$gram->addType ('name' => 'cement',
		'rate' => $cementRate,
		'rule' => ['.huff' => [$cementDrain => [ '.modify' => [ 'set' => [ 'type' => $gram->empty ] ] ],
				       map (($cementSet/@wallHue => [ '.modify' => [ 'set' => [ 'type' => 'wall',
												'decay' => 0,
												'hue' => $_ ]]]),
					    @wallHue),
				       $gram->bindNeumann (1 - $cementSet - $cementDrain,
							   [$gram->empty => $gram->moveTo,
							    'wall' => [ '.huff' => [$cementStick => ['.modify' => [ 'src' => [ 'loc' => $gram->neighbor ],
														    'dest' => [ 'loc' => $gram->origin ] ] ]]]])]]);

# cement tool
$gram->addTool ('name' => 'Cement spray',
		'size' => 8,
		'type' => 'cement',
		'reserve' => 100,
		'recharge' => 100,
		'overwrite' => [ '.tstate' => [ '@tag' => 'hexstate', 'type' => 'empty' ] ]);

# wall
my $wallRate = .0002;
$gram->addType ('name' => 'wall',
		'var' => [ 'hue' => 8, 'decay' => 4 ],
		'hue' => [ 'var' => 'hue' ],
		'bri' => [ 'var' => 'decay', 'mul' => -8, 'inc' => 255 ],
		'sat' => 32,
		'rate' => $wallRate,
		'rule' => ['.switch' => ['loc' => $gram->origin,
					 'var' => 'decay',
					 'case' => [ 15 => [ '.modify' => [ 'set' => [ 'type' => $gram->empty ],
									    'next' => [ 'rule' => ['.text' => [ 'rate' => .01,
														'hexcolor' => "20ffff",
														'text' => 'decay' ] ] ] ] ] ],
					 'default' => [ '.modify' => [ 'dest' => [ 'var' => 'decay' ],
								       'inc' => 1 ]]]]);

# print
$gram->print;


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
The preprocessor should read from standard input.

Default is "perl/convert-define-to-proc.pl | gcc -x c -E -I${INC_PATH} -"
where ${INC_PATH} can be set using the -INC option to zoocompiler.

=item B<-savepp>

Specify a filename to save the intermediate file generated by running the input through the preprocessor.

=back

=head1 DESCRIPTION

B<This program> will compile a PixelZoo .zg file into an XML grammar.

=cut
