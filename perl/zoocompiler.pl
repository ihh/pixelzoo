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

GetOptions('help|?' => \$help, man => \$man, verbose => \$verbose, debug => \$debug, 'xmllint=s' => \$xmllint) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

# pod2usage(2) unless @ARGV == 1;

$verbose = 1 if $debug;

# read input
if (@ARGV == 0) { warn "[waiting for XML input]\n" }
my @input = <>;
my $input = join ("", @input);

my $twig = Twiggy->new();
$twig->parse ($input);

my @proto_nest = $twig->twig_nest;

# compile
my $gram = Grammar->newGrammar;
$gram->verbose($verbose);
$gram->debug($debug);
$gram->xmllint($xmllint) if defined $xmllint;

$gram->parse_types (\@proto_nest);

my $compiled_nest = $gram->compiled_proto_xml (\@proto_nest);

$gram->print ($compiled_nest);

__END__

=head1 NAME
zoocompiler.pl - compile PixelZoo "game" XML from "prototype" XML

=head1 SYNOPSIS

zoocompiler.pl [options]

 Options:
  -help               brief help message
  -man                full documentation
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

=back

=head1 DESCRIPTION

B<This program> will compile a PixelZoo grammar from "prototype" to "game" XML dialect.

=cut
