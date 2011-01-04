#!/usr/bin/perl -w

use Getopt::Long;
use Pod::Usage;
use XML::Parser;
use XML::Writer;

my $man = 0;
my $help = 0;

GetOptions('help|?' => \$help, man => \$man) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

pod2usage(2) unless @ARGV == 1;
 
# initialize parser and read the file
$parser = new XML::Parser( Style => 'Tree' );
my $tree = $parser->parsefile( shift @ARGV );
 
# serialize the structure
use Data::Dumper;
print Dumper( $tree );

__END__

=head1 NAME
zoocompiler.pl - compile PixelZoo XML files

=head1 SYNOPSIS

zoocompiler.pl [options] <XML file>

 Options:
  -help            brief help message
  -man             full documentation

=head1 OPTIONS

=over 8

=item B<-help>

Prints a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=back

=head1 DESCRIPTION

B<This program> will compile a PixelZoo XML grammar from name/property format into state format.

=cut
