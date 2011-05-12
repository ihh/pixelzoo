
=head1 NAME

AutoHash.pm

=head1 SYNOPSIS

Simple Perl module wrapping a hashref with AUTOLOAD-ed accessors.

=head1 EXAMPLES

  use AutoHash;

  # construct a hash with two key-value pairs
  my $autohash = AutoHash->new ( "key1" => "value1",
                                 "key2" => "value2" );

  # get a value
  print $autohash->key1, "\n";

  # set a value
  $autohash->key2 ("new value");
  print $autohash->key2, "\n";

=head1 GENERAL USAGE

An AutoHash object is a blessed hash reference.

Its only inbuilt method is the constructor, 'new'.

All other methods will be automatically interpreted as hash element accessors for the eponymous tag.

If the method is called with an argument, it's a setter; otherwise, it's a getter.

=head1 METHODS

=cut

package AutoHash;

use Exporter;
@ISA = qw (Exporter);
@EXPORT = qw (new AUTOLOAD);
@EXPORT_OK = @EXPORT;

use strict;
use vars '@ISA';

use Carp;


=head2 new

    my $autohash1 = AutoHash->new();
    my $autohash2 = AutoHash->new (%existing_hash);

Creates a new AutoHash object.

=cut

sub new {
    my ($class, @data) = @_;
    my $self = {@data};
    $class = ref($class) if ref($class);
    bless $self, $class;
    return $self;
}

=head2 deepcopy

    my $autohash2 = $autohash1->deepcopy();

Recursively copies an AutoHash object.

=cut

sub deepcopy {
    my ($self) = @_;
    my $copy = AutoHash->new (deepcopy_list (%$self));
    bless $copy, ref($self);
    return $copy;
}

sub deepcopy_list {
    my @list = @_;
    my @copy;
    for my $v (@list) {
	if (ref($v) && ref($v) eq 'HASH') {
	    push @copy, {%$v};
	} elsif (ref($v) && ref($v) eq 'ARRAY') {
	    push @copy, {@$v};
	} elsif (ref($v) && ref($v) eq 'AutoHash') {
	    push @copy, $v->deepcopy;
	} else {
	    # default: $v is a scalar, or a non-copiable reference
	    push @copy, $v;
	}
    }
    return @copy;
}

=head2 Accessors (getters)

    $autohash->MYTAG()

Returns the hash value with tag "MYTAG".

=head2 Accessors (setters)

    $autohash->MYTAG ($MYVALUE)

Sets the value of hash tag "MYTAG" to $MYVALUE.
Returns $MYVALUE.

Creates a new AutoHash object.

=cut

# AUTOLOAD method
sub AUTOLOAD {
    my ($self, @args) = @_;
    my $sub = our $AUTOLOAD;

    confess "AutoHash method $sub called on non-reference" unless ref($self);

    $sub =~ s/.*:://;  # strip off module path

    # check for DESTROY
    return if $sub eq "DESTROY";

    # check we have the element
    confess "AutoHash: attempt to access element $sub, which is not in our hash" unless exists($self->{$sub});

    # get or set
    return @args 
	? ($self->{$sub} = shift(@args))
	: $self->{$sub};
}

=head1 AUTHOR

Ian Holmes.

=cut

1;
