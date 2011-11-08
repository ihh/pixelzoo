package Twiggy::Elt;
use Moose;
use XML::Twig;
use namespace::autoclean;

extends 'XML::Twig::Elt';


=head1 NAME

Twiggy::Elt - XML::Twig::Elt derived class

=head1 SYNOPSIS

XML::Twig::Elt, with minor enhancements

=head1 DESCRIPTION

L<XML::Twiggy::Elt> derived class, offering a few helper methods for the Zoo.

=head1 METHODS

=head2 children

Casts super method to L<Twiggy::Elt>

=cut

sub children {
    my ($self, @args) = @_;
    my @child = $self->SUPER::children(@args);
    grep (bless ($_, 'Twiggy::Elt'), @child);
    return @child;
}

=head2 twig_nest

Converts an L<XML::Twig> into a tree of nested anonymous arrays of tag=>value pairs.

The single argument, if present, is a reference to a hash of (tag,attribute) pairs:
any tags in the keyset of this hash will be substituted with the value of their corresponding
attributes.

=cut

my %default_tag_attr_hash = ( 'p' => 'value',
			      'target' => 'value',
			      'val' => 'var');
sub twig_nest {
    my ($self, $tag_attr_ref) = @_;
    $tag_attr_ref = \%default_tag_attr_hash unless defined $tag_attr_ref;   # temporarily hardwired hack
    my $tag = $self->tag;
    my @child = $self->children;
    if (defined($tag_attr_ref) && exists $tag_attr_ref->{$tag}) {
	my $attr = $tag_attr_ref->{$tag};
	$tag .= '@' . $attr . '=' . $self->{'att'}->{$attr};
    }
    return ($tag => '') unless @child;
    if (@child == 1 && ($child[0]->is_cdata || $child[0]->is_pcdata)) {
	return ($tag => $child[0]->text);
    }
    return ($tag => [map ($_->twig_nest, @child)]);
}

=head1 AUTHOR

Ian Holmes

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
