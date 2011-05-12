package Twiggy;
use Moose;
use namespace::autoclean;

extends 'XML::Twig';


=head1 NAME

Twiggy - XML::Twig derived class

=head1 SYNOPSIS

XML::Twig, with minor enhancements

=head1 DESCRIPTION

L<XML::Twig> derived class, offering conversion to a nested-array format

=head1 METHODS

=cut

=head2 twig_nest

Returns an XML::Twig as a tree of nested anonymous arrays of tag=>value pairs.

=cut

sub twig_nest {
    my ($self, $elt) = @_;
    if (!defined ($elt)) {
	$elt = $self->root;
    }
    my @child = $elt->children;
    if (@child == 1 && ($child[0]->is_cdata || $child[0]->is_pcdata)) {
	return ($elt->tag => $child[0]->text);
    }
    return ($elt->tag => [map ($self->twig_nest($_), @child)]);
}

=head1 AUTHOR

Ian Holmes

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
