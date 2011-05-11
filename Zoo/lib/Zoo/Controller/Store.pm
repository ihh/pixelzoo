package Zoo::Controller::Store;
use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller::REST' }

=head1 NAME

Zoo::Controller::Store - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut


=head2 index

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    $c->response->body('Matched Zoo::Controller::Store in Store.');
}


=head2 end

=cut

sub end : ActionClass('RenderView') { }


=head1 AUTHOR

Ian Holmes

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

__PACKAGE__->meta->make_immutable;

1;
