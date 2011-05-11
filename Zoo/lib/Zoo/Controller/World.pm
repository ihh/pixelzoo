package Zoo::Controller::World;
use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller::REST' }
#BEGIN { extends 'Catalyst::Controller' }

=head1 NAME

Zoo::Controller::World - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut


=head2 world

=cut

sub world :Chained('/') :PathPart('world') :CaptureArgs(1) {
    my ( $self, $c, $worldId ) = @_;

    $c->stash->{world} = [$c->model('DB')->resultset('World')->search({ 'id' => $worldId })];

#    $c->response->body('Matched Zoo::Controller::World in world.');
}


=head2 board

=cut

sub board :Chained('world') :PathPart('board') :Args(0) :ActionClass('REST') {
    my ( $self, $c ) = @_;

#    $c->response->body('Matched Zoo::Controller::World in board.');
}

sub board_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/game.tt2';
}



=head2 index

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    $c->stash->{template} = 'world/list.tt2';

#    $c->response->body('Matched Zoo::Controller::World in index.');
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
