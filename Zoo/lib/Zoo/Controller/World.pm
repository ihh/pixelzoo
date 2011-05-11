package Zoo::Controller::World;
use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller::REST' }
#BEGIN { extends 'Catalyst::Controller' }

__PACKAGE__->config(
    'map'       => {
        'text/xml'           => 'XML::Twig',
    },
);

=head1 NAME

Zoo::Controller::World - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut


=head2 index

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    my @worlds = $c->model('DB')->resultset('World')->all;
    $c->stash->{worlds} = \@worlds;

    $c->stash->{template} = 'world/list.tt2';
}


=head2 world

=cut

sub world :Chained('/') :PathPart('world') :CaptureArgs(1) {
    my ( $self, $c, $worldId ) = @_;

    $c->stash->{template} = 'empty.tt2';   # start of chain: default to empty view template

    my @world = $c->model('DB')->resultset('World')->search({ 'id' => $worldId });
    if (@world != 1) {
	$c->stash( error_msg => "World $worldId does not exist" );
	$c->response->status(404);
	$c->detach();
    }

    $c->stash->{world} = pop(@world);
}


=head2 board

=cut

sub board :Chained('world') :PathPart('board') :Args(0) :ActionClass('REST') { }

sub board_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/board.tt2';
    $c->stash->{board_xml} = $c->stash->{world}->board_xml;
}


=head2 status

=cut

sub status :Chained('world') :PathPart('status') :Args(0) :ActionClass('REST') { }

sub status_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/status.tt2';
}


=head2 end

=cut

# The "right" way to provide XML as a web service is probably to use
# the REST Controller's serialization action class to serialize XML::Twig objects,
# together with XML::Twig accessors on the Model so that everything passes through as XML::Twig.
# However, for now it's easier just to use Template Toolkit templates to stitch text together.
# Thus, I restore the default ActionClass on the end method, like so:
sub end : ActionClass('RenderView') { }


=head1 AUTHOR

Ian Holmes

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

__PACKAGE__->meta->make_immutable;

1;
