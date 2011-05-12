package Zoo::Controller::World;
use Moose;
use Twiggy;
use Level;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller::REST' }

__PACKAGE__->config(
    'map'       => {
        'text/xml'           => 'Twiggy',
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

    my @worlds = $c->model('DB')->worlds;
    $c->stash->{worlds} = \@worlds;

    $c->stash->{template} = 'world/list.tt2';
}


=head2 world

=cut

sub world :Chained('/') :PathPart('world') :CaptureArgs(1) {
    my ( $self, $c, $worldId ) = @_;

    $c->stash->{template} = 'empty.tt2';   # start of chain: default to empty view template

    my $world = $c->model('DB')->world_by_id($worldId);
    if (!defined $world) {
	$c->stash( error_msg => "World $worldId does not exist" );
	$c->response->status(404);
	$c->detach();
    }

    $c->stash->{world} = $world;
# The commented-out line auto-calculates the board size from the XML, and stuffs it in the stash
# I think ideally we should do it the other way round - keep it in the world table (which we do now) and enforce XML consistency (which we don't)
#    $c->stash->{board_size} = $world->board->root->first_child_text('size');
}


=head2 board

=cut

sub board :Chained('world') :PathPart('board') :Args(0) :ActionClass('REST') { }

sub board_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/board.tt2';
    $c->response->headers->last_modified($c->stash->{world}->last_modified_time);
}


=head2 status

=cut

sub status :Chained('world') :PathPart('status') :Args(0) :ActionClass('REST') { }

sub status_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/status.tt2';
    $c->response->headers->last_modified($c->stash->{world}->last_stolen_time);
}

=head2 game

=cut

sub game :Chained('world') :PathPart('game') :CaptureArgs(0) {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/game.tt2';
}


=head2 owner

=cut

sub owner :Chained('game') :PathPart('owner') :Args(0) :ActionClass('REST') { }

sub owner_GET {
    my ( $self, $c ) = @_;
    $c->stash->{game_xml} = $c->stash->{world}->owner_game_xml;
}

=head2 guest

=cut

sub guest :Chained('game') :PathPart('guest') :Args(0) :ActionClass('REST') { }

sub guest_GET {
    my ( $self, $c ) = @_;
    $c->stash->{game_xml} = $c->stash->{world}->guest_game_xml;
}

=head2 voyeur

=cut

sub voyeur :Chained('game') :PathPart('voyeur') :Args(0) :ActionClass('REST') { }

sub voyeur_GET {
    my ( $self, $c ) = @_;
    $c->stash->{game_xml} = $c->stash->{world}->voyeur_game_xml;
}

=head2 view

=cut

sub view :Chained('world') :PathPart('view') :Args(0) :ActionClass('REST') { }

sub view_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/game.tt2';
    $c->stash->{board_xml} = $c->stash->{world}->board_xml;
    $c->stash->{game_xml} = $c->stash->{world}->voyeur_game_xml;
}

=head2 view_compiled

=cut

sub view_compiled :Chained('world') :PathPart('view/compiled') :Args(0) :ActionClass('REST') { }

sub view_compiled_GET {
    my ( $self, $c ) = @_;

    my $world = $c->stash->{world};
    my @particles = $c->model('DB')->descendant_particles($world->board);
    $c->log->debug ("Particle names: " . join (", ", map ($_->name, @particles)));

    my $gram = Level->newLevel;
    $gram->boardSize($world->board_size);

    for my $particle (@particles) {
	my $twig = $particle->twig;
	my @nest = $twig->twig_nest;
	if (@nest == 2) {
	    $gram->addType (@{$nest[1]});
	}
    }

    $c->stash->{template} = 'world/compiled.tt2';
    $c->stash->{compiled_xml} = $gram->compiled_xml;
}


=head2 end

=cut

# The "right" way to provide XML as a web service is probably to use
# the REST Controller's serialization action class to serialize XML::Twig objects.
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
