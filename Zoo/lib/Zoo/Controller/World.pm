package Zoo::Controller::World;
use Data::Dumper;
use Moose;
use Twiggy;
use Level;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller::REST' }

__PACKAGE__->config(default => 'text/xml');

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
    $c->response->redirect ($c->uri_for($c->controller('World')->action_for('worldlist')), 303);
    $c->detach;
}

=head2 world/list

This must come before world/*

=cut

sub worldlist :Path('list') :Args(0) {
    my ( $self, $c ) = @_;

    my @worlds = $c->model('DB')->worlds;
    $c->stash->{worlds} = \@worlds;

    $c->stash->{template} = 'world/list.tt2';
}

=head2 world_id

This must come after world/list

=cut

sub world_id :Chained('/') :PathPart('world') :CaptureArgs(1) {
    my ( $self, $c, $world_id ) = @_;

    $c->stash->{template} = 'empty.tt2';   # start of chain: default to empty view template

    my $world = $c->model('DB')->world_by_id($world_id);
    if (!defined $world) {
	$c->stash( error_msg => "World $world_id does not exist" );
	$c->response->status(404);
	$c->detach();
    }

    $c->stash->{world} = $world;
    $c->stash->{world_id} = $world_id;
}

sub world_id_end :Chained('world_id') :PathPart('') :Args(0) :ActionClass('REST') { }

sub world_id_end_GET {
    my ( $self, $c ) = @_;
    my $world = $c->stash->{world};
    $c->response->redirect ($c->uri_for($c->controller('World')->action_for('status'), [$world->id]), 303);
    $c->detach;
}


=head2 board

=cut

sub board :Chained('world_id') :PathPart('board') :Args(0) :ActionClass('REST') { }

sub board_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/board.tt2';
    $c->response->headers->last_modified($c->stash->{world}->last_modified_time);
}


=head2 status

=cut

sub status :Chained('world_id') :PathPart('status') :Args(0) :ActionClass('REST') { }

sub status_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/status.tt2';
    $c->response->headers->last_modified($c->stash->{world}->last_stolen_time);
}

=head2 game

=cut

sub game :Chained('world_id') :PathPart('game') :CaptureArgs(0) {
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


=head2 assemble

Assembles a Grammar from the components (Board, Particles, Game metadata, Tools). A helper method called from the view and lock chains.

=cut

sub assemble {
    my ( $self, $c, $board, $game, @tool_names ) = @_;

    # tools
    my @tools = $c->model('DB')->tools_by_name(@tool_names);
    $c->log->debug ("Tool names: " . join (", ", map ($_->name, @tools)));
    $c->stash->{tools} = \@tools;

    my @tool_twig = map ($_->twig, @tools);
#    warn map($_->sprint,@tool_twig);

    # What we should do here is loop over the following call to descendant_particles,
    # popping tools until the number of particles is less than a configurable limit (<64k)

    # What to do if the board itself contains >64K downstream particles?
    # ideal/generic: sort particles by some function f(D,B) where D = upstream dependencies and B = number on board; drop lowest-ranked.

    # particles
#    print "particles: ", map (join (" ", map ($_->text, $_->descendants("gstate"))) . "\n", @tool_twig);
#    print "particles: ", map (join (" ", $_->particle_names) . "\n", @tool_twig);
    my @particles = $c->model('DB')->descendant_particles ($board, @tool_twig);
    $c->log->debug ("Particle names: " . join (", ", map ($_->name, @particles)));
    $c->stash->{particles} = \@particles;

    # grammar
    my $gram = Grammar->newMinimalGrammar;   # using newMinimalGrammar avoids creating a default 'empty' particle type
    $gram->verbose(1);

    for my $particle (@{$c->stash->{particles}}) {
	warn "Adding type ", $particle->nest;
	$gram->addType ($particle->nest);
    }

    # board size
    $gram->boardSize($c->stash->{world}->board_size);

    # misc tags
    for my $board_tag (qw(init seed)) {
	for my $child ($board->root->children ($board_tag)) {
	    push @{$gram->xml->board_stash}, $board->twig_nest ($child);  # stash all recognized board tags
	}
    }

    # game
    my @game_nest = $game->twig_nest;
    push @{$gram->xml->game_stash}, @{$game_nest[1]};  # stash all game meta-info

    # tools
    for my $tool (@{$c->stash->{tools}}) {
	$gram->addTool ($tool->nest);
    }

    # stash grammar
    $c->stash->{grammar} = $gram;
#    warn Dumper($gram->xml->type);
}

=head2 view

=cut

sub view :Chained('world_id') :PathPart('view') :CaptureArgs(0) {
    my ( $self, $c ) = @_;

    # assemble using world's current board, voyeur rules, and no tools
    my $world = $c->stash->{world};
    $self->assemble ($c, $world->board, $world->voyeur_game);
}

sub view_end :Chained('view') :PathPart('') :Args(0) :ActionClass('REST') { }

sub view_end_GET {
    my ( $self, $c ) = @_;
    my $gram = $c->stash->{grammar};

    $c->stash->{template} = 'world/compiled.tt2';
    $c->stash->{compiled_xml} = $gram->assembled_xml;
}

=head2 view_compiled

=cut

sub view_compiled :Chained('view') :PathPart('compiled') :Args(0) :ActionClass('REST') { }

sub view_compiled_GET {
    my ( $self, $c ) = @_;
    my $gram = $c->stash->{grammar};

    $c->stash->{template} = 'world/compiled.tt2';
    $c->stash->{compiled_xml} = $gram->compiled_xml;
}


=head2 lock

Get the Lock(s) for the World.

=cut

sub lock :Chained('world_id') :PathPart('lock') :CaptureArgs(0) {
    my ( $self, $c ) = @_;

    my $world = $c->stash->{world};
    my $world_id = $c->stash->{world_id};

    my $lock = $c->model('DB')->world_active_lock($world);   # eventually, world_active_lock should check userid too

    $c->stash->{lock} = $lock;
}

sub lock_end :Chained('lock') :PathPart('') :Args(0) :ActionClass('REST') { }

sub lock_end_GET {
    my ( $self, $c ) = @_;
    my $world = $c->stash->{world};
    my $lock = $c->stash->{lock};
    if (defined $lock) {
	$c->response->redirect ("/world/" . $world->id . "/lock/" . $lock->id, 303);
	$c->detach;
    } else {
	$c->stash( error_msg => "No lock found" );
	$c->response->status(404);
	$c->detach();
    }
}

sub lock_end_POST {
    my ( $self, $c ) = @_;
    my $world = $c->stash->{world};
    my $lock = $c->stash->{lock};
    if (defined $lock) {
	$c->response->status(423);
	$c->detach();
    } else {
	# create the lock...
	# First, get the tool names from the POST'ed lock XML
	my $lock_twig = Twiggy->new();
	$lock_twig->parse ($c->request->body);
	my @tool_names = map ($_->text, $lock_twig->root->first_child("toolbox")->children("name"));
#	warn "Tools:\n", map (" $_\n", @tool_names);
	# Assemble the board XML
	# For now, use voyeur rules (until more owner/guest logic is implemented)
	$self->assemble ($c, $world->board, $world->voyeur_game, @tool_names);
	my $compiled_xml = $c->stash->{grammar}->compiled_xml;
	my $get_proto_xml = $c->stash->{grammar}->get_assembled_xml_stash;
	# add the lock to the database
	my $create_time = time();
	my $expiry_time = $create_time + $world->lock_expiry_delay;
	my $delete_time = $create_time + $world->lock_delete_delay;
	my $user_id = 1;    # HACK: TODO: use Catalyst::Plugin::Authentication to get proper user IDs
	my $lock = $c->model('DB::Lock')->create({
	    world_id => $c->stash->{world}->id,
	    owner_id => $user_id,
	    create_time => $create_time,
	    expiry_time => $expiry_time,
	    delete_time => $delete_time,
	    proto_xml => &{$get_proto_xml}(),
	    compiled_xml => $compiled_xml });
	# return lock info
	$c->stash->{template} = 'world/lock.tt2';
	$c->stash->{lock} = $lock;
	$self->status_created ($c, location => ($c->req->uri->as_string . '/' . $lock->lock_id), entity => {});  # dummy entity, since we're not serializing it (using TT instead)
    }
}

=head2 lock_id

Check that there is exactly one Lock for this World, and that the path contains the correct Lock ID.

=cut

sub lock_id :Chained('lock') :PathPart('') :CaptureArgs(1) {
    my ( $self, $c, $lock_id ) = @_;

    my $lock = $c->stash->{lock};
    unless (defined($lock) && $lock->id == $lock_id) {
	$c->stash( error_msg => "Lock $lock_id not found in world " . $c->stash->{world}->id );
	$c->response->status(404);
	$c->detach();
    }
}

sub lock_id_end :Chained('lock_id') :PathPart('') :Args(0) :ActionClass('REST') { }

sub lock_id_end_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/lock.tt2';
}



=head2 lock_view

=cut

sub lock_view :Chained('lock_id') :PathPart('view') :CaptureArgs(0) {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/compiled.tt2';
}

sub lock_view_end :Chained('lock_view') :PathPart('') :Args(0) :ActionClass('REST') { }

sub lock_view_end_GET {
    my ( $self, $c ) = @_;
    my $lock = $c->stash->{lock};
    $c->stash->{compiled_xml} = $lock->proto_xml;
}

=head2 lock_view_compiled

=cut

sub lock_view_compiled :Chained('lock_view') :PathPart('compiled') :Args(0) :ActionClass('REST') { }

sub lock_view_compiled_GET {
    my ( $self, $c ) = @_;
    my $lock = $c->stash->{lock};
    $c->stash->{compiled_xml} = $lock->compiled_xml;
}


=head2 turn

Post a turn.

=cut

sub turn :Chained('world_id') :PathPart('turn') :CaptureArgs(0) {
    my ( $self, $c ) = @_;

    my $world = $c->stash->{world};
    my $world_id = $c->stash->{world_id};

    # TODO: write me

    # TODO: add a cleanup rule
    #  DELETE FROM lock WHERE delete_time <= $current_time;
}

sub turn_end :Chained('turn') :PathPart('') :Args(0) :ActionClass('REST') { }

sub turn_end_POST {
    my ( $self, $c ) = @_;
    my $world = $c->stash->{world};

    # TODO: write me
    # Update the state of the board
    # Delete the lock
}


=head2 end

=cut

# The "right" way to provide XML as a web service is probably to use
# the REST Controller's serialization action class to serialize XML::Simple objects.
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
