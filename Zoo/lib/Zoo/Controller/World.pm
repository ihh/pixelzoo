package Zoo::Controller::World;
use Data::Dumper;
use Moose;
use Twiggy;
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

Returns list of worlds

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;
    $c->authenticate({});
    my @worlds = $c->model('DB')->worlds;
    $c->stash->{worlds} = \@worlds;
    $c->stash->{template} = 'world/list.tt2';
}

=head2 world_id

Get world by ID

=cut

sub world_id :Chained('/') :PathPart('world') :CaptureArgs(1) {
    my ( $self, $c, $world_id ) = @_;

    $c->stash->{template} = 'empty.tt2';   # start of chain: default to empty view template

    my $world = $c->model('DB')->world_by_id($world_id);
    if (!defined $world) {
	$c->stash( error_msg => "World $world_id does not exist" );
	$c->response->status(404);  # 404 Not Found
	$c->detach();
    }

    $c->stash->{world} = $world;
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
# Commenting out this last_modified stuff until we know for sure that the page won't contain any later-modified info, e.g. current lock details
#    $c->response->headers->last_modified($c->stash->{world}->last_modified_time);
}


=head2 status

=cut

sub status :Chained('world_id') :PathPart('status') :Args(0) :ActionClass('REST') { }

sub status_GET {
    my ( $self, $c ) = @_;
    $c->authenticate({});
    my $user = $c->user;
    my $world = $c->stash->{world};
    my $user_is_owner = $user->id == $world->owner_id;
    $c->stash->{user} = $user;
    $c->stash->{expired_locks} = [ $world->expired_locks ($user->id) ];
    $c->stash->{toolbox} = $user_is_owner ? $world->meta_rel->owner_toolbox : $world->meta_rel->guest_toolbox;
    $c->stash->{template} = 'world/status.tt2';
# Commenting out this last_modified stuff until we know for sure that the page won't contain any later-modified info, e.g. current lock details
#    $c->response->headers->last_modified($c->stash->{world}->last_stolen_time);
}

=head2 game

=cut

sub game :Chained('world_id') :PathPart('game') :CaptureArgs(0) {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'world/game.tt2';
}


=head2 assemble

Assembles a game XML file from the components (Board, Particles, Tools, etc). A helper method called from the view and lock chains.

=cut

sub assemble {
    my ( $self, $c, $board, $toolset_xml, @tool_ids ) = @_;

    # tools
    my @tools = $c->model('DB')->tools_by_id(@tool_ids);
#    $c->log->debug ("Tool names: " . join (", ", map ($_->name, @tools)));
    $c->stash->{tools} = \@tools;
    $c->stash->{toolset_xml} = defined($toolset_xml) ? $toolset_xml : "";

    my @tool_twig = map ($_->twig, @tools);

    # What we should do here is loop over the following call to descendant_particles,
    # popping tools until the number of particles is less than a configurable limit (<64k)

    # What to do if the board itself contains >64K downstream particles?
    # ideal/generic: sort particles by some function f(D,B) where D = upstream dependencies and B = number on board; drop lowest-ranked.

    # get contest info
    my $world = $c->stash->{world};
    my $user = $c->user;
    my $contestType = $world->meta_rel->contest_type->name;
    my $contestVar = $world->meta_rel->contest_var;

    # set contest info
    $c->stash->{contestType} = $contestType;
    $c->stash->{contestVar} = $contestVar;
    $c->stash->{incumbent} = $world->owner_id;
    $c->stash->{challenger} = $user->id;

    # particles
    my @particles = $c->model('DB')->descendant_particles ([$contestType],
							   \@tools,
							   [$board, @tool_twig]);

    $c->stash->{particles} = \@particles;
#    $c->log->debug ("Particle names: " . join (", ", map ($_->name, @particles)));

    # misc tags
    $c->stash->{seed} = $board->root->has_child("seed") ? $board->root->first_child("seed")->sprint : "";
    $c->stash->{inits} = [map ($_->sprint, $board->root->children ("init"))];
}

=head2 view

=cut

sub view :Chained('world_id') :PathPart('view') :CaptureArgs(0) {
    my ( $self, $c ) = @_;

    # assemble using world's current board and no tools
    my $world = $c->stash->{world};
    $self->assemble ($c, $world->board);
}

sub view_end :Chained('view') :PathPart('') :Args(0) :ActionClass('REST') { }

sub view_end_GET {
    my ( $self, $c ) = @_;
    my $gram = $c->stash->{grammar};

    $c->stash->{template} = 'world/compiled.tt2';
}


=head2 lock

Actions relating to the Lock(s) for the World.

=cut

sub lock :Chained('world_id') :PathPart('lock') :CaptureArgs(0) {
    my ( $self, $c ) = @_;

    $self->stash_lock($c);
}


=head2 stash_lock

Get the user's Lock for the World, if any.

=cut

sub stash_lock {
    my ($self, $c) = @_;
    my $world = $c->stash->{world};
    my $lock = $c->model('DB')->world_active_lock($world);
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
	$c->response->status(404);  # 404 Not Found
	$c->detach();
    }
}

sub lock_end_POST {
    my ( $self, $c ) = @_;
    $c->authenticate({});
    my $user_id = $c->user->id;
    my $world = $c->stash->{world};
    my $user_is_owner = $user_id == $world->owner_id;
    my $lock = $c->stash->{lock};
    my $user_has_lock = defined($lock) && $lock->owner_id == $user_id;
    my $create_time = time();
    my $expiry_time = $create_time + $world->meta_rel->lock_expiry_delay;
    if (defined($lock) && !$user_has_lock) {
	$c->response->status(423);  # 423 Locked
    } elsif ($world->expired_locks ($user_id)) {
	$c->response->status(403);  # 403 Forbidden
	$c->detach();
    } else {
	# if user already owns a lock, delete it but keep its expiry time
	if ($user_has_lock) {
	    $expiry_time = $lock->expiry_time;
	    $lock->delete;
	    $lock = undef;
	    $user_has_lock = 0;
	}
	# create the lock...
	my $delete_time = $expiry_time + $world->meta_rel->lock_delete_delay;
	# First, get the tool names from the POST'ed lock XML
	my @tool_ids;
	if ($c->request->content_length) {
	    my $lock_twig = Twiggy->new();
	    $lock_twig->parse ($c->request->body);
	    @tool_ids = map ($_->text, $lock_twig->root->first_child("tools")->children("id"));
	}
	# Assemble the board XML
	my $toolset_xml = $user_is_owner ? $world->meta_rel->owner_toolset_xml : $world->meta_rel->guest_toolset_xml;
	# For now, use voyeur rules (until more owner/guest logic is implemented)
	$self->assemble ($c, $world->board, $toolset_xml, @tool_ids);
	# add the lock to the database
	$lock = $c->model('DB::Lock')->create({
	    world_id => $c->stash->{world}->id,
	    owner_id => $user_id,
	    create_time => $create_time,
	    expiry_time => $expiry_time,
	    delete_time => $delete_time,
	    toolset_xml => $toolset_xml });
	# and the tools
	for my $tool_id (@tool_ids) {
	    $c->model('DB::LockTool')->create({
		lock_id => $lock->lock_id,
		tool_id => $tool_id });
	}
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
	$c->response->status(404);  # 404 Not Found
	$c->detach();
    }
}

sub lock_id_end :Chained('lock_id') :PathPart('') :Args(0) :ActionClass('REST') { }

sub lock_id_end_GET {
    my ( $self, $c ) = @_;
    my $lock = $c->stash->{lock};
    my $world = $c->stash->{world};
    my @tool_ids = map ($_->tool, $lock->lock_tools);
    $self->assemble ($c, $world->board, $lock->toolset_xml, @tool_ids);
    $c->stash->{template} = 'world/lock.tt2';
}

sub lock_id_end_DELETE {
    my ( $self, $c ) = @_;

    # Authenticate
    $c->authenticate({});
    my $user_id = $c->user->id;

    # If user owns lock, delete it; otherwise, complain
    my $lock = $c->stash->{lock};
    if ($user_id == $lock->owner_id) {
	# we don't actually delete the lock; just set its expiry_time to now, and reduce its delete_time
	my $current_time = time();
	$lock->expiry_time ($current_time);
	$lock->delete_time ($current_time + $lock->world->meta_rel->lock_delete_delay);
	$lock->update;
	# return the modified lock
	$c->stash->{template} = 'world/lock.tt2';
	$c->stash->{lock} = $lock;
	$c->response->status (200);  # 200 OK
    } else {
	$c->response->status(403);  # 403 Forbidden
    }
}




=head2 turn

Post a turn.

=cut

sub turn :Chained('world_id') :PathPart('turn') :CaptureArgs(0) { }

sub turn_end :Chained('turn') :PathPart('') :Args(0) :ActionClass('REST') { }

sub turn_end_POST {
    my ( $self, $c ) = @_;

    # Authenticate
    $c->authenticate({});
    my $user_id = $c->user->id;

    # Delete all out-of-date locks
    $c->model('DB')->purge_locks();

    # Get the current lock, if any
    $self->stash_lock($c);

    my $world = $c->stash->{world};
    my $lock = $c->stash->{lock};

    if (defined $lock) {
	if ($lock->owner_id == $user_id) {

	    # Update the state of the board
	    my $turn_twig = Twiggy->new();
	    $turn_twig->parse ($c->request->body);
	    my $board_twig = $turn_twig->root->first_child("board");

	    $world->board_xml ($board_twig->sprint);
	    $world->board_time ($board_twig->first_child("t")->text);

	    if ($board_twig->has_child("winner")) {
		# Ultimately we should validate ownership here
		$world->owner_id ($board_twig->first_child("winner")->text);
	    }

	    my $current_time = time();
	    $world->last_modified_time ($current_time);

	    $world->update;  # commit the changes

	    $c->response->status(204);  # 204 No Content (success)
	} else {
	    $c->response->status(423);  # 423 Locked (not your lock)
	    $c->detach();
	}
    } else {
	$c->response->status(403);  # 403 Forbidden (not locked)
	$c->detach();
    }
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
