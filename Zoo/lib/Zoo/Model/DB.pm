package Zoo::Model::DB;

use strict;
use base 'Catalyst::Model::DBIC::Schema';

__PACKAGE__->config(
    schema_class => 'Zoo::Schema',
    
    connect_info => {
        dsn => 'dbi:SQLite:zoo.db',
        user => '',
        password => '',
        on_connect_do => q{PRAGMA foreign_keys = ON},
    }
);

=head1 NAME

Zoo::Model::DB - Catalyst DBIC Schema Model

=head1 SYNOPSIS

See L<Zoo>

=head1 DESCRIPTION

L<Catalyst::Model::DBIC::Schema> Model using schema L<Zoo::Schema>

=head1 METHODS

=cut

=head2 worlds

Get a list of L<Zoo::Schema::Result::World> identifiers.

=cut

sub worlds {
    my ($self) = @_;
    my @world = $self->resultset('World')->all;
    return @world;
}


=head2 world_by_id

Get a L<Zoo::Schema::Result::World> object, given its identifier.

=cut

sub world_by_id {
    my ($self, $worldId) = @_;
#    $self->storage->debug(1);
    my $world = $self->resultset('World')->find($worldId);
    return $world;
}


=head2 world_active_lock

Get a L<Zoo::Schema::Result::Lock> object, for a given L<Zoo::Schema::Result::World> object.

=cut

sub world_active_lock {
    my ($self, $world) = @_;
    my @locks = $world->active_locks;
    return @locks ? $locks[$#locks] : undef;
}


=head2 purge_locks

Delete all locks that are no longer relevant.

=cut

sub purge_locks {
    my ($self) = @_;
#    $self->storage->debug(1);
    my $current_time = time();
    my $lock_rs = $self->resultset('Lock')->search_rs({'delete_time' => { '<=' => $current_time }});
    $lock_rs->delete();
}


=head2 particles_by_name

Get a list of L<Zoo::Schema::Result::Particle> objects, given a list of their name identifiers.

=cut

sub particles_by_name {
    my ($self, @particle_names) = @_;
    my @particles = $self->resultset('Particle')->search([map ({ 'name' => $_ }, @particle_names)]);
    return @particles;
}

=head2 tools_by_id

Get a list of L<Zoo::Schema::Result::Tool> objects, given a list of their identifiers.

=cut

sub tools_by_id {
    my ($self, @tool_ids) = @_;
    my @tools = @tool_ids ? $self->resultset('Tool')->search([map ({ 'id' => $_ }, @tool_ids)]) : ();
    return @tools;
}

=head2 descendant_particles

Get the list of L<Zoo::Schema::Result::Particle> objects that are named by, or downstream of all the particles named by, a given set of Particle, Tool, and Twiggy XML objects.

The dependency and tool_dependency tables are used to find downstream particles.

=cut

sub descendant_particles {
    my ($self, $particle_list, $tool_list, $twig_list) = @_;

    # hard-wired name of empty particle
    my $emptyType = "empty";

    # build the initial set of particles
    my %particle_name_hash = ($emptyType => 1);
    for my $particle (@$particle_list) {
	$particle_name_hash{ref($particle) ? $particle->name : $particle} = 1;
    }
    for my $tool (@$tool_list) {
	for my $particle ($tool->particles) {
	    $particle_name_hash{$particle->name} = 1;
	}
    }
    for my $twig (@$twig_list) {
	my @names = $twig->particle_names;
	%particle_name_hash = (%particle_name_hash, map (($_ => 1), @names));
    }
    my @ancestor_names = keys %particle_name_hash;
    my @ancestors = $self->particles_by_name (@ancestor_names);
    warn "Some named particles not found" if @ancestors < @ancestor_names;

    # do recursive query
    my %descendant_hash;
    while (@ancestors) {
	%descendant_hash = (%descendant_hash, map (($_->name => $_), @ancestors));
	my @descendants = map ($_->descendants, @ancestors);
	@ancestors = grep (!exists($descendant_hash{$_->name}), @descendants);
    }

    # the following sort ensures that 'empty' is particle #0; alphabetic sort of remaining particles is just for reproducibility
    my @sorted_particles = sort
    { $a->name eq $emptyType ? -1 : ($b->name eq $emptyType ? +1 : ($a->name cmp $b->name)) }
    values %descendant_hash;
#    warn "descendant_particles = (", join(", ",map("'".($_->name)."'",@particles)), ")";
    return @sorted_particles;
}

=head2 users_by_name

Get a list of L<Zoo::Schema::Result::User> objects, given a username.

=cut

sub users_by_name {
    my ($self, $username) = @_;
    my @users = $self->resultset('User')->search({ 'username' => $username });
    return @users;
}

=head2 user_by_id

Get a L<Zoo::Schema::Result::User> object, given an ID.

User IDs are unique primary keys. If no user with this ID is found, returns undef.

=cut

sub user_by_id {
    my ($self, $userid) = @_;
    my @users = $self->resultset('User')->search({ 'id' => $userid });
    return @users ? @users[0] : undef;
}

=head1 GENERATED BY

Catalyst::Helper::Model::DBIC::Schema - 0.48

=head1 AUTHOR

Ian Holmes

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
