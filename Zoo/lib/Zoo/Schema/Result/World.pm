use utf8;
package Zoo::Schema::Result::World;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::World

=cut

use strict;
use warnings;

use Moose;
use MooseX::NonMoose;
use MooseX::MarkAsMethods autoclean => 1;
extends 'DBIx::Class::Core';

=head1 COMPONENTS LOADED

=over 4

=item * L<DBIx::Class::InflateColumn::DateTime>

=back

=cut

__PACKAGE__->load_components("InflateColumn::DateTime");

=head1 TABLE: C<world>

=cut

__PACKAGE__->table("world");

=head1 ACCESSORS

=head2 id

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 name

  data_type: 'text'
  is_nullable: 1

=head2 meta_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 owner_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 board_time

  data_type: 'integer'
  is_nullable: 1

=head2 last_modified_time

  data_type: 'integer'
  is_nullable: 1

=head2 last_stolen_time

  data_type: 'integer'
  is_nullable: 1

=head2 board_xml

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "id",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "name",
  { data_type => "text", is_nullable => 1 },
  "meta_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "owner_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "board_time",
  { data_type => "integer", is_nullable => 1 },
  "last_modified_time",
  { data_type => "integer", is_nullable => 1 },
  "last_stolen_time",
  { data_type => "integer", is_nullable => 1 },
  "board_xml",
  { data_type => "text", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("id");

=head1 RELATIONS

=head2 locks

Type: has_many

Related object: L<Zoo::Schema::Result::Lock>

=cut

__PACKAGE__->has_many(
  "locks",
  "Zoo::Schema::Result::Lock",
  { "foreign.world_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 meta_rel

Type: belongs_to

Related object: L<Zoo::Schema::Result::WorldMeta>

=cut

__PACKAGE__->belongs_to(
  "meta_rel",
  "Zoo::Schema::Result::WorldMeta",
  { id => "meta_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "SET NULL",
    on_update     => "SET NULL",
  },
);

=head2 owner

Type: belongs_to

Related object: L<Zoo::Schema::Result::User>

=cut

__PACKAGE__->belongs_to(
  "owner",
  "Zoo::Schema::Result::User",
  { id => "owner_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "SET NULL",
    on_update     => "SET NULL",
  },
);


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-09 15:04:14
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:gXEPwPdlWvivwAoLCOPS3g

=head1 METHODS

=head2 board

Returned type: L<Twiggy>

=cut

sub board {
    my ($self) = @_;
    my $twig = Twiggy->new();
    $twig->parse ($self->board_xml);
    return $twig;
}

=head2 voyeur_game

Returned type: L<Twiggy>

=cut

sub voyeur_game {
    my ($self) = @_;
    my $twig = Twiggy->new();
    $twig->parse ($self->meta_rel->voyeur_game_xml);
    return $twig;
}

=head2 owner_game

Returned type: L<Twiggy>

=cut

sub owner_game {
    my ($self) = @_;
    my $twig = Twiggy->new();
    $twig->parse ($self->meta_rel->owner_game_xml);
    return $twig;
}

=head2 guest_game

Returned type: L<Twiggy>

=cut

sub guest_game {
    my ($self) = @_;
    my $twig = Twiggy->new();
    $twig->parse ($self->meta_rel->guest_game_xml);
    return $twig;
}

=head2 active_locks

Returned type: list that should contain zero or one L<Zoo::Schema::Result::Locks> objects

=cut

sub active_locks {
    my ($self) = @_;
    my $current_time = time();
    my @locks = $self->locks->search({'world_id' => $self->id,
				      'expiry_time' => { '>' => $current_time }});
    return @locks;
}

=head2 expired_locks

Returned type: list that should contain zero or one L<Zoo::Schema::Result::Locks> objects

=cut

sub expired_locks {
    my ($self, $user_id) = @_;
    my $current_time = time();
    my @locks = $self->locks->search({'world_id' => $self->id,
				      'owner_id' => $user_id,
				      'delete_time' => { '>' => $current_time },
				      'expiry_time' => { '<=' => $current_time }});
    return @locks;
}

# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
