package Zoo::Schema::Result::World;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use Moose;
use MooseX::NonMoose;
use namespace::autoclean;
extends 'DBIx::Class::Core';

__PACKAGE__->load_components("InflateColumn::DateTime");

=head1 NAME

Zoo::Schema::Result::World

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

=head2 owner_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 board_size

  data_type: 'integer'
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

=head2 owner_game_xml

  data_type: 'text'
  is_nullable: 1

=head2 guest_game_xml

  data_type: 'text'
  is_nullable: 1

=head2 voyeur_game_xml

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "id",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "name",
  { data_type => "text", is_nullable => 1 },
  "owner_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "board_size",
  { data_type => "integer", is_nullable => 1 },
  "board_time",
  { data_type => "integer", is_nullable => 1 },
  "last_modified_time",
  { data_type => "integer", is_nullable => 1 },
  "last_stolen_time",
  { data_type => "integer", is_nullable => 1 },
  "board_xml",
  { data_type => "text", is_nullable => 1 },
  "owner_game_xml",
  { data_type => "text", is_nullable => 1 },
  "guest_game_xml",
  { data_type => "text", is_nullable => 1 },
  "voyeur_game_xml",
  { data_type => "text", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("id");

=head1 RELATIONS

=head2 owner

Type: belongs_to

Related object: L<Zoo::Schema::Result::User>

=cut

__PACKAGE__->belongs_to(
  "owner",
  "Zoo::Schema::Result::User",
  { id => "owner_id" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);

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


# Created by DBIx::Class::Schema::Loader v0.07010 @ 2011-05-11 12:59:08
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:FTMn8O3LbDl6hU+LeZZhMA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
