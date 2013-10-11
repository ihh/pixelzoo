use utf8;
package Zoo::Schema::Result::WorldMeta;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::WorldMeta

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

=head1 TABLE: C<world_meta>

=cut

__PACKAGE__->table("world_meta");

=head1 ACCESSORS

=head2 id

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 board_size

  data_type: 'integer'
  is_nullable: 1

=head2 owner_toolbox_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 guest_toolbox_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 lock_expiry_delay

  data_type: 'integer'
  is_nullable: 1

=head2 lock_delete_delay

  data_type: 'integer'
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
  "board_size",
  { data_type => "integer", is_nullable => 1 },
  "owner_toolbox_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "guest_toolbox_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "lock_expiry_delay",
  { data_type => "integer", is_nullable => 1 },
  "lock_delete_delay",
  { data_type => "integer", is_nullable => 1 },
  "owner_game_xml",
  { data_type => "text", is_nullable => 1 },
  "guest_game_xml",
  { data_type => "text", is_nullable => 1 },
  "voyeur_game_xml",
  { data_type => "text", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("id");

=head1 RELATIONS

=head2 guest_toolbox

Type: belongs_to

Related object: L<Zoo::Schema::Result::Toolbox>

=cut

__PACKAGE__->belongs_to(
  "guest_toolbox",
  "Zoo::Schema::Result::Toolbox",
  { id => "guest_toolbox_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "SET NULL",
    on_update     => "SET NULL",
  },
);

=head2 owner_toolbox

Type: belongs_to

Related object: L<Zoo::Schema::Result::Toolbox>

=cut

__PACKAGE__->belongs_to(
  "owner_toolbox",
  "Zoo::Schema::Result::Toolbox",
  { id => "owner_toolbox_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "SET NULL",
    on_update     => "SET NULL",
  },
);

=head2 worlds

Type: has_many

Related object: L<Zoo::Schema::Result::World>

=cut

__PACKAGE__->has_many(
  "worlds",
  "Zoo::Schema::Result::World",
  { "foreign.meta_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-09 15:04:14
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:fBKgWIzLiX/3eIH0kS6vcw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
