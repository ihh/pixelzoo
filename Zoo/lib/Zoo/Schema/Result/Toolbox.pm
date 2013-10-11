use utf8;
package Zoo::Schema::Result::Toolbox;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::Toolbox

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

=head1 TABLE: C<toolbox>

=cut

__PACKAGE__->table("toolbox");

=head1 ACCESSORS

=head2 id

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 name

  data_type: 'varchar'
  is_nullable: 1
  size: 255

=head2 max_tools

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "id",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "name",
  { data_type => "varchar", is_nullable => 1, size => 255 },
  "max_tools",
  { data_type => "integer", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("id");

=head1 RELATIONS

=head2 toolbox_tools

Type: has_many

Related object: L<Zoo::Schema::Result::ToolboxTool>

=cut

__PACKAGE__->has_many(
  "toolbox_tools",
  "Zoo::Schema::Result::ToolboxTool",
  { "foreign.toolbox_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 world_meta_guest_toolboxes

Type: has_many

Related object: L<Zoo::Schema::Result::WorldMeta>

=cut

__PACKAGE__->has_many(
  "world_meta_guest_toolboxes",
  "Zoo::Schema::Result::WorldMeta",
  { "foreign.guest_toolbox_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 world_meta_owner_toolboxes

Type: has_many

Related object: L<Zoo::Schema::Result::WorldMeta>

=cut

__PACKAGE__->has_many(
  "world_meta_owner_toolboxes",
  "Zoo::Schema::Result::WorldMeta",
  { "foreign.owner_toolbox_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-10 14:18:32
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:6g+IeFFUD5s/Jeiojzaz8A


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
