use utf8;
package Zoo::Schema::Result::ToolboxTool;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::ToolboxTool

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

=head1 TABLE: C<toolbox_tool>

=cut

__PACKAGE__->table("toolbox_tool");

=head1 ACCESSORS

=head2 toolbox_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=head2 tool_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=head2 is_default

  data_type: 'integer'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "toolbox_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "tool_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "is_default",
  { data_type => "integer", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</toolbox_id>

=item * L</tool_id>

=back

=cut

__PACKAGE__->set_primary_key("toolbox_id", "tool_id");

=head1 RELATIONS

=head2 tool

Type: belongs_to

Related object: L<Zoo::Schema::Result::Tool>

=cut

__PACKAGE__->belongs_to(
  "tool",
  "Zoo::Schema::Result::Tool",
  { id => "tool_id" },
  { is_deferrable => 0, on_delete => "NO ACTION", on_update => "NO ACTION" },
);

=head2 toolbox

Type: belongs_to

Related object: L<Zoo::Schema::Result::Toolbox>

=cut

__PACKAGE__->belongs_to(
  "toolbox",
  "Zoo::Schema::Result::Toolbox",
  { id => "toolbox_id" },
  { is_deferrable => 0, on_delete => "NO ACTION", on_update => "NO ACTION" },
);


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-15 16:13:16
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:OAXbXhDMAncMG6ghhMfNIQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
