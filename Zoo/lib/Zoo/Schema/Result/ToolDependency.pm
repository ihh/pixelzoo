use utf8;
package Zoo::Schema::Result::ToolDependency;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::ToolDependency

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

=head1 TABLE: C<tool_dependency>

=cut

__PACKAGE__->table("tool_dependency");

=head1 ACCESSORS

=head2 tool_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=head2 particle_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 255

=cut

__PACKAGE__->add_columns(
  "tool_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "particle_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 255 },
);

=head1 PRIMARY KEY

=over 4

=item * L</tool_id>

=item * L</particle_id>

=back

=cut

__PACKAGE__->set_primary_key("tool_id", "particle_id");

=head1 RELATIONS

=head2 particle

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "particle",
  "Zoo::Schema::Result::Particle",
  { name => "particle_id" },
  { is_deferrable => 0, on_delete => "RESTRICT", on_update => "RESTRICT" },
);

=head2 tool

Type: belongs_to

Related object: L<Zoo::Schema::Result::Tool>

=cut

__PACKAGE__->belongs_to(
  "tool",
  "Zoo::Schema::Result::Tool",
  { id => "tool_id" },
  { is_deferrable => 0, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-30 15:06:46
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:XsMWXUAOtQTG+rmDK4D5iA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
