use utf8;
package Zoo::Schema::Result::ParticleTool;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::ParticleTool

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

=head1 TABLE: C<particle_tool>

=cut

__PACKAGE__->table("particle_tool");

=head1 ACCESSORS

=head2 particle_name

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 255

=head2 tool_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "particle_name",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 255 },
  "tool_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</particle_name>

=back

=cut

__PACKAGE__->set_primary_key("particle_name");

=head1 RELATIONS

=head2 particle_name

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "particle_name",
  "Zoo::Schema::Result::Particle",
  { name => "particle_name" },
  { is_deferrable => 0, on_delete => "NO ACTION", on_update => "NO ACTION" },
);

=head2 tool

Type: belongs_to

Related object: L<Zoo::Schema::Result::Tool>

=cut

__PACKAGE__->belongs_to(
  "tool",
  "Zoo::Schema::Result::Tool",
  { id => "tool_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "NO ACTION",
    on_update     => "NO ACTION",
  },
);


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-29 21:52:57
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:QRTLQT1LmsAfxD+Wv9UzSw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
