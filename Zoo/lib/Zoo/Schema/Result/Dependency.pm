use utf8;
package Zoo::Schema::Result::Dependency;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::Dependency

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

=head1 TABLE: C<dependency>

=cut

__PACKAGE__->table("dependency");

=head1 ACCESSORS

=head2 ancestor_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 255

=head2 descendant_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 255

=cut

__PACKAGE__->add_columns(
  "ancestor_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 255 },
  "descendant_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 255 },
);

=head1 PRIMARY KEY

=over 4

=item * L</ancestor_id>

=item * L</descendant_id>

=back

=cut

__PACKAGE__->set_primary_key("ancestor_id", "descendant_id");

=head1 RELATIONS

=head2 ancestor

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "ancestor",
  "Zoo::Schema::Result::Particle",
  { name => "ancestor_id" },
  { is_deferrable => 0, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 descendant

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "descendant",
  "Zoo::Schema::Result::Particle",
  { name => "descendant_id" },
  { is_deferrable => 0, on_delete => "RESTRICT", on_update => "RESTRICT" },
);


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-09 12:07:56
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:ZQb195XXkZPt43XObpDYiw


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
