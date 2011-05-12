package Zoo::Schema::Result::Dependency;

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

Zoo::Schema::Result::Dependency

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
__PACKAGE__->set_primary_key("ancestor_id", "descendant_id");

=head1 RELATIONS

=head2 descendant

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "descendant",
  "Zoo::Schema::Result::Particle",
  { name => "descendant_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 ancestor

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "ancestor",
  "Zoo::Schema::Result::Particle",
  { name => "ancestor_id" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07010 @ 2011-05-11 21:22:02
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:1hfcVaexrriPFHLjJYb/2A


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
