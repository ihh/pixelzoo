package Zoo::Schema::Result::ParticleDep;

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

Zoo::Schema::Result::ParticleDep

=cut

__PACKAGE__->table("particle_dep");

=head1 ACCESSORS

=head2 particle_name

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 255

=head2 dep_name

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 255

=cut

__PACKAGE__->add_columns(
  "particle_name",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 255 },
  "dep_name",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 255 },
);
__PACKAGE__->set_primary_key("particle_name", "dep_name");

=head1 RELATIONS

=head2 dep_name

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "dep_name",
  "Zoo::Schema::Result::Particle",
  { name => "dep_name" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 particle_name

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "particle_name",
  "Zoo::Schema::Result::Particle",
  { name => "particle_name" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07010 @ 2011-05-11 12:57:12
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:u5yzxR8X7GQAsUGh7lf2Ew


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
