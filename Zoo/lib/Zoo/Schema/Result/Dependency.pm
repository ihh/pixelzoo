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

=head2 creator_name

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 255

=head2 downstream_name

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 0
  size: 255

=cut

__PACKAGE__->add_columns(
  "creator_name",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 255 },
  "downstream_name",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 0, size => 255 },
);
__PACKAGE__->set_primary_key("creator_name", "downstream_name");

=head1 RELATIONS

=head2 downstream_name

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "downstream_name",
  "Zoo::Schema::Result::Particle",
  { name => "downstream_name" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 creator_name

Type: belongs_to

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->belongs_to(
  "creator_name",
  "Zoo::Schema::Result::Particle",
  { name => "creator_name" },
  { is_deferrable => 1, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07010 @ 2011-05-11 18:19:39
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:tSSOu3ImGJ9LVkaNny704w


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
