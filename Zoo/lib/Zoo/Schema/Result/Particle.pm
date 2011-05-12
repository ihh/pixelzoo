package Zoo::Schema::Result::Particle;

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

Zoo::Schema::Result::Particle

=cut

__PACKAGE__->table("particle");

=head1 ACCESSORS

=head2 name

  data_type: 'varchar'
  is_nullable: 0
  size: 255

=head2 image_id

  data_type: 'varchar'
  is_foreign_key: 1
  is_nullable: 1
  size: 255

=head2 cost

  data_type: 'decimal'
  is_nullable: 1

=head2 xml

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "name",
  { data_type => "varchar", is_nullable => 0, size => 255 },
  "image_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 1, size => 255 },
  "cost",
  { data_type => "decimal", is_nullable => 1 },
  "xml",
  { data_type => "text", is_nullable => 1 },
);
__PACKAGE__->set_primary_key("name");

=head1 RELATIONS

=head2 image

Type: belongs_to

Related object: L<Zoo::Schema::Result::Image>

=cut

__PACKAGE__->belongs_to(
  "image",
  "Zoo::Schema::Result::Image",
  { name => "image_id" },
  {
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);

=head2 dependency_descendants

Type: has_many

Related object: L<Zoo::Schema::Result::Dependency>

=cut

__PACKAGE__->has_many(
  "dependency_descendants",
  "Zoo::Schema::Result::Dependency",
  { "foreign.descendant_id" => "self.name" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 dependency_ancestors

Type: has_many

Related object: L<Zoo::Schema::Result::Dependency>

=cut

__PACKAGE__->has_many(
  "dependency_ancestors",
  "Zoo::Schema::Result::Dependency",
  { "foreign.ancestor_id" => "self.name" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 inventories

Type: has_many

Related object: L<Zoo::Schema::Result::Inventory>

=cut

__PACKAGE__->has_many(
  "inventories",
  "Zoo::Schema::Result::Inventory",
  { "foreign.particle_id" => "self.name" },
  { cascade_copy => 0, cascade_delete => 0 },
);


# Created by DBIx::Class::Schema::Loader v0.07010 @ 2011-05-11 21:22:02
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:BWi6JjNp6RCE99dmRvBPEg


=head2 descendants

Type: many_to_many

=cut

__PACKAGE__->many_to_many('descendants' => 'dependency_ancestors', 'descendant');


=head2 ancestors

Type: many_to_many

=cut

__PACKAGE__->many_to_many('ancestors' => 'dependency_descendants', 'ancestor');


=head1 METHODS

=head2 twig

Returned type: L<XML::Twig>

=cut

sub twig {
    my ($self) = @_;
    my $twig = XML::Twig->new();
    $twig->parse ($self->xml);
    return $twig;
}


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
