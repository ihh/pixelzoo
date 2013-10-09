use utf8;
package Zoo::Schema::Result::Particle;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::Particle

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

=head1 TABLE: C<particle>

=cut

__PACKAGE__->table("particle");

=head1 ACCESSORS

=head2 name

  data_type: 'varchar'
  is_nullable: 0
  size: 255

=head2 creator_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

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
  "creator_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "image_id",
  { data_type => "varchar", is_foreign_key => 1, is_nullable => 1, size => 255 },
  "cost",
  { data_type => "decimal", is_nullable => 1 },
  "xml",
  { data_type => "text", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</name>

=back

=cut

__PACKAGE__->set_primary_key("name");

=head1 RELATIONS

=head2 creator

Type: belongs_to

Related object: L<Zoo::Schema::Result::User>

=cut

__PACKAGE__->belongs_to(
  "creator",
  "Zoo::Schema::Result::User",
  { id => "creator_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
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

=head2 image

Type: belongs_to

Related object: L<Zoo::Schema::Result::Image>

=cut

__PACKAGE__->belongs_to(
  "image",
  "Zoo::Schema::Result::Image",
  { name => "image_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "SET NULL",
    on_update     => "CASCADE",
  },
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

=head2 ancestors

Type: many_to_many

Composing rels: L</dependency_descendants> -> ancestor

=cut

__PACKAGE__->many_to_many("ancestors", "dependency_descendants", "ancestor");

=head2 descendants

Type: many_to_many

Composing rels: L</dependency_descendants> -> descendant

=cut

__PACKAGE__->many_to_many("descendants", "dependency_descendants", "descendant");


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-09 12:07:56
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:d/fMzjOLvv368Ke94p1X9Q


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

Returned type: L<Twiggy>

=cut

sub twig {
    my ($self) = @_;
    my $twig = Twiggy->new();
    $twig->parse ($self->xml);
    return $twig;
}

=head2 nest

=cut

sub nest {
    my ($self) = @_;
    my @nest = $self->twig->twig_nest;
    return @nest == 2 ? @{$nest[1]} : ();
}


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
