use utf8;
package Zoo::Schema::Result::User;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::User

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

=head1 TABLE: C<user>

=cut

__PACKAGE__->table("user");

=head1 ACCESSORS

=head2 id

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 username

  data_type: 'varchar'
  is_nullable: 1
  size: 15

=head2 password

  data_type: 'text'
  is_nullable: 1

=head2 cash

  data_type: 'decimal'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "id",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "username",
  { data_type => "varchar", is_nullable => 1, size => 15 },
  "password",
  { data_type => "text", is_nullable => 1 },
  "cash",
  { data_type => "decimal", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("id");

=head1 RELATIONS

=head2 images

Type: has_many

Related object: L<Zoo::Schema::Result::Image>

=cut

__PACKAGE__->has_many(
  "images",
  "Zoo::Schema::Result::Image",
  { "foreign.creator_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 inventories

Type: has_many

Related object: L<Zoo::Schema::Result::Inventory>

=cut

__PACKAGE__->has_many(
  "inventories",
  "Zoo::Schema::Result::Inventory",
  { "foreign.user_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 locks

Type: has_many

Related object: L<Zoo::Schema::Result::Lock>

=cut

__PACKAGE__->has_many(
  "locks",
  "Zoo::Schema::Result::Lock",
  { "foreign.owner_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 particles

Type: has_many

Related object: L<Zoo::Schema::Result::Particle>

=cut

__PACKAGE__->has_many(
  "particles",
  "Zoo::Schema::Result::Particle",
  { "foreign.creator_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 tools

Type: has_many

Related object: L<Zoo::Schema::Result::Tool>

=cut

__PACKAGE__->has_many(
  "tools",
  "Zoo::Schema::Result::Tool",
  { "foreign.creator_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 user_roles

Type: has_many

Related object: L<Zoo::Schema::Result::UserRole>

=cut

__PACKAGE__->has_many(
  "user_roles",
  "Zoo::Schema::Result::UserRole",
  { "foreign.user_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 worlds

Type: has_many

Related object: L<Zoo::Schema::Result::World>

=cut

__PACKAGE__->has_many(
  "worlds",
  "Zoo::Schema::Result::World",
  { "foreign.owner_id" => "self.id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 roles

Type: many_to_many

Composing rels: L</user_roles> -> role

=cut

__PACKAGE__->many_to_many("roles", "user_roles", "role");


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-09 12:07:56
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:iKtxv2CyB+vmiOmj/FBYYQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
