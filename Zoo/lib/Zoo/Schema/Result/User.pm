package Zoo::Schema::Result::User;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

use strict;
use warnings;

use Moose;
use MooseX::NonMoose;
use namespace::autoclean;
extends 'DBIx::Class::Core';

__PACKAGE__->load_components("InflateColumn::DateTime", "TimeStamp");

=head1 NAME

Zoo::Schema::Result::User

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
__PACKAGE__->set_primary_key("id");

=head1 RELATIONS

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


# Created by DBIx::Class::Schema::Loader v0.07010 @ 2011-05-10 18:44:49
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:yCJx/7UVqDgJ0llfgZswLA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
