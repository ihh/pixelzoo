use utf8;
package Zoo::Schema::Result::Lock;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::Lock

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

=head1 TABLE: C<lock>

=cut

__PACKAGE__->table("lock");

=head1 ACCESSORS

=head2 lock_id

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 world_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 owner_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 create_time

  data_type: 'integer'
  is_nullable: 1

=head2 expiry_time

  data_type: 'integer'
  is_nullable: 1

=head2 delete_time

  data_type: 'integer'
  is_nullable: 1

=head2 toolset_xml

  data_type: (empty string)
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "lock_id",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "world_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "owner_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "create_time",
  { data_type => "integer", is_nullable => 1 },
  "expiry_time",
  { data_type => "integer", is_nullable => 1 },
  "delete_time",
  { data_type => "integer", is_nullable => 1 },
  "toolset_xml",
  { data_type => "", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</lock_id>

=back

=cut

__PACKAGE__->set_primary_key("lock_id");

=head1 RELATIONS

=head2 lock_tools

Type: has_many

Related object: L<Zoo::Schema::Result::LockTool>

=cut

__PACKAGE__->has_many(
  "lock_tools",
  "Zoo::Schema::Result::LockTool",
  { "foreign.lock_id" => "self.lock_id" },
  { cascade_copy => 0, cascade_delete => 0 },
);

=head2 owner

Type: belongs_to

Related object: L<Zoo::Schema::Result::User>

=cut

__PACKAGE__->belongs_to(
  "owner",
  "Zoo::Schema::Result::User",
  { id => "owner_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);

=head2 world

Type: belongs_to

Related object: L<Zoo::Schema::Result::World>

=cut

__PACKAGE__->belongs_to(
  "world",
  "Zoo::Schema::Result::World",
  { id => "world_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);

=head2 tools

Type: many_to_many

Composing rels: L</lock_tools> -> tool

=cut

__PACKAGE__->many_to_many("tools", "lock_tools", "tool");


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-31 17:28:28
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Y/qe/78xtYFAyiW1CN8QUg

=head2 time_to_expire

Returned type: integer time to expiration (seconds)

=cut

sub time_to_expire {
    my ($self) = @_;
    my $current_time = time();
    return $self->expiry_time - $current_time;
}


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
