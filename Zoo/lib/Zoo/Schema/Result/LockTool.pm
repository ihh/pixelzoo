use utf8;
package Zoo::Schema::Result::LockTool;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::LockTool

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

=head1 TABLE: C<lock_tool>

=cut

__PACKAGE__->table("lock_tool");

=head1 ACCESSORS

=head2 lock_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=head2 tool_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "lock_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
  "tool_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</lock_id>

=item * L</tool_id>

=back

=cut

__PACKAGE__->set_primary_key("lock_id", "tool_id");

=head1 RELATIONS

=head2 lock

Type: belongs_to

Related object: L<Zoo::Schema::Result::Lock>

=cut

__PACKAGE__->belongs_to(
  "lock",
  "Zoo::Schema::Result::Lock",
  { lock_id => "lock_id" },
  { is_deferrable => 0, on_delete => "CASCADE", on_update => "CASCADE" },
);

=head2 tool

Type: belongs_to

Related object: L<Zoo::Schema::Result::Tool>

=cut

__PACKAGE__->belongs_to(
  "tool",
  "Zoo::Schema::Result::Tool",
  { id => "tool_id" },
  { is_deferrable => 0, on_delete => "CASCADE", on_update => "CASCADE" },
);


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-31 17:28:28
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:Fm4rLErXQQ/SUzok/MBCKQ


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
