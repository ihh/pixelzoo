use utf8;
package Zoo::Schema::Result::Tool;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Zoo::Schema::Result::Tool

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

=head1 TABLE: C<tool>

=cut

__PACKAGE__->table("tool");

=head1 ACCESSORS

=head2 id

  data_type: 'integer'
  is_auto_increment: 1
  is_nullable: 0

=head2 name

  data_type: 'varchar'
  is_nullable: 1
  size: 255

=head2 toolbox_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 creator_id

  data_type: 'integer'
  is_foreign_key: 1
  is_nullable: 1

=head2 xml

  data_type: 'text'
  is_nullable: 1

=cut

__PACKAGE__->add_columns(
  "id",
  { data_type => "integer", is_auto_increment => 1, is_nullable => 0 },
  "name",
  { data_type => "varchar", is_nullable => 1, size => 255 },
  "toolbox_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "creator_id",
  { data_type => "integer", is_foreign_key => 1, is_nullable => 1 },
  "xml",
  { data_type => "text", is_nullable => 1 },
);

=head1 PRIMARY KEY

=over 4

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("id");

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

=head2 toolbox

Type: belongs_to

Related object: L<Zoo::Schema::Result::Toolbox>

=cut

__PACKAGE__->belongs_to(
  "toolbox",
  "Zoo::Schema::Result::Toolbox",
  { id => "toolbox_id" },
  {
    is_deferrable => 0,
    join_type     => "LEFT",
    on_delete     => "SET NULL",
    on_update     => "SET NULL",
  },
);


# Created by DBIx::Class::Schema::Loader v0.07036 @ 2013-10-09 15:04:14
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:LQTBpfUSnsfmBN08eCCV2w

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
