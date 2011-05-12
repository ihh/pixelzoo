package Zoo::Schema::Result::Tool;

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

Zoo::Schema::Result::Tool

=cut

__PACKAGE__->table("tool");

=head1 ACCESSORS

=head2 name

  data_type: 'varchar'
  is_nullable: 0
  size: 255

=head2 creator_id

  data_type: 'integer'
  is_foreign_key: 1
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
  "xml",
  { data_type => "text", is_nullable => 1 },
);
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
    is_deferrable => 1,
    join_type     => "LEFT",
    on_delete     => "CASCADE",
    on_update     => "CASCADE",
  },
);


# Created by DBIx::Class::Schema::Loader v0.07010 @ 2011-05-11 23:12:05
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:BLhMlSA7yleX56mi9gW+9Q

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
