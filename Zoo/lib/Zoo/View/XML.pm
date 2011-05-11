package Zoo::View::XML;

use strict;
use warnings;

use base 'Catalyst::View::TT';

__PACKAGE__->config(
    # Change default TT extension
    TEMPLATE_EXTENSION => '.tt2',
    # Set the location for TT files
    INCLUDE_PATH => [
	Zoo->path_to( 'root', 'src' ),
    ],
    render_die => 1,
);

=head1 NAME

Zoo::View::XML - TT View for Zoo

=head1 DESCRIPTION

TT View for Zoo.

=head1 SEE ALSO

L<Zoo>

=head1 AUTHOR

Ian Holmes

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
