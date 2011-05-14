package Zoo::Controller::User;
use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller::REST' }

=head1 NAME

Zoo::Controller::User - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut


=head2 index

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    $c->response->body('Matched Zoo::Controller::User in User.');
}


=head2 login

=cut

sub login :Local :Args(2) {
    my ( $self, $c, $username, $password ) = @_;

    # Attempt to log the user in
    if ($c->authenticate({ username => $username,
			   password => $password  } )) {
	# If successful, then let them use the application
	my $uri = $c->uri_for($c->controller('World')->action_for('index'));
        $c->log->debug("URI is $uri");
	$c->response->redirect($uri);
    } else {
	$c->stash(error_msg => "Bad username or password.");
	$c->response->status(403);  # Forbidden
	$c->detach();
    }

    unless ($c->user_exists) {
	$c->stash(error_msg => "Empty username or password.");
	$c->response->status(404);  # Not Found
	$c->detach();
    }
}


=head2 logout

=cut

sub logout :Local :Args(0) {
    my ($self, $c) = @_;

    # Clear the user's state
    $c->logout;

    # 204 No Content
    $c->response->status(204);  # No Content
    $c->detach();
}

=head2 end

=cut

sub end : ActionClass('RenderView') { }


=head1 AUTHOR

Ian Holmes

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

__PACKAGE__->meta->make_immutable;

1;
