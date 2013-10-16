package Zoo::Controller::User;
use Moose;
use Twiggy;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller::REST' }

__PACKAGE__->config(default => 'text/xml');

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

Test user's password credentials, supplied by Basic HTTP Authentication.

=cut

sub login :Local :Args(0) {
    my ( $self, $c ) = @_;

    # Test user's credentials
    $c->authenticate({});

    # 204 No Content
    $c->response->status(204);  # No Content
    $c->detach();
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


=head2 status

=cut

sub user :Chained('/') :PathPart('user') :CaptureArgs(0) {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'empty.tt2';   # start of chain: default to empty view template
}

sub status :Chained('user') :PathPart('status') :CaptureArgs(0) { }

sub status_end :Chained('status') :PathPart('') :Args(0) :ActionClass('REST') { }

sub status_end_POST {
    my ( $self, $c ) = @_;
    if ($c->request->content_length) {
	my $status_twig = Twiggy->new();
	$status_twig->parse ($c->request->body);
	my $username = $status_twig->root->first_child("user")->text;
	my $password = $status_twig->root->first_child("pass")->text;
	if ($c->model('DB')->users_by_name($username)) {
	    $c->response->status(409);  # 409 CONFLICT
	    $c->detach();
	} else {
	    my $user = $c->model('DB::User')->create({
		username => $username,
		password => $password,
		cash => 0 });
	    $c->stash->{user} = $user;
	    $c->stash->{template} = 'user/status_auth.tt2';
	    $c->response->status(201);  # 201 CREATED
	}
    }
}

sub status_end_GET {
    my ( $self, $c ) = @_;
    $c->authenticate({});
    $c->stash->{user} = $c->user;
    $c->stash->{template} = 'user/status.tt2';
}

sub status_id :Chained('user') :PathPart('status') :CaptureArgs(1) {
    my ( $self, $c, $user_id ) = @_;
    my $user = $c->model('DB')->user_by_id($user_id);
    if (!defined $user) {
	$c->stash( error_msg => "User ID $user_id does not exist" );
	$c->response->status(404);
	$c->detach();
    }
    $c->stash->{user} = $user;
}

sub status_id_end :Chained('status_id') :PathPart('') :Args(0) :ActionClass('REST') { }

sub status_id_end_GET {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'user/status.tt2';
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
