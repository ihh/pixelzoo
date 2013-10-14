package Zoo;
use Moose;
use namespace::autoclean;

use Catalyst::Runtime 5.80;

# Set flags and add plugins for the application.
#
# Note that ORDERING IS IMPORTANT here as plugins are initialized in order,
# therefore you almost certainly want to keep ConfigLoader at the head of the
# list if you're using it.
#
#         -Debug: activates the debug mode for very useful log messages
#   ConfigLoader: will load the configuration from a Config::General file in the
#                 application's home directory
# Static::Simple: will serve static files from the application's root
#                 directory

use Catalyst qw/
    -Debug
    ConfigLoader
    Static::Simple

    StackTrace
            
    Authentication
    Authentication::Credential::HTTP

    Session
    Session::Store::FastMmap
    Session::State::Cookie
/;

extends 'Catalyst';

our $VERSION = '0.01';

# Configure the application.
#
# Note that settings in zoo.conf (or other external
# configuration file that you set up manually) take precedence
# over this when using ConfigLoader. Thus configuration
# details given here can function as a default configuration,
# with an external configuration file acting as an override for
# local deployment.

__PACKAGE__->config(
    name => 'Zoo',
    # Disable deprecated behavior needed by old applications
    disable_component_resolution_regex_fallback => 1,
);

__PACKAGE__->config->{authentication} =
{
    default_realm => 'members',
    realms => {
	members => {
		credential => {
		    class => 'HTTP',
		    type => 'basic',
		    password_type => 'clear',
		    password_field => 'password'
		},
			store => {
			    class => 'DBIx::Class',
			    user_model => 'DB::User',
			    role_relation => 'roles',
			    role_field => 'role_id'
		    }
	    }
	}
};

# Start the application
__PACKAGE__->setup();


=head1 NAME

Zoo - Catalyst based application

=head1 SYNOPSIS

    script/zoo_server.pl

=head1 DESCRIPTION

[enter your description here]

=head1 SEE ALSO

L<Zoo::Controller::Root>, L<Catalyst>

=head1 AUTHOR

Ian Holmes

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
