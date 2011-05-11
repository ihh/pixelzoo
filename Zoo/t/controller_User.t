use strict;
use warnings;
use Test::More;


use Catalyst::Test 'Zoo';
use Zoo::Controller::User;

ok( request('/user')->is_success, 'Request should succeed' );
done_testing();
