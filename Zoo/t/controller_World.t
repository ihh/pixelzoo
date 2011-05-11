use strict;
use warnings;
use Test::More;


use Catalyst::Test 'Zoo';
use Zoo::Controller::World;

ok( request('/world')->is_success, 'Request should succeed' );
done_testing();
