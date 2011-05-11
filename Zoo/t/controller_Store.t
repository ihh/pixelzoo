use strict;
use warnings;
use Test::More;


use Catalyst::Test 'Zoo';
use Zoo::Controller::Store;

ok( request('/store')->is_success, 'Request should succeed' );
done_testing();
