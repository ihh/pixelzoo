#!/usr/bin/env perl -w

use strict;

use FindBin qw($Bin);
use lib "$Bin/../Zoo/lib";

use Getopt::Long;
use Pod::Usage;
use XML::Twig;
use Carp;

use Level::Simple;

# parse options
my $man = 0;
my $help = 0;
my $debug = 0;
my $verbose = 0;
my $xmllint;
my ($proto, $out);

GetOptions('help|?' => \$help, man => \$man, verbose => \$verbose, debug => \$debug,
	   'out=s' => \$out, 'proto=s' => \$proto, 'xmllint=s' => \$xmllint) or pod2usage(2);
pod2usage(1) if $help;
pod2usage(-exitstatus => 0, -verbose => 2) if $man;

# pod2usage(2) unless @ARGV == 1;

$verbose = 1 if $debug;


# test
my $gram = Level::Simple->newLevel;
$gram->verbose($verbose);
$gram->debug($debug);
$gram->protofile($proto) if defined $proto;
$gram->outfile($out) if defined $proto;
$gram->xmllint($xmllint) if defined $xmllint;


# polymer cage builders
# outline of program:

# switch (state)
#  case 0: (paused)
#   nop

#  case 1: (init)
#   set orig.steps = orig.edge_len
#   set orig.edges = 3
#   set orig.tail_state = 0 (paused)
#   set orig.state = 2 (build)

#  case 2: (build)
#   switch (orig.r_dir)  (loop over neighborhood)
#    if (steps = 0)
#     if (edges = 0)
#      bind (r_pos = neighborhood[r_dir])
#       case polymer:  (connect the ends)
#        switch (r_pos.state)
#         case 0: (paused)
#          set r_pos.state = 3 (active)
#          set r_pos.l_bond = 1
#          set r_pos.l_dir = orig.r_dir + 4  (mutual bond directions)
#          set orig.state = 3 (active)
#          set orig.r_bond = 1
#    else (edges > 0)
#     set orig.r_dir = orig.r_dir + 2  (turn right 90 degrees)
#     set orig.steps = orig.edge_len
#     set orig.edges = orig.edges - 1
#     if (orig.edges = 0)
#      set orig.steps = orig.steps - 1
#   else (steps > 0)
#    bind (r_pos = neighborhood[r_dir])
#     case empty:
#      set r_pos = orig
#      set orig.state = orig.tail_state
#      set orig.r_bond = 1
#      set r_pos.l_bond = 1
#      set r_pos.l_dir = orig.r_dir + 4  (mutual bond directions)
#      set r_pos.tail_state = 3 (active)
#      decrement r_pos.steps

#  case 3: (active)
#   poly_rule

# where...
# poly_rule:
#  if (l_bond)
#   verify_or_die (l_dir, r_dir)
#   if (r_bond)
#    verify_or_die (r_dir, l_dir)
#    random_lr_step
#   else
#    random_l_step
#  else
#   if (r_bond)
#    verify_or_die (r_dir, l_dir)
#    random_r_step
#  else
#   random_step

my @moore_dir2xy = map ([$gram->dir->{$_}->x, $gram->dir->{$_}->y], qw(n ne e se s sw w nw));

sub poly_rule {
    my ($gram, $type) = @_;
    return
	$gram->switchRule
	(undef, 'l_bond',
	 { 1 => verify_or_die_l
	       ($gram, $type,
		sub {
		    my ($l_dir) = @_;
		    $gram->switchRule
			(undef, 'r_bond',
			 { 1 => verify_or_die_r
			       ($gram, $type,
				sub {
				    my ($r_dir) = @_;
				    random_lr_step ($gram, $type, $l_dir, $r_dir);
				}),
			   0 => random_l_step ($gram, $type, $l_dir) })}),
	   0 => $gram->switchRule
	       (undef, 'r_bond',
		{1 => verify_or_die_r
		     ($gram, $type,
		      sub {
			  my ($r_dir) = @_;
			  random_r_step ($gram, $type, $r_dir);
		      }),
		 0 => $gram->suicide }) });
}

sub poly_build_rule {
    my ($gram, $type) = @_;

    return
# switch (state)
	$gram->switchRule
	('orig', 'state',
#  case 0: (paused)
#   nop
	 { 0 => $gram->nopRule,

#  case 1: (init)
	   1 =>
#   set orig.steps = orig.edge_len
	       $gram->incRule
	       ('orig', 'edge_len', 'orig', 'steps', 0,
#   set orig.edges = 3
		$gram->setRule
		('orig', 'edges', 3,
#   set orig.tail_state = 0 (paused)
		 $gram->setRule
		 ('orig', 'tail_state', 0,
#   set orig.state = 2 (build)
		  $gram->setRule
		  ('orig', 'state', 2)))),

#  case 2: (build)
		  2 =>
#   switch (orig.r_dir)
		  $gram->switchRule
		  ('orig', 'r_dir',
# (loop over neighborhood)
		   { map (($_ =>
#    if (steps = 0)
			   $gram->switchRule
			   ('orig', 'steps',
			    { 0 =>
#     if (edges = 0)
				  $gram->switchRule
				  ('orig', 'edges',
				   { 0 =>
#      bind (r_pos = neighborhood[r_dir])
					 $gram->bindRule
					 ('r_pos', @{$moore_dir2xy[$_]},
#       case polymer:  (connect the ends)
					  { $type =>
#        switch (r_pos.state)
						$gram->switchRule
						('r_pos', 'state',
#         case 0: (paused)
						 { 0 =>
#          set r_pos.state = 3 (active)
						       $gram->setRule
						       ('r_pos', 'state', 3,
#          set r_pos.l_bond = 1
							$gram->setRule
							('r_pos', 'l_bond', 1,
#          set r_pos.l_dir = orig.r_dir + 4  (mutual bond directions)
							 $gram->incRule
							 ('orig', 'r_dir', 'r_pos', 'l_dir', 4,
#          set orig.state = 3 (active)
							  $gram->setRule
							  ('orig', 'state', 3,
#          set orig.r_bond = 1
							   $gram->setRule
							   ('orig', 'r_bond', 1)))))})})},

#    else (edges > 0)
#     set orig.r_dir = orig.r_dir + 2  (turn right 90 degrees)
				   $gram->incRule
				   ('orig', 'r_dir', 'orig', 'r_dir', 2,
#     set orig.steps = orig.edge_len
				    $gram->incRule
				    ('orig', 'edge_len', 'orig', 'steps', 0,
#     set orig.edges = orig.edges - 1
				     $gram->incRule
				     ('orig', 'edges', 'orig', 'edges', -1,
#     if (orig.edges = 0)
				      $gram->switchRule
				      ('orig', 'edges',
				       { 0 =>
#      set orig.steps = orig.steps - 1
					     $gram->incRule
					     ('orig', 'steps', 'orig', 'steps', -1)})))))},

#   else (steps > 0)
#    bind (r_pos = neighborhood[r_dir])
			    $gram->bindRule
			    ('r_pos', @{$moore_dir2xy[$_]},
#     case empty:
			     { 'empty' =>
#      set r_pos = orig
				   $gram->copyTo
				   ('r_pos',
#      set orig.state = orig.tail_state
				    $gram->incRule
				    ('orig', 'tail_state', 'orig', 'state', 0,
#      set orig.r_bond = 1
				     $gram->setRule
				     ('orig', 'r_bond', 1,
#      set r_pos.l_bond = 1
				      $gram->setRule
				      ('r_pos', 'l_bond', 1,
#      set r_pos.l_dir = orig.r_dir + 4  (mutual bond directions)
				       $gram->incRule
				       ('orig', 'r_dir', 'r_pos', 'l_dir', 4,
#      set r_pos.tail_state = 3 (active)
					$gram->setRule
					('r_pos', 'tail_state', 3,
#      decrement r_pos.steps
					 $gram->incRule
					 ('r_pos', 'steps', 'r_pos', 'steps', -1)))))))}))),

# (end loop over neighborhood)
			  0..$#moore_dir2xy)}),
	 
#  case 3: (active)
#   poly_rule
			   3 => poly_rule ($gram, $type) });
}

sub moore_xy2dir {
    my ($xy) = @_;
    my ($x, $y) = @$xy;
    for (my $dir = 0; $dir < @moore_dir2xy; ++$dir) {
	if ($moore_dir2xy[$dir]->[0] == $x && $moore_dir2xy[$dir]->[1] == $y) {
	    return $dir;
	}
    }
    die "Can't turn ($x,$y) into a Moore direction index";
}

sub test_moore_neighbors {
    my ($xy1, $xy2) = @_;
    my ($x1, $y1, $x2, $y2) = (@$xy1, @$xy2);
    return abs($x1-$x2) <= 1 && abs($y1-$y2) <= 1 && !($x1 == $x2 && $y1 == $y2);
}

sub delta_dir {
    my ($xy1, $xy2) = @_;
    my ($x1, $y1, $x2, $y2) = (@$xy1, @$xy2);
    return moore_xy2dir ([$x2 - $x1, $y2 - $y1]);
}

sub set_neighbor_dir {
    my ($gram, $nbr_loc, $nbr_dir_var, $nbr_pos, $step_pos, $next) = @_;
    my $new_nbr_dir = delta_dir ($nbr_pos, $step_pos);
    return $gram->setRule ($nbr_loc, $nbr_dir_var, $new_nbr_dir, $next);
}

sub set_lpos_rdir {
    my ($gram, $nbr_pos, $step_pos, $next) = @_;
    return set_neighbor_dir ($gram, 'l_nbr', 'r_dir', $nbr_pos, $step_pos, $next);
}

sub set_rpos_ldir {
    my ($gram, $nbr_pos, $step_pos, $next) = @_;
    return set_neighbor_dir ($gram, 'r_nbr', 'l_dir', $nbr_pos, $step_pos, $next);
}

sub poly_type_and_vars {
    my ($gram, $type, $ldir, $rdir) = @_;
    return [ 'type' => $type,
	     $gram->var('state') => 3,
	     defined($ldir)
	     ? ($gram->var('l_bond') => 1, $gram->var('l_dir') => $ldir)
	     : ($gram->var('l_bond') => 0),
	     defined($rdir)
	     ? ($gram->var('r_bond') => 1, $gram->var('r_dir') => $rdir)
	     : ($gram->var('r_bond') => 0) ];
}

sub random_lr_step {
    my ($gram, $type, $ldir, $rdir) = @_;
    my ($lpos, $rpos) = @moore_dir2xy[$ldir,$rdir];
    my @potentials = grep (test_moore_neighbors ($lpos, $_) && test_moore_neighbors ($rpos, $_), @moore_dir2xy);
    my @step = map ($gram->bindRule
		    ('step_pos',
		     @$_,
		     { $gram->empty =>
			   set_lpos_rdir
			   ($gram, $lpos, $_,
			    set_rpos_ldir
			    ($gram, $rpos, $_,
			     $gram->setRule
			     ('step_pos',
			      undef,
			      poly_type_and_vars ($gram, $type,
						  delta_dir ($_, $lpos),
						  delta_dir ($_, $rpos)),
			      $gram->suicide))) }),
		    @potentials);
    return $gram->uniformHuffRule (@step);
}

sub random_l_step {
    my ($gram, $type, $ldir) = @_;
    my $lpos = $moore_dir2xy[$ldir];
    my @potentials = grep (test_moore_neighbors ($lpos, $_), @moore_dir2xy);
    my @step = map ($gram->bindRule
		    ('step_pos',
		     @$_,
		     { $gram->empty =>
			   set_lpos_rdir
			   ($gram, $lpos, $_,
			    $gram->setRule
			    ('step_pos',
			     undef,
			     poly_type_and_vars ($gram, $type,
						 delta_dir ($_, $lpos),
						 undef),
			     $gram->suicide)) }),
		    @potentials);
    return $gram->uniformHuffRule (@step);
}

sub random_r_step {
    my ($gram, $type, $rdir) = @_;
    my $rpos = $moore_dir2xy[$rdir];
    my @potentials = grep (test_moore_neighbors ($rpos, $_), @moore_dir2xy);
    my @step = map ($gram->bindRule
		    ('step_pos',
		     @$_,
		     { $gram->empty =>
			   set_rpos_ldir
			   ($gram, $rpos, $_,
			    $gram->setRule
			    ('step_pos',
			     undef,
			     poly_type_and_vars ($gram, $type,
						 undef,
						 delta_dir ($_, $rpos)),
			     $gram->suicide)) }),
		    @potentials);
    return $gram->uniformHuffRule (@step);
}

sub verify_or_die_l {
    my ($gram, $type, $next) = @_;
    return verify_or_die ($gram, $type, 'l_bond', 'l_dir', 'l_nbr', 'r_bond', 'r_dir', $next);
}

sub verify_or_die_r {
    my ($gram, $type, $next) = @_;
    return verify_or_die ($gram, $type, 'r_bond', 'r_dir', 'r_nbr', 'l_bond', 'l_dir', $next);
}

sub verify_or_die {
    my ($gram, $type, $bond_var, $dir_var, $nbr_loc, $nbr_bond_var, $nbr_dir_var, $next_sub) = @_;
    return $gram->switchRule (undef, $dir_var, { map (($_ =>
						     verify_pos_or_die
						     ($gram, $type, $bond_var, $moore_dir2xy[$_], $nbr_loc, $nbr_bond_var, $nbr_dir_var, &$next_sub($_))),
						    0..$#moore_dir2xy) });
}

sub verify_pos_or_die {
    my ($gram, $type, $bond_var, $nbr_pos, $nbr_loc, $nbr_bond_var, $nbr_dir_var, $next) = @_;
    my $unbond = $gram->setRule (undef, $bond_var, 0);
    return $gram->bindRule
	($nbr_loc,
	 @$nbr_pos,
	 { $type =>
	       $gram->switchRule ($nbr_loc,
				  $nbr_bond_var,
				  { 0 => $unbond,
				    1 => $gram->switchRule
					($nbr_loc,
					 $nbr_dir_var,
					 { delta_dir ($nbr_pos, [0,0]) => $next },
					 $unbond) }) },
	 $unbond);
}

my ($polyName, $polyRate) = ('polymer', .1);
$gram->addType ('name' => $polyName,
		'vars' => [ $gram->var('l_bond') => 1,
			    $gram->var('l_dir') => 3,
			    $gram->var('r_bond') => 1,
			    $gram->var('r_dir') => 3,
			    $gram->var('state') => 2,
			    $gram->var('tail_state') => 2,
			    $gram->var('edge_len') => 6,
			    $gram->var('edges') => 2,
			    $gram->var('steps') => 6 ],
		'hue' => ['add' => 20],
		'sat' => ['add' => 255],
		'bri' => ['add' => 255],
		'rate' => $polyRate,
		'rule' => poly_build_rule ($gram, $polyName));

# polymer tool
$gram->addTool ('name' => 'Polymer spray',
		'size' => 4,
		'gvars' => [ 'type' => $polyName, $gram->var('r_dir') => 2, $gram->var('edge_len') => 10, $gram->var('state') => 1 ],
		'reserve' => 1,
		'recharge' => 100,
		'spray' => 2500,
		'overwrite' => [ 'gstate' => 'empty' ]);




# add the simpletest stuff

# cement
my %cement = ('name' => 'cement',
	      'rate' => .1,
	      'step' => .02,
	      'drain' => .001,
	      'stick' => 1,
	      'sticksto' => 'wall',
	      'copies' => 1,
	      'set' => .02,
	      'setsto' => 'wall',
	      'setsvar' => 'hue',
	      'setsvals' => [0,42,84]);

$gram->make_spray_tool(%cement);


# wall
my ($wallRate, $wallMaxDecay) = (.0002, 15);
$gram->make_wall ($wallRate, $wallMaxDecay);

# acid
my ($acidRate, $acidDrain, $acidBurn) = (.1, .03, .5);
$gram->make_acid($acidRate, $acidDrain, $acidBurn);

# acid tool
$gram->addTool ('name' => 'Acid spray',
		'size' => 16,
		'gstate' => 'acid',
		'reserve' => 1000,
		'recharge' => 100,
		'spray' => 2,
		'overwrite' => [ 'gstate' => 'empty' ]);

# plant
my %plant = ('rate' => .01, 'branch' => .2, 'die' => .0001, 'max_branches' => 3);
$gram->make_plant (%plant);

# seed
my %seed = ('name' => 'seed',
	    'rate' => .1,
	    'step' => .05,
	    'drain' => .03,
	    'stick' => 1,
	    'sticksto' => 'wall',
	    'copies' => 0,
	    'set' => .0005,
	    'setsto' => 'plant',
	    'setsvar' => 'gens_left',
	    'setsvals' => [1,3,6]);

$gram->make_spray_tool(%seed);


# rock-paper-scissors animal

my %rps = ('name' => 'cyclobs',
	   'rate' => .08,
	   'food' => 'plant',
	   'food_unripeness_var' => 'gens_left',

	   # if next to empty space...
	   'step' => .2,
	   'breed' => .005,
	   'die' => .005,

	   # if next to same species...
	   'choke' => .003,

	   # if next to prey species, or food...
	   'eat' => .2,
	   'convert' => .8,

	   # text feedback rates
	   'text' => .001,
	   'log' => 1);

$gram->make_rps_animal (%rps);

# rps animal tool
$gram->addTool ('name' => $rps{'name'},
		'size' => 8,
		'gvars' => [ 'type' => $rps{'name'}, $gram->var('species') => 3 ],
		'reserve' => 5,
		'recharge' => 100,
		'spray' => 100,
		'overwrite' => [ 'gstate' => 'empty' ]);

# perfume
my ($animalName, $perfumeRate, $perfumeDrain, $perfumeBillow, $perfumeInduce) = ($rps{'name'}, .6, .03, .03, .5);
$gram->make_perfume ($animalName, $perfumeRate, $perfumeDrain, $perfumeBillow, $perfumeInduce);

# perfume tool
$gram->addTool ('name' => 'Perfume spray',
		'size' => 4,
		'gstate' => 'perfume',
		'reserve' => 1000,
		'recharge' => 100,
		'spray' => 2500,
		'overwrite' => [ 'gstate' => 'empty' ]);


# print
$gram->print;


__END__

=head1 NAME
polyzoo.pl - generate a PixelZoo XML grammar

=head1 SYNOPSIS

polyzoo.pl [options]

 Options:
  -help               brief help message
  -man                full documentation
  -proto              file to save proto-XML
  -out                file to save output XML (otherwise sent to stdout)
  -xmllint            path to xmllint
  -verbose            report progress
  -debug              more info than you want

=head1 OPTIONS

=over 8

=item B<-help>

Prints a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=item B<-xmllint>

Specify the path to xmllint.

=item B<-proto>

Specify a filename to save the proto-XML.

=item B<-out>

Specify a filename to save the output XML.
If no filename is specified, it will be printed to standard output.

=back

=head1 DESCRIPTION

B<This program> will generate a PixelZoo XML grammar.

=cut
