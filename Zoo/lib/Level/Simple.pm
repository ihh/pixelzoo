#!/usr/bin/perl -w

package Level::Simple;

use strict;
use vars ('@ISA');

use Exporter;
use Carp qw(carp croak cluck confess);

use AutoHash;
use Grammar;
use Level;

push @ISA, qw (Level);

sub make_spray_tool {
    my ($gram, %cement) = @_;

    my ($cementName, $cementRate, $cementStep, $cementDrain, $cementStick, $cementSet,
	$stickType, $copyFlag, $setType, $wallVar, $wallVals)
	= map ($cement{$_}, qw(name rate step drain stick set sticksto copies setsto setsvar setsvals));

    # cement type
    my $setSub = ['huff' =>
		  [ map ((1/@$wallVals =>
			  [ 'modify' => [ 'set' => [ 'type' => $setType, $gram->var($wallVar) => $_ ] ] ]),
			 @$wallVals) ]];
    my $stickSub = $copyFlag
	? ['modify' => [ 'src' => [ 'loc' => $gram->neighbor ], 'dest' => [ 'loc' => $gram->origin ] ] ]
	: $setSub;

    $gram->addType ('name' => $cementName,
		    'hue' => ['add' => 32],
		    'sat' => ['add' => 192],
		    'bri' => ['add' => 96],
		    'rate' => $cementRate,
		    'rule' => ['huff' =>
			       [$cementDrain => $gram->suicide,
				$cementSet => $setSub,
				$gram->bindMoore (1 - $cementSet - $cementDrain,
						  [$gram->bmatch($gram->empty) =>
						   ['huff' =>
						    [$cementStep => $gram->moveTo]],
						   $gram->bmatch($stickType) => 
[ 'huff' =>
  [$cementStick => $stickSub]]])]]);

# cement tool
$gram->addTool ('name' => "$cementName spray",
		'size' => 2,
		'gstate' => $cementName,
		'reserve' => 1000,
		'recharge' => 100,
		'spray' => 1000,
		'overwrite' => [ 'gstate' => 'empty' ]);
}

sub make_wall {
    my ($gram, $wallRate, $wallMaxDecay) = @_;
    $gram->addType ('name' => 'wall',
		    'vars' => [ $gram->var('hue') => 8, $gram->var('decay') => 4 ],
		    'hue' => [ 'var' => 'hue' ],
		    'sat' => ['add' => 32],
		    'bri' => [ 'var' => 'decay', 'mul' => -8, 'add' => 255 ],
		    'rate' => $wallRate,
		    'rule' => ['switch' => ['loc' => $gram->origin,
					    'var' => 'decay',
					    'scase' =>
					    [ $gram->smatch($wallMaxDecay) => 
					      $gram->suicide
					      ($gram->balloon ("decay", 'rate' => .01, 'hexcolor' => "20ffff"))],
					    'default' => [ 'modify' => [ 'src' => [ 'var' => 'decay' ],
									 'inc' => 1 ]]]]);

}

sub make_acid {
    my ($gram, $acidRate, $acidDrain, $acidBurn) = @_;
    $gram->addType ('name' => 'acid',
		    'hue' => ['add' => 80],
		    'sat' => ['add' => 192],
		    'bri' => ['add' => 64],
		    'rate' => $acidRate,
		    'rule' => ['huff' =>
			       [$acidDrain => $gram->suicide,
				$gram->bindMoore (1 - $acidDrain,
						  [$gram->bmatch($gram->empty) => $gram->moveTo],
						  ['huff' => 
[ $acidBurn => $gram->homicide ($gram->suicide) ] ])]]);
}

sub make_plant {
    my ($gram, %plant) = @_;
    my $no_branch = ['modify' => ['src' => ['loc' => $gram->neighbor, 'var' => 'gens_left'],
				  'inc' => -1,
				  'dest' => ['loc' => $gram->neighbor, 'var' => 'gens_left' ]]];
    $gram->addType ('name' => 'plant',
		    'vars' => [ $gram->var('gens_left') => 3, $gram->var('branches') => 2 ],
		    'hue' => ['var' => 'gens_left', 'mul' => +8, 'add' => 82],
		    'sat' => ['add' => 240],
		    'bri' =>  ['var' => 'gens_left', 'mul' => -16, 'add' => 144],
		    'rate' => $plant{rate},
		    'rule' => ['huff' => 
			       [$plant{'die'} => $gram->suicide,
				(1 - $plant{'die'}) =>
				['switch' =>
				 ['loc' => $gram->origin,
				  'var' => 'gens_left',
				  'scase' => { $gram->smatch('0') =>
						   $gram->nop },
				  'default' => $gram->huffNeumann
				  ({ $gram->bmatch($gram->empty) =>
					 $gram->copyTo
					 ($gram->neighbor,
					  ['switch' => ['loc' => $gram->origin,
							'var' => 'branches',
							'scase' => { $gram->smatch($plant{'max_branches'}) =>
									 $no_branch },
							'default' => ['huff' =>
								      [(1-$plant{'branch'}) => $no_branch,
								       $plant{'branch'} =>
								       ['modify' =>
									['src' => 
									 ['loc' => $gram->origin, 'var' => 'branches'],
									 'inc' => +1,
									 'dest' =>
									 ['loc' => $gram->origin,
									  'var' => 'branches' ]]] ]]]]) }) ] ] ] ] );
}

sub make_species_switch {
    my ($gram, $selfRule, $predatorRule, $preyRule) = @_;
    ($selfRule, $predatorRule, $preyRule) = map (defined($_)
						 ? (ref($_) ? $_ : [])
						 : $gram->nop,
						 $selfRule, $predatorRule, $preyRule);
    my %sw;
    for my $orig (0..2) {
	my $prey = ($orig + 1) % 3;
	for my $nbr (0..2) {
	    $sw{$orig}->{$gram->smatch($nbr)} =
		($nbr == $orig)
		? $selfRule
		: (($nbr == $prey)
		   ? $predatorRule
		   : $preyRule);
	}
    }
    return ('switch' => ['loc' => $gram->origin,
			 'var' => 'species',
			 'scase' => [map (( $gram->smatch($_) =>
					    ['switch' => ['loc' => $gram->neighbor,
							  'var' => 'species',
							  'scase' => [%{$sw{$_}}]]] ),
					  0..2)]]);
}

sub make_rps_animal {
    my ($gram, %rps) = @_;
    my $stepOrBreed = $rps{'breed'} + $rps{'step'};
    my $eatOrConvert = $rps{'convert'} + $rps{'eat'};
    $gram->addType
	('name' => $rps{'name'},
	 'vars' => [ $gram->var('species') => 2 ],
	 'hue' => [ 'var' => 'species', 'mul' => 42, 'add' => 10 ],
	 'sat' => ['add' => 255],
	 'bri' => ['add' => 255],
	 'rate' => $rps{'rate'},
	 'rule' =>
	 ['switch' => ['loc' => $gram->origin,
		       'var' => 'species',
		       'scase' => [$gram->smatch(3) =>
				   ['huff' =>
				    [map ((1/3 =>
					   ['modify' =>
					    ['inc' => $_,
					     'dest' => ['var' => 'species']]]),
					  0..2)]]],
		       'default' =>
		       ['huff' =>
			[ $gram->bindNeumann
			  (1,
			   [ $gram->bmatch($gram->empty) =>
			     ['huff' =>
			      [$rps{'die'} => $gram->suicide,
			       $stepOrBreed =>
			       $gram->moveOrSpawnTo ($rps{'breed'} / $stepOrBreed,
						     $gram->neighbor,
						     $gram->balloon("step",'rate'=>$rps{'text'}),
						     $gram->balloon("breed",'rate'=>$rps{'text'}))]],
			     $gram->bmatch($rps{'food'}) =>
			     ['switch' => ['loc' => $gram->neighbor,
					   'var' => $rps{'food_unripeness_var'},
					   'scase' =>
					   { $gram->smatch(0) =>
						 [ 'huff' =>
						   [ $eatOrConvert =>
						     $gram->moveOrSpawnTo
						     ($rps{'convert'} / $eatOrConvert,
						      $gram->neighbor,
						      $gram->balloon("eat",'rate'=>$rps{'text'}),
						      $gram->balloon("spawn",'rate'=>$rps{'text'})) ] ],
						     $gram->smatch(1) =>
						     [ 'huff' =>
						       [ $rps{'eat'} => $gram->moveTo ] ] }]],
			     
			     $gram->bmatch($rps{'name'}) =>
			     [ make_species_switch
			       ($gram,
				[ 'huff' => 
				  [ $rps{'choke'} => $gram->suicide ] ],
				[ 'huff' => 
				  [ $eatOrConvert => $gram->moveOrSpawnTo
				    ($rps{'convert'} / $eatOrConvert,
				     $gram->neighbor,
				     $gram->balloon("prey",'rate'=>$rps{'text'}),
				     $gram->balloon("0wn",'rate'=>$rps{'text'})) ]]) ] ])]]]]);
}

sub make_perfume {
    my ($gram, $animalName, $perfumeRate, $perfumeDrain, $perfumeBillow, $perfumeInduce) = @_;

    $gram->addType ('name' => 'perfume',
		    'hue' => ['add' => 192],
		    'sat' => ['add' => 192],
		    'bri' => ['add' => 96],
		    'rate' => $perfumeRate,
		    'rule' => ['huff' => 
			       [$perfumeDrain => $gram->suicide,
				$gram->bindMoore (1 - $perfumeDrain,
						  [$gram->bmatch($gram->empty) => 
						   $gram->moveOrSpawnTo ($perfumeBillow),
						   $gram->bmatch($animalName) => 
						   ['huff' => 
						    [ $perfumeInduce =>
						      $gram->copyFromTo ($gram->neighbor, $gram->origin) ] ] ])]]);
}



# polymer cage builders
# outline of program:

# switch (state)
#  case 0: (paused)
#   nop

#  case 1: (init)
#   set orig.steps = orig.edge_len
#   set orig.edges = $edges - 1
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
#          set r_pos.l_dir = [direction from r_pos to orig]
#          set orig.state = 3 (active)
#          set orig.r_bond = 1
#    else (edges > 0)
#     set orig.r_dir = orig.r_dir + $turn_angle  (turn right)
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
#      set r_pos.l_dir = [direction from r_pos to orig]
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

sub poly_rule {
    my ($gram, $bond_dir2xy, $step_dir2xy, $type) = @_;
    return
	$gram->switchRule
	(undef, 'l_bond',
	 { 1 => verify_or_die_l
	       ($gram, $bond_dir2xy, $type,
		sub {
		    my ($l_dir) = @_;
		    $gram->switchRule
			(undef, 'r_bond',
			 { 1 => verify_or_die_r
			       ($gram, $bond_dir2xy, $type,
				sub {
				    my ($r_dir) = @_;
				    random_lr_step ($gram, $bond_dir2xy, $step_dir2xy, $type, $l_dir, $r_dir);
				}),
			   0 => random_l_step ($gram, $bond_dir2xy, $step_dir2xy, $type, $l_dir) })}),
	   0 => $gram->switchRule
	       (undef, 'r_bond',
		{1 => verify_or_die_r
		     ($gram, $bond_dir2xy, $type,
		      sub {
			  my ($r_dir) = @_;
			  random_r_step ($gram, $bond_dir2xy, $step_dir2xy, $type, $r_dir);
		      }),
		 0 => $gram->suicide }) });
}

sub poly_build_rule {
    my ($gram, $bond_dir2xy, $step_dir2xy, $edges, $turn_angle, $type) = @_;

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
#   set orig.edges = $edges - 1
		$gram->setRule
		('orig', 'edges', $edges - 1,
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
					 ('r_pos', @{$bond_dir2xy->[$_]},
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
#          set r_pos.l_dir = [direction from r_pos to orig]
							 $gram->setRule
							 ('r_pos', 'l_dir', delta_dir ($bond_dir2xy,$bond_dir2xy->[$_],[0,0]),
#          set orig.state = 3 (active)
							  $gram->setRule
							  ('orig', 'state', 3,
#          set orig.r_bond = 1
							   $gram->setRule
							   ('orig', 'r_bond', 1)))))})})},

#    else (edges > 0)
#     set orig.r_dir = orig.r_dir + $turn_angle  (turn right)
				   $gram->incRule
				   ('orig', 'r_dir', 'orig', 'r_dir', $turn_angle,
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
			    ('r_pos', @{$bond_dir2xy->[$_]},
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
#      set r_pos.l_dir = [direction from r_pos to orig]
				       $gram->setRule
				       ('r_pos', 'l_dir', delta_dir ($bond_dir2xy,$bond_dir2xy->[$_],[0,0]),
#      set r_pos.tail_state = 3 (active)
					$gram->setRule
					('r_pos', 'tail_state', 3,
#      decrement r_pos.steps
					 $gram->incRule
					 ('r_pos', 'steps', 'r_pos', 'steps', -1)))))))}))),

# (end loop over neighborhood)
			  0..$#$bond_dir2xy)}),
	 
#  case 3: (active)
#   poly_rule
			   3 => poly_rule ($gram, $bond_dir2xy, $step_dir2xy, $type) });
}

sub xy2dir_undef {
    my ($dir2xy_ref, $xy) = @_;
    my ($x, $y) = @$xy;
    for (my $dir = 0; $dir < @$dir2xy_ref; ++$dir) {
	if ($dir2xy_ref->[$dir]->[0] == $x && $dir2xy_ref->[$dir]->[1] == $y) {
	    return $dir;
	}
    }
    return undef;
}

sub bond_xy2dir {
    my ($dir2xy_ref, $xy) = @_;
    my $dir = bond_xy2dir_undef ($dir2xy_ref, $xy);
    confess "Can't find direction vector (@$xy)" unless defined $dir;
    return $dir;
}

sub delta_dir_undef {
    my ($dir2xy_ref, $xy1, $xy2) = @_;
    confess unless defined($xy2);
    my ($x1, $y1, $x2, $y2) = (@$xy1, @$xy2);
    my @xy = ($x2 - $x1, $y2 - $y1);
    return xy2dir_undef ($dir2xy_ref, \@xy);
}

sub delta_dir {
    my ($dir2xy_ref, $xy1, $xy2) = @_;
    my $dir = delta_dir_undef ($dir2xy_ref, $xy1, $xy2);
    confess "Can't find direction vector from (@$xy1) to (@$xy2)" unless defined $dir;
    return $dir;
}

sub test_bond_neighbors {
    my ($dir2xy_ref, $xy1, $xy2) = @_;
    return defined (delta_dir_undef ($dir2xy_ref, $xy1, $xy2));
}

sub set_neighbor_dir {
    my ($gram, $bond_dir2xy, $nbr_loc, $nbr_dir_var, $nbr_pos, $step_pos, $next) = @_;
    my $new_nbr_dir = delta_dir ($bond_dir2xy, $nbr_pos, $step_pos);
    return $gram->setRule ($nbr_loc, $nbr_dir_var, $new_nbr_dir, $next);
}

sub set_lpos_rdir {
    my ($gram, $bond_dir2xy, $nbr_pos, $step_pos, $next) = @_;
    return set_neighbor_dir ($gram, $bond_dir2xy, 'l_nbr', 'r_dir', $nbr_pos, $step_pos, $next);
}

sub set_rpos_ldir {
    my ($gram, $bond_dir2xy, $nbr_pos, $step_pos, $next) = @_;
    return set_neighbor_dir ($gram, $bond_dir2xy, 'r_nbr', 'l_dir', $nbr_pos, $step_pos, $next);
}

sub poly_type_and_vars {
    my ($gram, $bond_dir2xy, $type, $ldir, $rdir) = @_;
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
    my ($gram, $bond_dir2xy, $step_xy, $type, $ldir, $rdir) = @_;
    my ($lpos, $rpos) = @{$bond_dir2xy}[$ldir,$rdir];
    my @potentials = grep (test_bond_neighbors ($bond_dir2xy, $lpos, $_) && test_bond_neighbors ($bond_dir2xy, $rpos, $_), @$step_xy);
    my @step = map ($gram->bindRule
		    ('step_pos',
		     @$_,
		     { $gram->empty =>
			   set_lpos_rdir
			   ($gram, $bond_dir2xy, $lpos, $_,
			    set_rpos_ldir
			    ($gram, $bond_dir2xy, $rpos, $_,
			     $gram->setRule
			     ('step_pos',
			      undef,
			      poly_type_and_vars ($gram, $bond_dir2xy, $type,
						  delta_dir ($bond_dir2xy, $_, $lpos),
						  delta_dir ($bond_dir2xy, $_, $rpos)),
			      $gram->suicide))) }),
		    @potentials);
    return $gram->uniformHuffRule (@step);
}

sub random_l_step {
    my ($gram, $bond_dir2xy, $step_xy, $type, $ldir) = @_;
    my $lpos = $bond_dir2xy->[$ldir];
    my @potentials = grep (test_bond_neighbors ($bond_dir2xy, $lpos, $_), @$step_xy);
    my @step = map ($gram->bindRule
		    ('step_pos',
		     @$_,
		     { $gram->empty =>
			   set_lpos_rdir
			   ($gram, $bond_dir2xy, $lpos, $_,
			    $gram->setRule
			    ('step_pos',
			     undef,
			     poly_type_and_vars ($gram, $bond_dir2xy, $type,
						 delta_dir ($bond_dir2xy, $_, $lpos),
						 undef),
			     $gram->suicide)) }),
		    @potentials);
    return $gram->uniformHuffRule (@step);
}

sub random_r_step {
    my ($gram, $bond_dir2xy, $step_xy, $type, $rdir) = @_;
    my $rpos = $bond_dir2xy->[$rdir];
    my @potentials = grep (test_bond_neighbors ($bond_dir2xy, $rpos, $_), @$step_xy);
    my @step = map ($gram->bindRule
		    ('step_pos',
		     @$_,
		     { $gram->empty =>
			   set_rpos_ldir
			   ($gram, $bond_dir2xy, $rpos, $_,
			    $gram->setRule
			    ('step_pos',
			     undef,
			     poly_type_and_vars ($gram, $bond_dir2xy, $type,
						 undef,
						 delta_dir ($bond_dir2xy, $_, $rpos)),
			     $gram->suicide)) }),
		    @potentials);
    return $gram->uniformHuffRule (@step);
}

sub verify_or_die_l {
    my ($gram, $bond_dir2xy, $type, $next) = @_;
    return verify_or_die ($gram, $bond_dir2xy, $type, 'l_bond', 'l_dir', 'l_nbr', 'r_bond', 'r_dir', $next);
}

sub verify_or_die_r {
    my ($gram, $bond_dir2xy, $type, $next) = @_;
    return verify_or_die ($gram, $bond_dir2xy, $type, 'r_bond', 'r_dir', 'r_nbr', 'l_bond', 'l_dir', $next);
}

sub verify_or_die {
    my ($gram, $bond_dir2xy, $type, $bond_var, $dir_var, $nbr_loc, $nbr_bond_var, $nbr_dir_var, $next_sub) = @_;
    return $gram->switchRule (undef, $dir_var, { map (($_ =>
						     verify_pos_or_die
						     ($gram, $bond_dir2xy, $type, $bond_var, $bond_dir2xy->[$_], $nbr_loc, $nbr_bond_var, $nbr_dir_var, &$next_sub($_))),
						    0..$#$bond_dir2xy) });
}

sub verify_pos_or_die {
    my ($gram, $bond_dir2xy, $type, $bond_var, $nbr_pos, $nbr_loc, $nbr_bond_var, $nbr_dir_var, $next) = @_;
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
					 { delta_dir ($bond_dir2xy, $nbr_pos, [0,0]) => $next },
					 $unbond) }) },
	 $unbond);
}

sub ceiling_bits {
    my ($n) = @_;
    my $b = log($n) / log(2);
    my $c = int($b);
    return $c>=$b ? $c : $c+1;
}

sub add_polymer_loop_builder {
    my ($gram, $name, $rate, $bond_xy, $step_xy, $edges, $turn_angle, $length) = @_;

    my $bond_bits = ceiling_bits (@$bond_xy + 0);
    my $edges_bits = ceiling_bits ($edges - 1);
    my $steps_bits = ceiling_bits ($length);

    $gram->addType ('name' => $name,
		    'vars' => [ $gram->var('l_bond') => 1,
				$gram->var('l_dir') => $bond_bits,
				$gram->var('r_bond') => 1,
				$gram->var('r_dir') => $bond_bits,
				$gram->var('state') => 2,
				$gram->var('tail_state') => 2,
				$gram->var('edge_len') => $steps_bits,
				$gram->var('edges') => $edges_bits,
				$gram->var('steps') => $steps_bits ],
		    'hue' => ['add' => 20],
		    'sat' => ['add' => 255],
		    'bri' => ['add' => 255],
		    'rate' => $rate,
		    'rule' => poly_build_rule ($gram, $bond_xy, $step_xy, $edges, $turn_angle, $name));
}


1;
