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

1;
