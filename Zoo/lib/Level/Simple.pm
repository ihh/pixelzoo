#!/usr/bin/perl -w

package Level::Simple;

use strict;
use vars ('@ISA', '@EXPORT', '@EXPORT_OK');

use Exporter;
use Carp qw(carp croak cluck confess);

use AutoHash;
use Grammar;
use Level;

push @ISA, qw (Level);
#push @EXPORT, qw ();
#@EXPORT_OK = @EXPORT;

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


1;
