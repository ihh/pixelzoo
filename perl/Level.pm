#!/usr/bin/perl -w

package Level;

use strict;
use vars ('@ISA', '@EXPORT', '@EXPORT_OK');

use Exporter;
use Carp qw(carp croak cluck confess);
use XML::Twig;
use Data::Dumper;

use AutoHash;
use Grammar;

push @ISA, qw (Grammar);
push @EXPORT, qw (newLevel);
@EXPORT_OK = @EXPORT;

# constructor
sub newLevel {
    my ($class) = @_;
    my $self = Grammar->newGrammar;
    my $emptyType = $self->empty;
    %$self = (%$self,
	      # stubs for make_game to build a game
	      'entrancePort' => AutoHash->new ("pos" => { "x" => 0, "y" => 0 }, "count" => 0, "rate" => 1, "width" => 1, "height" => 1, "type" => $emptyType),
	      'exitPort' => AutoHash->new ("pos" => { "x" => 0, "y" => 0 }, "count" => 0, "radius" => 6, "type" => $emptyType),
	      'xml' => AutoHash->new ('game' => [],   # allow designer/tester to override entire default Game structure (almost certainly not advisable)
				      'goal' => [],   # allow designer to override default Goal structure (probably not advisable)
				      'init' => [],   # allow designed to initialize Board
				      'tool' => [],   # to add tools, use addTool helper
				      'type' => [ '.particle' => { 'name' => $emptyType,
								   'var' => { 'hue' => 6, 'saturation' => 3, 'brightness' => 3 },
								   'hue' => { 'var' => 'hue' },
								   'sat' => { 'var' => 'saturation' },
								   'bri' => { 'var' => 'brightness' },
								   'rate' => 0,
								   'rule' => ['.nop'] },
						  # to add new particle types & rules, use addType helper
				      ]),
	);
    bless $self, $class;
    return $self;
}

# specific builder methods

# entrance "brush"
sub make_entrance_brush {
    my ($self) = @_;
    my @entranceLoc;
    my $entranceWidth = $self->entrancePort->width;
    my $entranceHeight = $self->entrancePort->height;
    for (my $w = 0; $w < $entranceWidth; ++$w) {
	for (my $h = 0; $h < $entranceHeight; ++$h) {
	    push @entranceLoc, "pos" => ["x" => $w, "y" => $h];
	}
    }
    return \@entranceLoc;
}

# exit
sub make_exit {
    my ($self) = @_;
    my (@exitLoc, @exitInit);
    my $exitRadius = $self->exitPort->radius;
    my $exitx = $self->exitPort->pos->{"x"};
    my $exity = $self->exitPort->pos->{"y"};
    for (my $x = -$exitRadius; $x <= $exitRadius; ++$x) {
	for (my $y = -$exitRadius; $y <= $exitRadius; ++$y) {
	    my $r = sqrt($x*$x+$y*$y);
	    if ($r < $exitRadius) {
		my $col = $self->palette->{'white'};  # white
		if ($r < $exitRadius/3 || $r > $exitRadius*2/3) {
		    $col = $self->palette->{'red'};  # red
		}
		my $ex = $exitx + $x;
		my $ey = $exity + $y;
		push @exitLoc, "pos" => ["x" => $ex, "y" => $ey] if $ex!=$exitx || $ey!=$exity;  # don't count the centre twice
		push @exitInit, "init" => ["x" => $ex, "y" => $ey, "hexval" => hexv($col)];
	    }
	}
    }
    return (\@exitLoc, \@exitInit);
}


# top-level proto-XML
sub make_game {
    my ($self) = @_;
    if (@{$self->xml->game} > 0) {
	return $self->xml->game;
    }

    my ($exitLoc, $exitInit) = $self->make_exit;
    my @game = (@{$self->xml->goal} > 0
		? @{$self->xml->goal}   # allow designer to override default Goal structure
		: ("goal" => ['@type' => "and",
			      "lazy" => "",
			      "cached" => "",

# place entrance and exit balloons
			      "goal" => ['@type' => "area",
					 "pos" => $self->entrancePort->pos,
					 "goal" => ['@type' => "balloon",
						    "balloon" => ["text" => "ENTRANCE",
								  "persist" => '']]],

			      "goal" => ['@type' => "area",
					 "pos" => $self->exitPort->pos,
					 "goal" => ['@type' => "balloon",
						    "balloon" => ["text" => "EXIT",
								  "persist" => '']]],

# print hello message
			      "goal" => ['@type' => "print",
					 "text" => "Welcome to level 1!\n" .
					 "Guide guests safely to the Exit.\n"],

# open the guest exit (currently the only exit)
			      "goal" => ['@type' => "setexit",
					 "state" => "PortalCounting"],

# introduce the guests
			      "goal" => ['@type' => "area",
					 "pos" => $self->entrancePort->pos,
					 "goal" => ['@type' => "spray",
						    "tool" => ["name" => "entrance",
							       "size" => minPowerOfTwo (max ($self->entrancePort->width, $self->entrancePort->height)),
							       "brush" => ["center" => ["x" => int($self->entrancePort->width/2), "y" => int($self->entrancePort->height/2)],
									   "intensity" => $self->make_entrance_brush],
							       "spray" => 1,
							       "hexstate" => $self->getTypeAsHexState($self->entrancePort->type),
							       "reserve" => $self->entrancePort->count,
							       "recharge" => 0]]],

# delete entrance balloon
			      "goal" => ['@type' => "area",
					 "pos" => $self->entrancePort->{"pos"},
					 "goal" => ['@type' => "balloon"]],

# print status message
			      "goal" => ['@type' => "print",
					 "text" => "The zoo is now closed to further guests.\nGuide all remaining guests to the Exit."],


# more goals here... e.g.,
# generic:
#    require the player to maintain the min/max bounds on population levels (guests, animals, expensive particles e.g. fire)

# specific:
#  fend off attacks to the guests, or by animals on other animals
#  grow food for the guests
#  hurry the guests along with stimulant
#  put out fires
#  protect the guests from avalanches, lasers, poison gas
#  place signposts
#  build cages, place animals in cages

# Could add a stub here, e.g.  @{$self-xml->midgoal},  but seems a bit premature without concrete use case

# wait for player to reach the guest exit count
			      "goal" => ['@type' => "exit",
					 "state" => "PortalCounting",
					 "count" => ["min" => $self->exitPort->count]],

# delete exit balloon
			      "goal" => ['@type' => "area",
					 "pos" => $self->exitPort->pos,
					 "goal" => ['@type' => "balloon"]],

# place "UNLOCKED" balloon at exit
			      "goal" => ['@type' => "area",
					 "pos" => $self->exitPort->pos,
					 "goal" => ['@type' => "balloon",
						    "balloon" => ["text" => "UNLOCKED!"]]],

# print "UNLOCKED" message
			      "goal" => ['@type' => "print",
					 "text" => "Exit unlocked! You could try the next level, if there was one."],

# "unlock" the guest exit achievment
			      "goal" => ['@type' => "setexit",
					 "state" => "PortalUnlocked"],


# more goals here... (e.g., steal the owner's HQ exit)


# print win message
			      "goal" => ['@type' => "print",
					 "text" => "YOU WIN! Congratulations."],

# set the game state to WIN!!!
			      "goal" => ['@type' => "setgame",
					 "state" => "GameWon"],

# that's all, folks
		   ]),

# end of default Goal structure
		@{$self->xml->tool},
		"rate" => $self->boardRate,
		"exit" => [%{$self->exitPort}, @$exitLoc],
		"board" => ["size" => $self->boardSize,
			    "grammar" => $self->xml->type,
			    @$exitInit,
			    @{$self->xml->init}]
	);

    return \@game;
}



1;
