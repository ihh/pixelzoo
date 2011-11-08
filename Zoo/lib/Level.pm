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

	      # helpers
	      'neighbor' => 'nbr',
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
	    push @entranceLoc, "spot" => ["x" => $w, "y" => $h];
	}
    }
    return \@entranceLoc;
}

# exit
sub prep_exit {
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
		push @exitInit, "init" => ["x" => $ex, "y" => $ey, "hexstate" => hexv($col)];
	    }
	}
    }
    return (\@exitLoc, \@exitInit);
}

sub make_exit {
    my ($self) = @_;
    my ($exitLoc, $exitInit) = $self->prep_exit;
    return ("exit" => [sortHash ($self->exitPort, qw(type hextype)), @$exitLoc]);
}

sub make_exit_init {
    my ($self) = @_;
    my ($exitLoc, $exitInit) = $self->prep_exit;
    return @$exitInit;
}

sub make_goal {
    my ($self) = @_;
    my ($exitLoc, $exitInit) = $self->make_exit;

    return ("goal" => ["and" => ["lazy" => "",
				 "cache" => "",

# place entrance and exit balloons
				 "goal" => ["area" => ["pos" => $self->entrancePort->pos,
						       "goal" => ["place" => ["balloon" => ["text" => "ENTRANCE",
											    "persist" => '']]]]],
				 
				 "goal" => ["area" => ["pos" => $self->exitPort->pos,
						       "goal" => ["place" => ["balloon" => ["text" => "EXIT",
											    "persist" => '']]]]],

# print hello message
				 "goal" => ["print" => ["message" => "Welcome to level 1!\n" .
							"Guide guests safely to the Exit.\n"]],

# open the guest exit (currently the only exit)
				 "goal" => ["setexit" => ["exitstate" => "PortalCounting"]],

# introduce the guests
				 "goal" => ["area" => ["pos" => $self->entrancePort->pos,
						       "goal" => ["usetool" => ["tool" => ["name" => "entrance",
											   "size" => minPowerOfTwo (max ($self->entrancePort->width, $self->entrancePort->height)),
											   "brush" => ["center" => ["x" => int($self->entrancePort->width/2), "y" => int($self->entrancePort->height/2)],
												       "intensity" => $self->make_entrance_brush],
											   "hexstate" => $self->getTypeAsHexState($self->entrancePort->type),
											   "spray" => 1,
											   "reserve" => $self->entrancePort->count,
											   "recharge" => 0]]]]],

# delete entrance balloon
				 "goal" => ["area" => ["pos" => $self->entrancePort->{"pos"},
						       "goal" => ["place" => ""]]],

# print status message
		       "goal" => ["print" => ["message" => "The zoo is now closed to further guests.\nGuide all remaining guests to the Exit."]],


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
				 "goal" => ["setexit" => ["exitstate" => "PortalCounting"]],

# delete exit balloon
				 "goal" => ["area" => ["pos" => $self->exitPort->pos,
						       "goal" => ["place" => ""]]],

# place "UNLOCKED" balloon at exit
				 "goal" => ["area" => ["pos" => $self->exitPort->pos,
						       "goal" => ["place" => ["balloon" => ["text" => "UNLOCKED!"]]]]],

# print "UNLOCKED" message
				 "goal" => ["print" => ["message" => "Exit unlocked! You could try the next level, if there was one."]],

# "unlock" the guest exit achievment
				 "goal" => ["setexit" => ["exitstate" => "PortalUnlocked"]],


# more goals here... (e.g., steal the owner's HQ exit)


# print win message
		       "goal" => ["print" => ["message" => "YOU WIN! Congratulations."]],

# set the game state to WIN!!!
		       "goal" => ["setgame" => ["gamestate" => "GameWon"]],

# that's all, folks
	    ]]);
}



# addType helpers
sub bindDirs {
    my ($self, $dirs, $totalProb, $cases, $default, $loc, $guard) = @_;
    die "Too many arguments" if defined($guard);
    die "Loc should not be a reference" if ref($loc);
    $loc = $self->neighbor unless defined $loc;
    confess "Not an ARRAY" unless ref($dirs) eq 'ARRAY';
    my $prob = $totalProb / @$dirs;
    return map (($self->pvalue($prob) => ['bind' => ['loc' => $loc,
				       'x' => $self->dir->{$_}->x,
				       'y' => $self->dir->{$_}->y,
				       defined($cases) ? ('case' => $cases) : (),
				       defined($default) ? ('default' => $default) : ()]]),
		@$dirs);
}

sub bindNeumann {
    my ($self, $totalProb, $cases, $default, $loc) = @_;
    return $self->bindDirs ([qw(n e s w)], $totalProb, $cases, $default, $loc);
}

sub bindBishop {
    my ($self, $totalProb, $cases, $default, $loc) = @_;
    return $self->bindDirs ([qw(nw ne se sw)], $totalProb, $cases, $default, $loc);
}

sub bindMoore {
    my ($self, $totalProb, $cases, $default, $loc) = @_;
    return $self->bindDirs ([qw(n ne e se s sw w nw)], $totalProb, $cases, $default, $loc);
}

sub huffDirs { my ($self, $cases, $default, $loc) = @_; return ['huff' => [$self->bindDirs (1, $cases, $default, $loc)]] }
sub huffNeumann { my ($self, $cases, $default, $loc) = @_; return ['huff' => [$self->bindNeumann (1, $cases, $default, $loc)]] }
sub huffBishop { my ($self, $cases, $default, $loc) = @_; return ['huff' => [$self->bindBishop (1, $cases, $default, $loc)]] }
sub huffMoore { my ($self, $cases, $default, $loc) = @_; return ['huff' => [$self->bindMoore (1, $cases, $default, $loc)]] }

sub moveOrSpawnTo {
    my ($self, $spawnProb, $loc, $afterMove, $afterSpawn) = @_;
    $loc = $self->neighbor unless defined $loc;
    return $self->copyTo ($self->neighbor,
			  ['huff' => [defined($afterSpawn) ? ($self->pvalue($spawnProb) => $afterSpawn) : (),
				      $self->pvalue(1-$spawnProb) => $self->suicide ($afterMove)]]);
}

sub moveTo {
    my ($self, $loc, $next) = @_;
    $loc = $self->neighbor unless defined $loc;
    return $self->copyTo ($loc, $self->suicide ($next));
}

sub copyTo {
    my ($self, $loc, $next) = @_;
    $loc = $self->neighbor unless defined $loc;
    return $self->copyFromTo ('o', $loc, $next);
}

sub copyFromTo {
    my ($self, $src, $dest, $next) = @_;
    return ['modify' => [ 'src' => [ 'loc' => $src ],
			  'dest' => [ 'loc' => $dest ],
			  defined($next) ? ('next' => $next) : () ] ];
}

sub suicide {
    my ($self, $next) = @_;
    return ['modify' => [ 'set' => [ 'type' => $self->empty  ],
			  defined($next) ? ('next' => $next) : () ] ];
}

sub homicide {
    my ($self, $next) = @_;
    return ['modify' => [ 'set' => [ 'type' => $self->empty  ],
			  'dest' => [ 'loc' => $self->neighbor ],
			  defined($next) ? ('next' => $next) : () ] ];
}

sub balloon {
    my ($self, $text, @args) = @_;
    my %ballArgs = ('text' => $text,
		    'rate' => 1,
		    @args);
    return ['rule' => ['ball' => [sortHash(\%ballArgs, $self->balloonArgs)]]];
}

1;
