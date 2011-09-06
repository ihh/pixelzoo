PERL        := /opt/local/bin/perl
XMLLINT     := /usr/bin/xmllint

SDLCONFIG = sdl-config
PKGCONFIG = pkg-config

SDL_CFLAGS  := $(shell $(SDLCONFIG) --cflags)
SDL_LDFLAGS := $(shell $(SDLCONFIG) --libs) -L/usr/X11R6/lib -lXi

# Use of pkg-config can be commented out & replaced with hardwired gcc args, due to MacPorts libxml flakiness -IH 5/4/2011
XML_CFLAGS  := $(shell $(PKGCONFIG) --cflags libxml-2.0)
XML_LDFLAGS := $(shell $(PKGCONFIG) --libs libxml-2.0)
#XML_CFLAGS  := -I/usr/include/libxml2
#XML_LDFLAGS := -L/usr/lib -lxml2 -lpthread -lz -lm

CC          := gcc
COPTS       := -g -Wall
CFLAGS      := $(XML_CFLAGS) $(SDL_CFLAGS) -Isrc
ANSI        := -ansi
LIBS        := -lc $(XML_LDFLAGS) $(SDL_LDFLAGS)

TARGETS := test_red_black_tree sdlgame

OFILES  := $(addprefix obj/,$(addsuffix .o,$(filter-out $(TARGETS),$(basename $(notdir $(wildcard src/*.c))))))
XFILES  := $(addprefix bin/,$(TARGETS))

XMLFILES := t/simple.xml

all: lib targets xml

test: all eloise-sdl-test xml-valid-test

clean:
	rm -rf obj/* bin/* *~ *.dSYM $(XMLFILES)

sdl: targets
	bin/sdlgame -g t/testgame.xml -l t/movelog.xml -b t/board.xml -r t/revcomp.xml

timed-sdl: targets
	bin/sdlgame -g t/testgame.xml -l t/movelog.xml -b t/board.xml -r t/revcomp.xml -t 5000000000

# Simple replay test, constructed as follows:
#
#  % bin/sdlgame -g t/testgame.xml -l t/movelog.xml -b t/board.xml -t 5000000000
#
#   ...trace the letters "eloise" on the screen for the 20 seconds or so that the test runs
#
#  % cp t/board.xml t/eloise_board.xml
#  % cp t/movelog.xml t/eloise_movelog.xml
#  % cp t/testgame.xml t/eloise_queue.xml
#
#   ...then paste t/eloise_movelog.xml into t/eloise_queue.xml
#
# eloise_X.xml should be identical to eloise_copy_X.xml for X in { movelog, board }
eloise-sdl-test: targets
	bin/sdlgame -d -g t/eloise_queue.xml -l t/eloise_copy_movelog.xml -b t/eloise_copy_board.xml -r t/eloise_copy_revcomp.xml -t 5000000000
	diff t/eloise_movelog.xml t/eloise_copy_movelog.xml
	diff t/eloise_board.xml t/eloise_copy_board.xml

eloise: eloise-sdl-test

# XML validation tests
# These should produce no output
xml-valid-test:
	$(XMLLINT) --dtdvalid Zoo/dtd/game.dtd --noout t/testgame.xml
	$(XMLLINT) --dtdvalid Zoo/dtd/proto.dtd --noout t/proto.xml

# Targets

targets: $(XFILES)

xml: $(XMLFILES)

lib: $(OFILES)

bin/%:  tsrc/%.c $(OFILES)
	@test -e bin || mkdir bin
	$(CC) $(COPTS) $(CFLAGS) $(LIBS) -o $@ tsrc/$*.c $(OFILES)

.SUFFIXES :

.SECONDARY:

t/simple.xml: perl/zoocompiler.pl Zoo/lib/Grammar.pm Zoo/lib/Level.pm
	$(PERL) perl/zoocompiler.pl -xmllint $(XMLLINT) -proto t/proto.xml -out t/simple.xml -verbose

xml-debug: perl/zoocompiler.pl Zoo/lib/Grammar.pm Zoo/lib/Level.pm
	$(PERL) perl/zoocompiler.pl -xmllint $(XMLLINT) -proto t/proto.xml -out t/simple.xml -debug

obj/%.o: src/%.c
	@test -e obj || mkdir obj
	$(CC) $(ANSI) $(COPTS) $(CFLAGS) -c $< -o $@

xmltest: bin/xmltest
	bin/xmltest t/testgame.xml
