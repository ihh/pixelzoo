#PERL        := /opt/local/bin/perl
PERL        := /usr/bin/perl
XMLLINT     := /usr/bin/xmllint

SDLCONFIG = sdl-config
PKGCONFIG = pkg-config

SDL_CFLAGS  := $(shell $(SDLCONFIG) --cflags)
SDL_LDFLAGS := $(shell $(SDLCONFIG) --libs) -L/usr/X11R6/lib -lXi

# Use of pkg-config can be commented out & replaced with hardwired gcc args, to combat MacPorts libxml flakiness -IH 5/4/2011
XML_CFLAGS  := $(shell $(PKGCONFIG) --cflags libxml-2.0)
XML_LDFLAGS := $(shell $(PKGCONFIG) --libs libxml-2.0)
#XML_CFLAGS  := -I/usr/include/libxml2
#XML_LDFLAGS := -L/usr/lib -lxml2 -lpthread -lz -lm

CC          := gcc
AR          := ar
COPTS       := -g -Wall
CFLAGS      := $(XML_CFLAGS) $(SDL_CFLAGS) -Isrc
ANSI        := -ansi
LIBS        := -lc $(XML_LDFLAGS) $(SDL_LDFLAGS)
ARFLAGS     := -rcvs

TARGETS   := test_red_black_tree sdlgame
LIBDIR    := lib
LIBNAME   := pixelzoo
LIBTARGET := $(LIBDIR)/lib$(LIBNAME).a

OFILES  := $(addprefix obj/,$(addsuffix .o,$(filter-out $(TARGETS),$(basename $(notdir $(wildcard src/*.c))))))
XFILES  := $(addprefix bin/,$(TARGETS))

XMLFILES := t/simple.xml

all: lib targets xml

test: all eloise-sdl-test xml-valid-test

clean:
	rm -rf obj/* bin/* lib/* *~ *.dSYM

cleanxml:
	rm $(XMLFILES)

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
	bin/sdlgame -d -g t/eloise_queue.xml -l t/eloise_copy_movelog.xml -b t/eloise_copy_board.xml -t 5000000000
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

lib: $(LIBTARGET)

bin/%:  tsrc/%.c $(LIBTARGET)
	@test -e bin || mkdir bin
	$(CC) $(COPTS) $(CFLAGS) $(LIBS) -L$(LIBDIR) -l$(LIBNAME) -o $@ tsrc/$*.c

$(LIBTARGET): $(OFILES)
	@test -e lib || mkdir lib
	$(AR) $(ARFLAGS) $(LIBTARGET) $(OFILES)

.SUFFIXES :

.SECONDARY:

t/simple.xml: perl/zoocompiler.pl Zoo/lib/Grammar.pm Zoo/lib/Level.pm
	$(PERL) perl/zoocompiler.pl -xmllint $(XMLLINT) -proto t/proto.xml -out t/simple.xml -verbose

xml-debug: perl/zoocompiler.pl Zoo/lib/Grammar.pm Zoo/lib/Level.pm
	$(PERL) perl/zoocompiler.pl -xmllint $(XMLLINT) -proto t/proto.xml -out t/simple.xml -debug

obj/%.o: src/%.c
	@test -e obj || mkdir obj
	$(CC) $(ANSI) $(COPTS) $(CFLAGS) -c $< -o $@

# The following target builds and runs a simple XML reader, without any other SDL or pixelzoo targets
# It can be used to test libxml2 on a new platform.
xmltest:
	@test -e bin || mkdir bin
	$(CC) $(COPTS) $(XML_CFLAGS) -lc $(XML_LDFLAGS) -o bin/xmltest tsrc/xmltest.c
	bin/xmltest t/testgame.xml
