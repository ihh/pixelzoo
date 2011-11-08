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

TARGETS   := test_red_black_tree sdlgame pztest
LIBDIR    := lib
LIBNAME   := pixelzoo
LIBTARGET := $(LIBDIR)/lib$(LIBNAME).a

OFILES  := $(addprefix obj/,$(addsuffix .o,$(filter-out $(TARGETS),$(basename $(notdir $(wildcard src/*.c))))))
XFILES  := $(addprefix bin/,$(TARGETS))

XMLTESTFILES := t/simple.copy.xml t/compiled.copy.xml

all: lib targets

clean:
	rm -rf obj/* bin/* lib/* *~ *.dSYM $(XMLTESTFILES)

cleanxml:
	rm $(XMLTESTFILES)

sdl: targets
	bin/sdlgame -g t/testgame.xml -l t/movelog.xml -b t/board.xml -r t/revcomp.xml

timed-sdl: targets
	bin/sdlgame -g t/testgame.xml -l t/movelog.xml -b t/board.xml -r t/revcomp.xml -t 5000000000

# Targets

targets: $(XFILES)

lib: $(LIBTARGET)

bin/%:  tsrc/%.c $(LIBTARGET)
	@test -e bin || mkdir bin
	$(CC) $(COPTS) $(CFLAGS) $(LIBS) -L$(LIBDIR) -l$(LIBNAME) -o $@ tsrc/$*.c

obj/%.o: src/%.c
	@test -e obj || mkdir obj
	$(CC) $(ANSI) $(COPTS) $(CFLAGS) -c $< -o $@

$(LIBTARGET): $(OFILES)
	@test -e lib || mkdir lib
	$(AR) $(ARFLAGS) $(LIBTARGET) $(OFILES)

.SUFFIXES :

.SECONDARY:


test: all xmltest eloise-pztest xml-valid-test xml-build-test

# The following target builds and runs a simple XML reader, without any other SDL or pixelzoo targets
# It can be used to test libxml2 on a new platform.
xmltest:
	@test -e bin || mkdir bin
	$(CC) $(COPTS) $(XML_CFLAGS) -lc $(XML_LDFLAGS) -o bin/xmltest tsrc/xmltest.c
	bin/xmltest t/testgame.xml

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
ELOISE_TEST_ARGS = -g t/eloise_queue.xml -l t/eloise_copy_movelog.xml -b t/eloise_copy_board.xml -t 5000000000
eloise-sdl-test: targets
	bin/sdlgame -d $(ELOISE_TEST_ARGS)
	diff t/eloise_movelog.xml t/eloise_copy_movelog.xml
	diff t/eloise_board.xml t/eloise_copy_board.xml

eloise-pztest: targets
	bin/pztest $(ELOISE_TEST_ARGS)
	diff t/eloise_movelog.xml t/eloise_copy_movelog.xml
	diff t/eloise_board.xml t/eloise_copy_board.xml

eloise: eloise-sdl-test eloise-pztest

# XML validation tests
# These should produce no output
xml-valid-test:
	$(XMLLINT) --dtdvalid Zoo/dtd/game.dtd --noout t/testgame.xml
	$(XMLLINT) --dtdvalid Zoo/dtd/proto.dtd --noout t/proto.xml

# XML generation tests

xml-build-test: $(XMLTESTFILES)

# Build a simple world using the Perl API.
t/simple.copy.xml: perl/simplezoo.pl Zoo/lib/Grammar.pm Zoo/lib/Level.pm
	$(PERL) perl/simplezoo.pl -xmllint $(XMLLINT) -proto t/proto.copy.xml -out $@ -verbose
	diff t/proto.xml t/proto.copy.xml
	diff t/simple.xml t/simple.copy.xml

xml-debug: perl/simplezoo.pl Zoo/lib/Grammar.pm Zoo/lib/Level.pm
	$(PERL) perl/simplezoo.pl -xmllint $(XMLLINT) -proto t/proto.copy.xml -out t/simple.copy.xml -debug

t/compiled.copy.xml: t/proto.xml
	$(PERL) perl/zoocompiler.pl $< >$@
	diff t/compiled.xml t/compiled.copy.xml

# Documentation
doc:
	cd Zoo/dtd; make
