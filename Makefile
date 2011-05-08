PERL        := /opt/local/bin/perl
XMLLINT     := /usr/bin/xmllint

SDLCONFIG = sdl-config
PKGCONFIG = pkg-config

SDL_CFLAGS  := $(shell $(SDLCONFIG) --cflags)
SDL_LDFLAGS := $(shell $(SDLCONFIG) --libs) -L/usr/X11R6/lib -lXi

# Use of pkg-config commented out & replaced with hardwired gcc args, due to MacPorts libxml flakiness -IH 5/4/2011
#XML_CFLAGS  := $(shell $(PKGCONFIG) --cflags libxml-2.0)
#XML_LDFLAGS := $(shell $(PKGCONFIG) --libs libxml-2.0)
XML_CFLAGS  := -I/usr/include/libxml2
XML_LDFLAGS := -L/usr/lib -lxml2 -lpthread -lz -lm

CC          := gcc
COPTS       := -g -Wall
CFLAGS      := $(XML_CFLAGS) $(SDL_CFLAGS) -Isrc
ANSI        := -ansi
LIBS        := -lc $(XML_LDFLAGS) $(SDL_LDFLAGS)

TARGETS := test_red_black_tree sdlgame

OFILES  := $(addprefix obj/,$(addsuffix .o,$(filter-out $(TARGETS),$(basename $(notdir $(wildcard src/*.c))))))
XFILES  := $(addprefix bin/,$(TARGETS))

XMLFILES := t/simple.xml

ifneq (,$(findstring debug,$(MAKECMDGOALS)))
# 'debug' was specified:
DEBUG := -debug
else
DEBUG :=
endif

all: lib targets xml

clean:
	rm -rf obj/* bin/* *~ *.dSYM $(XMLFILES)

sdl: targets
	bin/sdlgame -g t/testgame.xml -l t/movelog.xml -b t/board.xml -t 1000000000

targets: $(XFILES)

xml: $(XMLFILES)

lib: $(OFILES)

bin/%:  test/%.c $(OFILES)
	@test -e bin || mkdir bin
	$(CC) $(COPTS) $(CFLAGS) $(LIBS) -o $@ test/$*.c $(OFILES)

.SUFFIXES :

.SECONDARY:

t/simple.xml: perl/zoocompiler.pl perl/Grammar.pm perl/Level.pm
	$(PERL) perl/zoocompiler.pl -xmllint $(XMLLINT) -proto t/proto.xml -out t/simple.xml -verbose

xml-debug: perl/zoocompiler.pl perl/Grammar.pm perl/Level.pm
	$(PERL) perl/zoocompiler.pl -xmllint $(XMLLINT) -proto t/proto.xml -out t/simple.xml -debug

obj/%.o: src/%.c
	@test -e obj || mkdir obj
	$(CC) $(ANSI) $(COPTS) $(CFLAGS) -c $< -o $@
