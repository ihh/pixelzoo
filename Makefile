SDLCONFIG = sdl-config
PKGCONFIG = pkg-config

SDL_CFLAGS  := $(shell $(SDLCONFIG) --cflags)
SDL_LDFLAGS := $(shell $(SDLCONFIG) --libs) -L/usr/X11R6/lib -lXi

XML_CFLAGS  := $(shell $(PKGCONFIG) --cflags libxml-2.0)
XML_LDFLAGS := $(shell $(PKGCONFIG) --libs libxml-2.0)

CC          := gcc
COPTS       := -g -Wall
CFLAGS      := $(SDL_CFLAGS) $(XML_CFLAGS) -Isrc
ANSI        := -ansi
LIBS        := -lc $(SDL_LDFLAGS) $(XML_LDFLAGS)

TARGETS := test_red_black_tree sdltest sdlgame

OFILES  := $(addprefix obj/,$(addsuffix .o,$(filter-out $(TARGETS),$(basename $(notdir $(wildcard src/*.c))))))
XFILES  := $(addprefix bin/,$(TARGETS))

ZGFILES   := $(wildcard t/*.zg)
XMLFILES  := $(subst .zg,.xml,$(ZGFILES))

all: lib targets xml

clean:
	rm -rf obj/* bin/* *~ *.dSYM $(XMLFILES) t/poly.h

test: targets
	bin/sdlgame t/testgame.xml

oldtest: all
	test/testrb.sh && bin/sdltest

targets: $(XFILES)

xml: poly $(XMLFILES)

lib: $(OFILES)

poly: t/poly.h

t/poly.h: perl/makepoly.pl
	perl/makepoly.pl >$@

bin/%:  test/%.c $(OFILES)
	@test -e bin || mkdir bin
	$(CC) $(COPTS) $(CFLAGS) $(LIBS) -o $@ test/$*.c $(OFILES)

.SUFFIXES :

.SECONDARY:

%.xml: %.zg perl/zoocompiler.pl
	perl/zoocompiler.pl -v $< >$@

obj/%.o: src/%.c
	@test -e obj || mkdir obj
	$(CC) $(ANSI) $(COPTS) $(CFLAGS) -c $< -o $@
