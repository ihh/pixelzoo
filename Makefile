SDL_CFLAGS  := $(shell sdl-config --cflags)
SDL_LDFLAGS := $(shell sdl-config --libs) -L/usr/X11R6/lib -lXi

XML_CFLAGS  := $(shell pkg-config --cflags libxml-2.0)
XML_LDFLAGS := $(shell pkg-config --libs libxml-2.0)

CC          := gcc
COPTS       := -g -Wall
CFLAGS      := $(SDL_CFLAGS) $(XML_CFLAGS)
ANSI        := -ansi
LIBS        := -lc $(SDL_LDFLAGS) $(XML_LDFLAGS)

TARGETS := sdltest test_red_black_tree

OFILES  := $(addprefix obj/,$(addsuffix .o,$(filter-out $(TARGETS),$(basename $(wildcard *.c)))))
XFILES  := $(addprefix bin/,$(TARGETS))

all: $(XFILES)

lib: $(OFILES)

clean:
	rm -rf obj/* bin/* *~ *.dSYM

bin/%:  %.c $(OFILES)
	@test -e bin || mkdir bin
	$(CC) $(COPTS) $(CFLAGS) $(LIBS) -o $@ $*.c $(OFILES)

.SUFFIXES :

obj/%.o: %.c
	@test -e obj || mkdir obj
	$(CC) $(ANSI) $(COPTS) $(CFLAGS) -c $< -o $@
