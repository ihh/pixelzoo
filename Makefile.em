INCS    := -Isrc -Itsrc
ANSI    := -ansi -std=c99

TARGETS := sdlgame pztest
CFILES  := $(wildcard src/*.c)
HFILES  := $(wildcard src/*.h)

all: bin/pztest.js

bin/%.js: tsrc/%.c $(CFILES) $(HFILES)
	$(CC) $(INCS) $(ANSI) $(CFILES) tsrc/$*.c -o $@

.SUFFIXES :

.SECONDARY:
