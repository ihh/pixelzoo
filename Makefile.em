INCS    := -Isrc -Itsrc
ANSI    := -ansi -std=c99

CFILES  := $(filter-out src/fileio.c, $(wildcard src/*.c))
HFILES  := $(filter-out src/fileio.h, $(wildcard src/*.h))

all: bin/hardcoded-eloise-pztest.js

test: hardcoded-eloise-pztest

hardcoded-eloise-pztest: bin/hardcoded-eloise-pztest.js
	time node bin/hardcoded-eloise-pztest.js

bin/%.js: tsrc/%.c $(CFILES) $(HFILES)
	$(CC) $(INCS) $(ANSI) $(CFILES) tsrc/$*.c -o $@

.SUFFIXES :

.SECONDARY:
