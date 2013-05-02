INCS    := -Isrc -Itsrc
ANSI    := -ansi -std=c99
#EMFLAGS := - -s ASM_JS=1 -O2
EMFLAGS :=

CFILES  := $(filter-out src/fileio.c, $(wildcard src/*.c))
HFILES  := $(filter-out src/fileio.h, $(wildcard src/*.h))

all: bin/hardcoded-eloise-pztest.js

test: hardcoded-eloise-pztest

html: hardcoded-eloise-pztest.html.open

jslib: lib/pixelzoo.js

hardcoded-eloise-pztest: bin/hardcoded-eloise-pztest.js
	time node bin/hardcoded-eloise-pztest.js

%.html.open: bin/%.html
	open $<

bin/%.js: tsrc/%.c $(CFILES) $(HFILES)
	$(CC) $(EMFLAGS) $(INCS) $(ANSI) $(CFILES) tsrc/$*.c -o $@

bin/%.html: tsrc/%.c $(CFILES) $(HFILES)
	$(CC) $(EMFLAGS) $(INCS) $(ANSI) $(CFILES) tsrc/$*.c -o $@

lib/pixelzoo.js: $(CFILES) $(HFILES)
	/bin/echo -n $(CC) $(EMFLAGS) $(INCS) $(ANSI) $(CFILES) -o $@ >run_emcc
	cat src/pixelzoo.h | perl -e 'while(<>){if(/PZEXPORT (\w+)/){push@f,chr(39)."_$$1".chr(39)}}print" -s EXPORTED_FUNCTIONS=\"[",join(",",@f),"]\""' >>run_emcc
	/bin/sh run_emcc

run_emcc:

.SUFFIXES :

.SECONDARY:
