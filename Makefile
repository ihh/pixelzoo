#PERL        := /opt/local/bin/perl
#PERL        := /usr/bin/perl
PERL        := perl
XMLLINT     := /usr/bin/xmllint

SDLCONFIG = sdl-config
PKGCONFIG = pkg-config

SDL_CFLAGS  := $(shell $(SDLCONFIG) --cflags)
SDL_LDFLAGS := $(shell $(SDLCONFIG) --static-libs)
# -L/usr/X11R6/lib -lXi

INSTALLPATH := /usr/local/bin

CC          := gcc
AR          := ar
COPTS       := -g -Wall
CFLAGS      := $(SDL_CFLAGS) -Isrc -Itsrc
ANSI        := -ansi -std=c99
LIBS        := -lc $(SDL_LDFLAGS)
ARFLAGS     := -rcvs

ifneq (,$(findstring debug,$(MAKECMDGOALS)))
CFLAGS := $(CFLAGS) -DPIXELZOO_DEBUG
endif

TARGETS   := test_red_black_tree sdlgame pztest hardcoded-eloise-pztest
LIBDIR    := lib
LIBNAME   := pixelzoo
LIBTARGET := $(LIBDIR)/lib$(LIBNAME).a

CFILES  := $(wildcard src/*.c)
OFILES  := $(addprefix obj/,$(addsuffix .o,$(filter-out $(TARGETS),$(basename $(notdir $(CFILES))))))
XFILES  := $(addprefix bin/,$(TARGETS))

CHIBI_CFILES := chibi/sexp.c chibi/eval.c
CHIBI_OFILES := $(addprefix chibi/obj/,$(addsuffix .o,$(basename $(notdir $(CHIBI_CFILES)))))
CHIBI_CODE   := -Ichibi/lib -DSEXP_USE_DL=0 -DSEXP_USE_STATIC_LIBS
CHIBI_HDRS   := -Ichibi/include -DSEXP_64_BIT -Dsexp_default_module_path=\"$(CURDIR)/chibi/lib\" -Dsexp_pixelzoo_module_path=\"$(CURDIR)/scheme/zoo.scm\"

# SCHEME_FILES := general.scm poly.scm rna.scm util.scm
SCHEME_FILES := util.scm general.scm poly.scm

all: libtargets targets scheme/zoo.scm

clean:
	rm -rf obj/* bin/* lib/* *~ *.dSYM chibi/obj/*

sdl: targets
	bin/sdlgame -g t/testgame.xml -l t/movelog.xml -b t/board.xml -r t/revcomp.xml

timed-sdl: targets
	bin/sdlgame -g t/testgame.xml -l t/movelog.xml -b t/board.xml -r t/revcomp.xml -t 5000000000

sdl2: targets
	bin/sdlgame t/newproto.xml


# Targets

targets: $(XFILES)

libtargets: $(LIBTARGET)

bin/%:  tsrc/%.c $(LIBTARGET)
	@test -e bin || mkdir bin
	$(CC) $(COPTS) $(CFLAGS) $(CHIBI_HDRS) $(LIBS) -L$(LIBDIR) -l$(LIBNAME) -o $@ tsrc/$*.c

obj/%.o: src/%.c
	@test -e obj || mkdir obj
	$(CC) $(ANSI) $(COPTS) $(CFLAGS) $(CHIBI_HDRS) -c $< -o $@

# Base chibi-scheme
chibi/obj/%.o: chibi/%.c
	@test -e chibi/obj || mkdir chibi/obj
	$(CC) $(ANSI) $(COPTS) $(CHIBI_HDRS) $(CHIBI_CODE) -c $< -o $@

bin/chibi_scheme: $(CHIBI_CFILES)
	$(CC) $(ANSI) $(COPTS) $(CHIBI_HDRS) $(CHIBI_CODE) $(CHIBI_CFILES) chibi/main.c -o $@

chibi-test: bin/chibi_scheme
	(echo '(load "scheme/zoo.scm")'; cat) | bin/chibi_scheme

chibi-install: $(INSTALLPATH)/chibi-scheme

$(INSTALLPATH)/chibi-scheme: bin/chibi_scheme
	cp $< $@

# chibi-scheme board library
bin/pzchibi: $(LIBTARGET)
	$(CC) -DSEXP_PIXELZOO_REPL $(ANSI) $(COPTS) $(CHIBI_HDRS) $(CHIBI_CODE) $(LIBTARGET) chibi/main.c -o $@

# this will hang...
pzchibi-bug: bin/pzchibi
	cat scheme/pzchibi-bug.scm | bin/pzchibi

# Scheme code
SCHEME_FILES_WITH_PREFIX := $(addprefix scheme/,$(SCHEME_FILES))
scheme/zoo.scm: $(SCHEME_FILES_WITH_PREFIX)
	(echo "(begin"; cat $(SCHEME_FILES_WITH_PREFIX); echo ")") >$@

testrna: scheme/testrna.scm
	cat $< | bin/pzchibi

scheme/testrna.scm: $(SCHEME_FILES_WITH_PREFIX) scheme/rna.scm
	(((echo "(begin"; cat $(SCHEME_FILES_WITH_PREFIX)) | perl -pe 's/;.*//g;s/\n/ /g'); cat scheme/rna.scm; echo '(define self-type "RNA")'; echo '(rna-move-rule)'; echo ")") >$@

# pixelzoo core library
$(LIBTARGET): $(OFILES) $(CHIBI_OFILES)
	@test -e lib || mkdir lib
	$(AR) $(ARFLAGS) $(LIBTARGET) $(OFILES) $(CHIBI_OFILES)

debug:

.SUFFIXES :

.SECONDARY:


# Test collections

test: replay-test

all-tests: replay-test sdl-test compiler-test

replay-test: eloise-pztest hardcoded-eloise-pztest

sdl-test: eloise-sdl-test

compiler-test: xml-valid-test xml-build-test


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
	time bin/pztest $(ELOISE_TEST_ARGS)
	diff t/eloise_movelog.xml t/eloise_copy_movelog.xml
	diff t/eloise_board.xml t/eloise_copy_board.xml

hardcoded-eloise-pztest: targets
	time bin/hardcoded-eloise-pztest

eloise: eloise-sdl-test eloise-pztest

# XML validation tests
# These should produce no output
xml-valid-test:
	$(XMLLINT) --dtdvalid Zoo/dtd/game.dtd --noout t/testgame.xml

# Documentation
doc: dtddoc-doc doxygen-doc

doxygen-doc:
	cd src; doxygen

dtddoc-doc:
	cd Zoo/dtd; make

README.html: README.md
	perl -e 'use Text::Markdown "markdown";print markdown(join("",<>))' $< >$@

# emscripten
em: em-test

em-all:
	/usr/local/src/emscripten/emmake make -f Makefile.em all

em-test: test
	/usr/local/src/emscripten/emmake make -f Makefile.em test

em-html:
	/usr/local/src/emscripten/emmake make -f Makefile.em html

em-lib:
	/usr/local/src/emscripten/emmake make -f Makefile.em jslib
