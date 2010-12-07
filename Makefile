SDL_CFLAGS  := $(shell sdl-config --cflags)
SDL_LDFLAGS := $(shell sdl-config --libs) -L/usr/X11R6/lib -lXi

XML_CFLAGS  :=  -I/usr/include/libxml2
XML_LDFLAGS :=  -L/usr/lib

CC     := gcc
COPTS  := -g -Wall
CFLAGS := $(SDL_CFLAGS) $(XML_CFLAGS)
LIBS   := -lstdc++ -lxml2 $(SDL_LDFLAGS) $(XML_LDFLAGS)

TARGETS := sdltest
OFILES  := $(addsuffix .o,$(filter-out $(TARGETS),$(basename $(wildcard *.c))))

all: $(OFILES)

clean:
	rm *.o $(TARGETS)

$(TARGETS): $(OFILES) $(addsuffix .c,$(TARGETS))
	$(CC) $(COPTS) $(CFLAGS) $(LIBS) -o $@ $@.c $(OFILES)

.SUFFIXES :
.SUFFIXES : .o .c

.c.o:
	$(CC) $(COPTS) $(CFLAGS) -c $< -o $@
