CC = gcc -I/usr/include/libxml2

TARGET =
OFILES = $(addsuffix .o,$(filter-out $(TARGET),$(basename $(wildcard *.c))))

all: $(OFILES)

clean:
	rm $(OFILES) $(TARGET)

.c.o:
	$(CC) -c $< -o $@
