CC = gcc

TARGET =
OFILES = $(addsuffix .o,$(filter-out $(TARGET),$(basename $(wildcard *.c))))

all: $(OFILES)

clean:
	rm $(OFILES) $(TARGET)

.c.o:
	$(CC) -c $< -o $@
