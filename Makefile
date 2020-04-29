CFLAGS=-std=c11 -g -static -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

211cc: $(OBJS)
		$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): 211cc.h

test: 211cc
		./test.sh

clean:
		rm -f 211cc *.o *~ tmp*

.PHONY: test clean