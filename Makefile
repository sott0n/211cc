CFLAGS=-std=c11 -g -static
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

211cc: $(OBJS)
		$(CC) -o 211cc $(OBJS) $(LDFLAGS)

$(OBJS): 211cc.h

test: 211cc
		./211cc -test
		./test.sh

clean:
		rm -f 211cc *.o *~ tmp*

.PHONY: test clean