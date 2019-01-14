CFLAGS=-Wall -std=c11
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

211cc: $(OBJS)

$(OBJS): 211cc.h

test: 211cc
		./211cc -test
		./test.sh

clean:
		rm -f 211cc *.o *~