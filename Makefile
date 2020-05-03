CFLAGS=-std=c11 -g -static -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

211cc: $(OBJS)
		$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): 211cc.h

test: 211cc
		./211cc tests/tests.c > tmp.s
		gcc -static -o tmp tmp.s
		./tmp

clean:
		rm -rf 211cc *.o *~ tmp* tests/*~ tests/*.o

.PHONY: test clean