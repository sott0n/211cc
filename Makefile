CFLAGS=-std=c11 -g -static -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

211cc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): 211cc.h

211cc-stage2: 211cc $(SRCS) 211cc.h self.sh
	./self.sh

test: 211cc tests/extern.o
	./211cc tests/tests.c > tmp.s
	gcc -static -o tmp tmp.s tests/extern.o
	./tmp

test-stage2: 211cc-stage2 tests/extern.o
	./211cc-stage2 tests/tests.c > tmp.s
	gcc -static -o tmp tmp.s tests/extern.o
	./tmp

clean:
	rm -rf 211cc 211cc-stage* *.o *~ tmp* tests/*~ tests/*.o

.PHONY: test clean
