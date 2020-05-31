CFLAGS=-std=c11 -g -static -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

211cc: $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): 211cc.h

211cc-stage2: 211cc $(SRCS) 211cc.h self.sh
	./self.sh tmp-stage2 ./211cc 211cc-stage2

211cc-stage3: 211cc-stage2
	./self.sh tmp-stage3 ./211cc-stage2 211cc-stage3

test: 211cc tests/extern.o
	./211cc tests/tests.c > tmp.s
	gcc -static -o tmp tmp.s tests/extern.o
	./tmp

test-stage2: 211cc-stage2 tests/extern.o
	./211cc-stage2 tests/tests.c > tmp.s
	gcc -static -o tmp tmp.s tests/extern.o
	./tmp

test-stage3: 211cc-stage3
	diff 211cc-stage2 211cc-stage3

test-all: test test-stage2 test-stage3

clean:
	rm -rf 211cc 211cc-stage* *.o *~ tmp* tests/*~ tests/*.o

.PHONY: test clean
