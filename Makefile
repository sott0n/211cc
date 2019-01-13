211cc: 211cc.c

test: 211cc
		./211cc -test
		./test.sh

clean:
		rm -f 211cc *.o *~ tmp*