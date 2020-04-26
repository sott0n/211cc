#!/bin/bash

assert() {
    expected="$1"
    input="$2"

    ./211cc "$input" > tmp.s
    gcc -o tmp tmp.s
    ./tmp
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "Fail test: $input"
        echo "$expected expected, but got $actual"
        exit 1
    fi
}

assert 0 0
assert 42 42
assert 21 '5+20-4'
assert 41 " 12 + 34 - 5 "
assert 47 "5+6*7"
assert 15 "5*(9-6)"
assert 4 "8/2"
assert 4 "(3+5)/2"
assert 10 "-10+20"
assert 1 "a=1;"
assert 100 "a = 73 + 27;"
assert 4 "b = 4"
assert 10 "a = b = 10;"
assert 15 "c = 3 + 7; c + 5;"
assert 10 "a = 3; b = a + 7;"
assert 10 "a = 3; b = 7; c = a + b;"
assert 1 "5 == 5;"
assert 1 "99 != 98;"
assert 0 "2 == 5;"
assert 0 "10 != 10;"
assert 1 "a = 3; a == 3;"
assert 1 "2 > 1;"
assert 0 "2 < 1;"
assert 0 "5 > 5;"
assert 1 "5 >= 5;"
assert 0 "10 < 10;"
assert 1 "10 <= 10;"

echo Test OK