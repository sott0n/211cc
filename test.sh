#!/bin/bash

try() {
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

try 0 0
try 42 42
try 21 '5+20-4'
try 41 " 12 + 34 - 5 "
try 47 "5+6*7"
try 15 "5*(9-6)"
try 4 "8/2"
try 4 "(3+5)/2"
try 1 "a=1;"
try 100 "a = 73 + 27;"
try 4 "b = 4"
try 10 "a = b = 10;"
try 15 "c = 3 + 7; c + 5;"
try 10 "a = 3; c = a + 7;"

echo Test OK