#!/bin/bash
#set -eux
try() {
  expected="$1"
  input="$2"
  ros roswell/cl-9cc.ros "$input" > tmp.s
  gcc -static -o tmp tmp.s
  ./tmp
  actual="$?"
	echo $actual
  if [ "$actual" != "$expected" ]; then
    echo "$input expected, but got $actual"
    exit 1
  fi
}

try 10 'return 2*3+4;'
try 14 'return 2+3*4;'
try 26 'return 2*3+4*5;'
try 5 'return 50/10;'
try 9 'return 6*3/2;'

try 0 'return 0;'
try 42 'return 42;'
try 21 '1+2; return 5+20-4;'
try 41 'return 12 + 34 - 5 ;'
try 153 'return 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17;'

echo OK
