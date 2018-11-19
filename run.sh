#!/bin/bash
set -eux
run() {
  input="$1"
  ros compiler.ros "$input"
}
#run 0
#run 42
run '5+20-4'
