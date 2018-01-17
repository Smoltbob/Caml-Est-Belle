#!/bin/bash 
# Quick script to launch sierpinsky assuming the compiler has been compiled

./mincamlc sierpinsky_mincaml.ml
cp a.out ARM/sierpinsky.s
(cd ARM && make && qemu-arm sierpinsky.arm && rm sierpinsky.*)
