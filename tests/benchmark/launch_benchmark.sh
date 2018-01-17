#!/bin/bash 
# Quick script to launch sierpinsky assuming the compiler has been compiled

./mincamlc sierpinsky_mincaml_bench.ml
cp a.out ARM/bench_mincaml.s
(cd ARM && make && time qemu-arm bench_mincaml.arm && rm bench_mincaml.*)
ocamlbuild sierpinsky_ocaml_bench.byte
time ./sierpinsky_ocaml_bench.byte
