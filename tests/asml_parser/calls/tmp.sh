#!/bin/bash
# Goal : launch the tests and check that the output is correct
# Specify the paths and outputs for each file ?

for file in *.asml; do
    # Removing extention from filename
    filename=$(basename "$file")
    filename="${filename%.*}"
    echo $file
    ./../../../scripts/asmlparse $file > expected_asml/$filename.exp
done
