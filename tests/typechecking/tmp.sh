#!/bin/bash
# Goal : launch the tests and check that the output is correct
# Specify the paths and outputs for each file ?

for file in valid/*.ml; do
    # Removing extention from filename
    filename=$(basename "$file")
    filename="${filename%.*}"
    echo $file
    echo "well typed"> valid/expected_tc/$filename.exp
done
