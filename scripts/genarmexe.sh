#!/bin/bash
# This script generates the ARM executable code from an ARM assembly file
# Syntax : ./genarmexe prog.s
# Will output the ARM executable prog in the caller folder

if [ "$#" -ne 1 ]; then
    echo "Syntax : ./genarmexe prog.s"
    exit 1
fi
file="$1"
if [ "${file: -2}" != ".s" ]; then
    echo "Error: Excpected *.s file"
    exit 2
fi

# Move our file to the ARM directory, containing the necessary makefile
cp "$file" ARM/
# Get the base filename
filename=$(basename "$file")
filename="${filename%.*}"
# Generate the executable code and move it to our folder
(cd ARM/ && make > /dev/null && rm "$file")
mv ARM/"$filename".arm "$filename"
