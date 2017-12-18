#!/bin/bash
# Goal : launch the tests and check that the output is correct
# Specify the paths and outputs for each file ?

# No_let

if [[ $1 = "-v" ]]; then
    verb=1
else
    verb=0
fi

folder="let"

for file in "$folder"/*.asml; do
    # Removing extention from filename
    filename=$(basename "$file")
    filename="${filename%.*}"
    # Printing filename + path
    echo -e "File: \e[34m$file\e[0m"
    # Printing output from parsing + ARM generation
    RESULT=$(./../../../backend/asml_parser/asmlparse "$file")
    if [[ $verb = 1 ]]; then
        echo -e "\e[33mOutput  : $RESULT\e[0m"
        echo ""
    fi
    # Printing the expected output
    EXP=$(cat "$folder"/"$filename".exp)
    if [[ $verb = 1 ]]; then
        echo -e "\e[35mExpected :$EXP\e[0m"
        echo ""
    fi
    # Comparison between the two
    if [[ $RESULT == "$EXP" ]]; then
        echo -e "\e[7mResult\e[27m \e[32mOK\e[0m"
    else 
        echo -e "\e[7mResult\e[27m \e[31mKO\e[0m"
    fi
    echo "*********"
done
