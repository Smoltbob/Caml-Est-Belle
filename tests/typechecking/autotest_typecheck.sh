#!/bin/bash
# Goal : launch the tests and check that the output is correct
# Specify the paths and outputs for each file ?

# Variable for getopts
OPTIND=1
# Verbose
verb=0

# Parsing arguments with getopts
# vd:p: means expect arguments:
# -v alone (verbose)
# -d with an argument (folder)
# -p with an argument (executable)
while getopts "vd:p:t:" opt; do
    case "$opt" in
        v)
            verb=1
            ;;
        d)
            dir=$OPTARG
            ;;
        p)
            prog="./$OPTARG"
            ;;
        t)  
            typ=$OPTARG
            ;;
        *) echo "Options : -v [verbose] -d [asml files directory] -t [type]"
            exit 0
            ;;
    esac
done

#shift $((OPTIND-1))
#[ "$1" == "--" ] && shift

for folder in "$dir"/*/; do
    # Going through asml files in the folder to execute them
    for file in "$folder"*.ml; do
        # Removing extention from filename
        filename=$(basename "$file")
        filename="${filename%.*}"
        # Printing filename + path
        echo -e "File: \e[34m$file\e[0m"
        # Printing output from parsing + ARM generation
        RESULT=$("$prog" -t "$file" 2>&1)
        if [[ $verb = 1 ]]; then
            echo -e "\e[33mOutput  : $RESULT\e[0m"
            echo ""
        fi
        # Printing the expected output
        EXP=$(cat "$folder"expected_$typ/"$filename".exp)
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
    done
done
