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
while getopts "vd:p:" opt; do
    case "$opt" in
        v)
            verb=1
            ;;
        d)
            folder=$OPTARG
            ;;
        p)
            prog="./$OPTARG"
            ;;
        *) echo "Options : -v [verbose] -d [asml files directory]"
            exit 0
            ;;
    esac
done

#shift $((OPTIND-1))
#[ "$1" == "--" ] && shift

# Going through asml files in the folder to execute them
for file in "$folder"/*.asml; do
    # Removing extention from filename
    filename=$(basename "$file")
    filename="${filename%.*}"
    # Printing filename + path
    echo -e "File: \e[34m$file\e[0m"
    # Printing output from parsing + ARM generation
    RESULT=$("$prog" "$file")
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
