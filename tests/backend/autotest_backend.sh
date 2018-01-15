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
while getopts "vd:p:t:o:" opt; do
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
        o)
            outarm=$OPTARG
            ;;
        *) echo "Options : -v [verbose] -d [asml files directory] -t [type]"
            exit 0
            ;;
    esac
done

# File counter
counter=0
# Success counter
okcounter=0
# Going through asml files in the folder to execute them
for folder in "$dir"/*/; do
    # Right now we only check the if then else statement
    if [ "$(basename "$folder")" != "ARM" ] && [ "$(basename "$folder")" != "valid" ]; then
        basename "$folder"
        for file in "$folder"*.asml; do
            counter=$((counter + 1))
            # Removing extention from filename
            filename=$(basename "$file")
            filename="${filename%.*}"
            # Printing filename + path
            echo -e "File: \\e[34m$file\\e[0m"
            # Generating arm
            "$prog" "$file" > "$outarm/$filename.s"
            (cd "$outarm" && make "$filename".arm ) > /dev/null
            RESULT=$(cd "$outarm" && qemu-arm ./"$filename".arm)
            # Printing output from parsing + ARM generation
            # Printing the expected output
            EXP=$(cat "$folder"expected_"$typ"/"$filename".exp)
            if [[ $verb = 1 ]] || [[ $EXP != "$RESULT" ]]; then
                echo -e "\\e[33mOutput  : $RESULT\\e[0m"
                echo -e "\\e[35mExpected :$EXP\\e[0m"
                echo ""
            fi
            # Comparison between the two
            if [[ $RESULT == "$EXP" ]]; then
                echo -e "\\e[7mResult\\e[27m \\e[32mOK\\e[0m"
                okcounter=$((okcounter + 1))
            else 
                echo -e "\\e[7mResult\\e[27m \\e[31mKO\\e[0m"
            fi
            rm "$outarm/$filename.s"
        done
    fi
done
echo -e "\\e[4m->" "$okcounter" tests passed successfully from "$counter" "($((100 * okcounter/counter))%)" "\\e[24m"
