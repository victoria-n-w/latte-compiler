#! /bin/bash

fail () {
    echo -e "\e[0;31m$1\e[0m"
    exit 1
}

error () {
    echo -e "\e[0;31m$1\e[0m"
}

make || fail "Compilation failed"

# Tests all bad files, to make sure they fail

for file in $(find tests/bad lattests201003/lattests/bad -type f -name *.lat); do
    ./compiler $file > /dev/null 2>&1
    if [[ $? -eq 0 ]]; then
        echo -e "\e[0;31mBad file $file compiled successfully\e[0m"
    fi
done

# Tests all good files, to make sure they succeed
# Compare the output to the expected output

# count the total number of failures
failures=0

out_file=$(mktemp -t llvm-XXXXXX)
for file in $(find lattests201003/lattests/good -type f -name *.lat); do
    # if there is an input file, feed it to the program
    if [[ -f ${file%.lat}.input ]]; then
        cat ${file%.lat}.input | ./llvm.sh $file > $out_file
    else
        ./llvm.sh $file > $out_file
    fi

    if [[ $? -ne 0 ]]; then
        echo -e "\e[0;31mGood file $file failed to compile\e[0m"
        ((failures++))
        continue
    fi

    if [[ ! -f ${file%.lat}.output ]]; then
        echo -e "\e[0;33mWarning: No expected output for $file\e[0m"
        continue
    fi

    diff $out_file ${file%.lat}.output > /dev/null 2>&1
    if [[ $? -ne 0 ]]; then
        echo -e "\e[0;31mGood file $file failed to produce expected output\e[0m"
        ((failures++))
        continue
    fi

    # print filename in green
    echo -e "\e[0;32m$file\e[0m"
done

echo "$((failures)) failures"
