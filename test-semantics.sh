#! /bin/bash

fail () {
    echo -e "\e[0;31m$1\e[0m"
    exit 1
}

ok () {
    echo -e "\e[0;32m$1\e[0m"
}

bad () {
    echo -e "\e[0;31m$1\e[0m"
}

false_negatives=0

hash=$(git rev-parse --short HEAD)

if [[ -z $(git status -s --untracked-files=no) ]]; then
    res_file="results/${hash}.txt"
else
    res_file="results/${hash}-dirty.txt"
fi

echo "" > $res_file

compiler_file=./semantics

for good in $(find \
        mrjp-tests/good/basic/ \
        lattests201003/lattests/good \
        lattests201003/lattests/extensions \
        tests/good \
        -name *.lat); do
    $compiler_file "${good}" 2> /dev/null > /dev/null

    if [[ $? -ne 0 ]]; then
        ((false_negatives += 1))
        echo $good >> $res_file
        bad "${good}"
    fi
done

if [[ false_negatives -eq 0 ]]; then
    ok "GOOD OK"
fi

echo ""
echo ""

echo "TESTING SEMANTIC ERRORS:"

false_positives=0

for wrong in $(find tests/bad/ lattests201003/lattests/bad -name *.lat); do
    $compiler_file "${wrong}" 2> /dev/null > /dev/null

    if [[ $? -eq 0 ]]; then
        ((false_positives += 1))
        echo $wrong >> $res_file
        bad "${wrong}"
    fi
done

if [[ false_positives -eq 0 ]]; then
    ok "BAD OK"
fi

if [[ false_negatives -eq 0 && false_positives -eq 0 ]]; then
    echo -e "\e[0;32mAll Good\e[0m"
else
    echo -e "\e[0;31mFN:\e[0m $false_negatives \e[0;31mFP:\e[0m  $false_positives"
fi

echo "Written resutls to $res_file"
