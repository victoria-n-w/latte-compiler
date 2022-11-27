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

make || fail "Could not compile the compiler"

false_negatives=0

for good in mrjp-tests/good/basic/*.lat; do
    ./compiler < "${good}" 2> /dev/null > /dev/null

    if [[ $? -eq 0 ]]; then
        ok "${good}"
    else
        ((false_negatives += 1))
        bad "${good}"
    fi
done

echo ""
echo ""

echo "TESTING SEMANTIC ERRORS:"

false_positives=0

for wrong in $(find tests/bad/ -name *.lat); do
    ./compiler < "${wrong}" 2> /dev/null > /dev/null

    if [[ $? -ne 0 ]]; then
        ok "${wrong}"
    else
        ((false_positives += 1))
        bad "${wrong}"
    fi
done

if [[ false_negatives -eq 0 && false_positives -eq 0 ]]; then
    echo -e "\e[0;32mAll Good\e[0m"
else
    echo -e "\e[0;31mFN:\e[0m $false_negatives \e[0;31mFP:\e[0m  $false_positives"
fi
