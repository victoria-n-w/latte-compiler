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

for good in mrjp-tests/good/basic/*.lat; do
    ./compiler < "${good}" 2> /dev/null > /dev/null

    if [[ $? -eq 0 ]]; then
        ok "${good}"
    else
        bad "${good}"
    fi
done

echo ""
echo ""

echo "TESTING SEMANTIC ERRORS:"

for wrong in tests/bad/*.lat; do
    ./compiler < "${wrong}" 2> /dev/null > /dev/null

    if [[ $? -ne 0 ]]; then
        ok "${wrong}"
    else
        bad "${wrong}"
    fi
done
