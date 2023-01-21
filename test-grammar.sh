#! /bin/bash

for f in $(find \
        lattests201003/lattests/extensions/ \
        lattests201003/lattests/good \
        mrjp-tests/good \
        tests/good \
        -name "*.lat"); do
    echo "Testing $f"
    ./Latte/Test -s $f > /dev/null
    if [ $? -ne 0 ]; then
        echo "Failed to compile $f"
        exit 1
    fi
done
