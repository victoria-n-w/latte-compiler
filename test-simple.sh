#! /bin/bash

fail () {
    echo -e "\e[0;31m$1\e[0m"
    exit 1
}

error(){
    echo -e "\e[0;31m$1\e[0m"
}

ok() {
    echo -e "\e[0;32m$1\e[0m"
}

tmp_out=$(mktemp -t llvm-XXXXXX.output)

for f in $(find \
        lattests201003/lattests/good \
        lattests201003/lattests/extensions/struct \
        -name "*.output"); do
    echo "Testing ${f%.output}"

    # create a .bc file
    ./latc_llvm ${f%.output}.lat

    if [[ $? -ne 0 ]]; then
        error "File ${f%.output} failed to compile"
        continue
    fi

    bc_out=${f%.output}.bc

    # run the .bc file
    lli $bc_out > $tmp_out

    diff $tmp_out $f > /dev/null 2>&1

    if [[ $? -ne 0 ]]; then
        error "File ${f%.output} failed to produce expected output"
    else
        ok $f
    fi

done
