#! /bin/bash

fail () {
    echo -e "\e[0;31m$1\e[0m"
    exit 1
}

in_file=$1

if [[ -z $in_file ]]; then
    fail "No input file"
fi

if [[ ! -f $in_file ]]; then
    fail "Input file does not exist"
fi

# ll_out=$(mktemp -t llvm-XXXXXX.ll)
ll_out=core.ll
bc_out=$(mktemp -t llvm-XXXXXX.bc)
out_file=$(mktemp -t llvm-XXXXXX.bc)

# ./compiler $in_file > $ll_out || fail "Compilation failed"

llvm-as $ll_out -o $bc_out || fail "LLVM assembly failed"

llvm-link -o $out_file $bc_out lib/runtime.bc || fail "Linking failed"

lli $out_file || fail "Execution failed"

# rm $ll_out $bc_out $out_file
rm $bc_out $out_file
