#! /bin/bash

fail () {
    echo -e "\e[0;31m$1\e[0m" >&2
    exit 1
}

in_file=$1

if [[ -z $in_file ]]; then
    fail "No input file"
fi

if [[ ! -f $in_file ]]; then
    fail "Input file does not exist"
fi

ll_out=$(mktemp -t llvm-XXXXXX.ll)
bc_out=$(mktemp -t llvm-XXXXXX.bc)
s_out=$(mktemp -t llvm-XXXXXX.s)
out_file=$(mktemp -t llvm-XXXXXX)

./compiler $in_file > $ll_out || fail "Compilation failed"

llvm-as $ll_out -o $bc_out || fail "LLVM assembly failed"
llc $bc_out -o $s_out || fail "LLVM compilation failed"

clang $s_out lib/runtime.s -o $out_file || fail "Linking failed"

$out_file || fail "Execution failed"

rm $ll_out $bc_out $out_file $s_out
