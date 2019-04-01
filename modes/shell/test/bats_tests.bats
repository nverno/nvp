#!/usr/bin/env bats

setup() {
    NUM1=8
}

@test "addition using bc" {
    result="$(echo 2+1 | bc)"
    [ "$result" -eq 3 ]
}

@test "addition using dc" {
    result="$(echo 2 2+p | dc)"
    [ "$result" -eq 4 ]
}

@test "blahblah" {
    result="$((2**$NUM1))"
    [ "$result" -eq 256 ]
}

teardown() {
    echo out
}
