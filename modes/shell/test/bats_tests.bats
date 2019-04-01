#!/usr/bin/env bats

setup() {
    NUM1=2
}

@test "addition using bc" {
  result="$(echo 2+1 | bc)"
  [ "$result" -eq 3 ]
}

@test "addition using dc" {
  result="$(echo 2 2+p | dc)"
  [ "$result" -eq 4 ]
}
