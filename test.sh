#!/bin/bash

type=$1

if [ -z "$type" ]; then
  echo "Usage: $0 <test_name|all>"
  exit 1
fi

if [ "$type" = "all" ]; then
  for file in test/*_test.rkt; do
    result=$(racket "$file" 2>&1)
    if [ -z "$result" ]; then
      echo "Test \"$file\" succeeded"
    else
      echo "Test \"$file\" failed with error: $result"
      exit 125
    fi
  done
  exit 0
fi


result=$(racket "test/${type}_test.rkt" 2>&1)
if [ -z "$result" ]; then
  echo "Test \"$type\" succeeded"
else
  echo "Test \"$type\" failed with error: $result"
  exit 125
fi
