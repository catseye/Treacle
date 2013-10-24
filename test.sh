#!/bin/sh

cd src
huski tests.scm > test-output.txt || exit 1
cat test-output.txt
if grep -q FAILED test-output.txt; then
    echo "FAILED"
    rm test-output.txt
    exit 1
else
    echo "PASSED"
    rm test-output.txt
    exit 0
fi



