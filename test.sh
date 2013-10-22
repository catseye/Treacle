#!/bin/sh

cd src
plt-r5rs tests.scm | tee test-output.txt
if grep -q FAILED test-output.txt; then
    echo "FAILED"
    rm test-output.txt
    exit 1
else
    echo "PASSED"
    rm test-output.txt
    exit 0
fi



