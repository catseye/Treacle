#!/bin/sh

cd src
csi -b tests.scm > test-output.txt || exit 1
#huski tests.scm > test-output.txt || exit 1
#plt-r5rs tests.scm > test-output.txt || exit 1
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



