#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
tool=plotvarbcpred
test_wd=$(pwd)/test_${tool}_help

mkdir -p ${test_wd}
cd ${test_wd}

${tool}.py -h > /dev/null && echo "${tool} help/usage works"

# Clean up
cd ${wd}
rm -rf ${test_wd}
