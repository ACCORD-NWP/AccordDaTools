#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
test_wd=$(pwd)/test_plotjbbal_help

mkdir -p ${test_wd}
cd ${test_wd}

plotjbbal.py -h > /dev/null && echo "plotjbbal help/usage works"

# Clean up
cd ${wd}
rm -rf ${test_wd}
