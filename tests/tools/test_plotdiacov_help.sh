#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
test_wd=$(pwd)/test_plotdiacov_help

mkdir -p ${test_wd}
cd ${test_wd}

plotdiacov.py -h > /dev/null && echo "plotdiacov help/usage works"


# Clean up
cd ${wd}
rm -rf ${test_wd}
