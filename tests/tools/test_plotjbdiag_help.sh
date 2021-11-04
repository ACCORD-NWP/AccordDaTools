#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
test_wd=$(pwd)/test_plotjbdiag_help

mkdir -p ${test_wd}
cd ${test_wd}

plotjbdiag.py -h > /dev/null && echo "plotjbdiag help/usage works"


# Clean up
cd ${wd}
rm -rf ${test_wd}
