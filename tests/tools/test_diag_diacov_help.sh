#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
test_wd=$(pwd)/test_diag_diacov_help

mkdir -p ${test_wd}
cd ${test_wd}

diag_diacov.py -h > /dev/null && echo "diag_diacov help/usage works"


# Clean up
cd ${wd}
rm -rf ${test_wd}
