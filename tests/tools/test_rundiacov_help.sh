#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
test_wd=$(pwd)/test_rundiacov_help

mkdir -p ${test_wd}
cd ${test_wd}

rundiacov.sh -h > /dev/null && echo "diag_diacov help/usage works"


# Clean up
cd ${wd}
rm -rf ${test_wd}
