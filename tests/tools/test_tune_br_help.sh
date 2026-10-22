#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
test_wd=$(pwd)/test_tune_br_help

mkdir -p ${test_wd}
cd ${test_wd}

tune_br.py  -h > /dev/null && echo "tune_br  help/usage works"


# Clean up
cd ${wd}
rm -rf ${test_wd}
