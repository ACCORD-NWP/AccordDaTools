#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
test_wd=$(pwd)/test_obstool_help

mkdir -p ${test_wd}
cd ${test_wd}

obstool.py  -h > /dev/null && echo "obstool  help/usage works"


# Clean up
cd ${wd}
rm -rf ${test_wd}
