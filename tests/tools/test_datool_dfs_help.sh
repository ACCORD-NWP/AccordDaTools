#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
test_wd=$(pwd)/test_datool_dfs_help

mkdir -p ${test_wd}
cd ${test_wd}

datool_dfs.py -h > /dev/null && echo "datool_dfs help/usage works"


# Clean up
cd ${wd}
rm -rf ${test_wd}
