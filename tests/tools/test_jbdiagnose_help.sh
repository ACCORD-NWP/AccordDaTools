#!/usr/bin/env bash

set -uex

# A unique working directory
wd=$(pwd)
test_wd=$(pwd)/test_jbdiagnose_help

mkdir -p ${test_wd}
cd ${test_wd}

jbdiagnose.sh -h > /dev/null && echo "jbdiagnose.sh help/usage works"

# Clean up
cd ${wd}
rm -rf ${test_wd}
