#!/bin/bash
DIR="$( cd -P "$( dirname "$0" )" && pwd )"
source ${DIR}/env.sh

echo "TEST_INPUT_DIR : ${TEST_FILES_DIR}"
echo "TEST_DIR       : ${TEST_DIR}"
echo "TEST_FILE      : ${TEST_FILE}"

pushd ${TEST_DIR}
PATH=${BUILD_DIR}:$PATH

gdb --args ../IfcConvert -y ${TEST_FILES_DIR}/${TEST_FILE}
#IfcInfo --y ${TEST_FILES_DIR}/${TEST_FILE}

popd
