#!/bin/bash
DIR="$( cd -P "$( dirname "$0" )" && pwd )"
source ${DIR}/env.sh

if [ ! -d $PROFILE_DIR ]; then
    echo "creating profiling dir: ${PROFILE_DIR}"
    mkdir -p $PROFILE_DIR;
fi

pushd ${PROFILE_DIR}
echo "--- profiling on ${UNAME}"
echo "--- profiling DIR: ${PROFILE_DIR}"
valgrind --tool=callgrind ${BUILD_DIR}/IfcConvert -y ${TEST_FILE}
kcachegrind ./
popd
