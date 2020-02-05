#!/bin/bash
DIR="$( cd -P "$( dirname "$0" )" && pwd )"
source $DIR/env.sh

# pushd ${ROOT_DIR}
# rm .clang_complete

# for i in ${COMPILER_FLAGS}; do
#     echo ${i} >> .clang_complete
# done
# for i in ${INC_PATHS}; do
#     echo ${i} >> .clang_complete
# done
# popd

echo "BUILD_DIR: ${BUILD_DIR}"

if [ ! -d "${BUILD_DIR}" ]; then
    echo "no cmake build dir present, regenerating..."
    $DIR/cmake-init.sh;
fi

pushd ${BUILD_DIR}
echo "--- building on ${UNAME}"
# cmake --build . -j ${IFCOS_NUM_BUILD_PROC} --target install
cmake --build . -j 4
popd

