#!/bin/bash
DIR="$( cd -P "$( dirname "$0" )" && pwd )"
source $DIR/env.sh
TAGFILE=".TAGS"
CTAGS_OPTS="--c++-kinds=+p --fields=+iaS --extra=+q "

pushd ${ROOT_DIR}
echo "-- generating tags in ${SRC_DIR}"
ctags -e -f ${TAGFILE} ${CTAGS_OPTS} -R ${SRC_DIR}
popd

CSCOPE_DIR=${ROOT_DIR}/.cscope

if [ ! -d ${CSCOPE_DIR} ]; then
  mkdir -p ${CSCOPE_DIR};
fi

pushd ${CSCOPE_DIR}
rm cscope.*
rm BROWSE

for xx in ${SRC_DIR} ${OCC_INCLUDE_DIR}
do
    find ${xx} -type f -name "*.h" -o -name "*.cpp" -o -name "*.cc" -o -name "*.hxx" -not -name ".#*" >> cscope.files
done

cscope -b -q
ebrowse -f cscope.files

popd

