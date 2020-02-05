DIR="$( cd -P "$( dirname "$0" )" && pwd )"
source $DIR/env.sh

rdm &
pushd ${BUILD_DIR}
rc -J .
popd

popd
