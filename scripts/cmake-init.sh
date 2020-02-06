#!/usr/bin/bash
DIR="$( cd -P "$( dirname "$0" )" && pwd )"
source ${DIR}/env.sh

NO_WARNINGS="-Wno-overloaded-virtual -Wno-ignored-qualifiers"

CMAKE_SETTINGS="-DCMAKE_EXPORT_COMPILE_COMMANDS=ON "
CMAKE_SETTINGS+="-Wno-dev "
CMAKE_SETTINGS+="-DOCC_INCLUDE_DIR=/usr/include/opencascade "
CMAKE_SETTINGS+="-DOCC_LIBRARY_DIR=/usr/lib "
CMAKE_SETTINGS+="-DBUILD_EXAMPLES=OFF "
CMAKE_SETTINGS+="-DCMAKE_BUILD_TYPE=Debug "
CMAKE_SETTINGS+="-DCMAKE_INSTALL_PREFIX=/opt/ifcopenshell-git "
CMAKE_SETTINGS+="-DPYTHON_INCLUDE_DIR=/usr/include/python3.8 "
CMAKE_SETTINGS+="-DPYTHON_EXECUTABLE=/bin/python3 "
CMAKE_SETTINGS+="-DPYTHON_LIBRARY=/usr/lib/libpython3.8.so "
CMAKE_SETTINGS+="-DLIBXML2_INCLUDE_DIR=/usr/include/libxml2 "
CMAKE_SETTINGS+="-DLIBXML2_LIBRARIES=/usr/lib/libxml2.so "
CMAKE_SETTINGS+="-DSWIG_DIR=/usr/share/swig/4.0.1 "
CMAKE_SETTINGS+="-DSWIG_EXECUTABLE=/bin/swig "
CMAKE_SETTINGS+="-DSWIG_VERSION=4.0.1 "


# CMAKE_SETTINGS+="-DSWIG_DIR=/usr/share/swig/3.0.12 "
# CMAKE_SETTINGS+="-DSWIG_EXECUTABLE=/bin/swig3 "
# CMAKE_SETTINGS+="-DSWIG_VERSION=3.0.12 "
# CMAKE_SETTINGS+="-DCMAKE_C_COMPILER=clang "
# CMAKE_SETTINGS+="-DCMAKE_CXX_COMPILER=clang++ "
# CMAKE_SETTINGS+="-DCMAKE_C_FLAGS_DEBUG=-O0 -ggdb ${NO_WARNINGS} "
# CMAKE_SETTINGS+="-DCMAKE_CXX_FLAGS_DEBUG=-O0 -ggdb -Wall ${NO_WARNINGS} "
# CMAKE_SETTINGS+="-DCMAKE_CXX_FLAGS=-Wall -Wextra -Wno-ignored-qualifiers "
# CMAKE_SETTINGS+="-DUSE_IFC4=OFF "
# CMAKE_SETTINGS+="-DCOLLADA_SUPPORT=OFF "
# CMAKE_SETTINGS+="-DBUILD_IFCPYTHON=OFF "
# CMAKE_SETTINGS+="-DUNICODE_SUPPORT=OFF "
# CMAKE_SETTINGS+="-DCMAKE_INSTALL_PREFIX=${ARCHIVE} "
#CMAKE_SETTINGS+="-DOCC_INCLUDE_DIR=/usr/include/opencascade "
#CMAKE_SETTINGS+="-Dgp_Pnt_hxx=/usr/include/opencascade/gp_Pnt.hxx "

echo "##############################################################################"
echo "##  DIR            :  ${DIR}"
echo "##  ROOT DIR       :  ${ROOT_DIR}"
echo "##  BUILD DIR      :  ${BUILD_DIR}"
echo "##  OCCT path      :  ${OCCT_PATH}"
echo "##  CMAKE settings :  ${CMAKE_SETTINGS}"

if [ ! -d $BUILD_DIR ]; then
  mkdir -p $BUILD_DIR;
fi
cd $BUILD_DIR

cmake -G "Unix Makefiles" ${CMAKE_SETTINGS} $ROOT_DIR/IfcOpenShell/cmake/
