#!/bin/bash

pushd build 

cmake \
 -Wno-dev \
 -DCMAKE_BUILD_TYPE=Debug \
-DCMAKE_INSTALL_PREFIX=/opt/ifcopenshell-git \
-DOCC_INCLUDE_DIR=/usr/include/opencascade \
-Dgp_Pnt_hxx=/usr/include/opencascade/gp_Pnt.hxx \
-DPYTHON_INCLUDE_DIR=/usr/include/python3.8 \
-DPYTHON_EXECUTABLE=/bin/python3 \
-DPYTHON_LIBRARY=/usr/lib/libpython3.8.so \
-DLIBXML2_INCLUDE_DIR=/usr/include/libxml2 \
-DLIBXML2_LIBRARIES=/usr/lib/libxml2.so \
-DSWIG_DIR=/usr/share/swig/3.0.12 \
-DSWIG_EXECUTABLE=/bin/swig3 \
-DSWIG_VERSION=3.0.12 \
../cmake/ 

popd
