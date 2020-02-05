DIR="$( cd -P "$( dirname "$0" )" && pwd )"
pushd ${DIR}/../
ROOT_DIR=${PWD}
IFCOS_DIR="${ROOT_DIR}/IfcOpenShell"
SRC_DIR=${IFCOS_DIR}/src
popd
TODAY=`date +%Y%m%d`
UNAME=`uname`
BUILD_TYPE="Debug"
# BUILD_DIR="${ROOT_DIR}/build-${UNAME}"
# don't want to mess with .dir-locals.el too much to get uname
BUILD_DIR="${ROOT_DIR}/build-${UNAME}"
TEST_DIR="${BUILD_DIR}/testing"
if [ ! -d "${TEST_DIR}" ]; then
    echo "no build dir, creating.."
    mkdir -p ${TEST_DIR};
fi

# ordered to size:
TEST_FILE=${ROOT_DIR}/testfiles/743--segfault-on-geometry.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/IfcIShapeProfileDef.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/IfcTrapeziumProfileDef.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/IfcRectangleProfileDef.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/IfcLShapeProfileDef.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/IfcUShapeProfileDef.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/IfcTShapeProfileDef.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/IfcCsgPrimitive.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/mapped_circle.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/ifcopenshell_halfspaces.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/faceted_brep_csg.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/faces.ifc
 

# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/IfcReinforcingBar.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/acad2010_objects.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/nested_mapped_item.ifc
# TEST_FILE=${ROOT_DIR}/3.0-testfiles/ifc2x3/geometrygym_great_court_roof.ifc

#TEST_FILE=${ROOT_DIR}/0.0-in/shit/Koelcentrale.ifc
#TEST_FILE=${ROOT_DIR}/0.0-in/shit/revit2012_janesville_restaurant.ifc

PROFILE_DIR="${ROOT_DIR}/profile-${UNAME}"
ARCHIVE="${ROOT_DIR}/builds/ifcopenshell-${TODAY}-${UNAME}-${BUILD_TYPE}"

GENERATOR="Unix Makefiles"

OCC_INCLUDE_DIR="/usr/include/opencascade"
OCC_LIBRARY_DIR="/usr/lib"


