#!/bin/python3

import ifcopenshell
ifc_file = ifcopenshell.open('/Projects/Github/IfcOpenShell-devenv/testfiles/basic.ifc')
products = ifc_file.by_type('IfcProduct')
for product in products:
    print(product.is_a())
