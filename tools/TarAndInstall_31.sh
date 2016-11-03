#!/bin/bash
cd ../
#tar -pczf Rsoilwat_v31.tar.gz Rsoilwat_v31
R CMD BUILD Rsoilwat*
mv Rsoilwat31_0.31* Rsoilwat_v31.tar.gz
R CMD INSTALL Rsoilwat_v31.tar.gz
rm Rsoilwat_v31.tar.gz
