#!/bin/bash
cd ../
#tar -pczf Rsoilwat_v27.tar.gz Rsoilwat_v27
R CMD BUILD Rsoilwat*
mv Rsoilwat_0.27* Rsoilwat_v27.tar.gz
R CMD INSTALL Rsoilwat_v27.tar.gz
rm Rsoilwat_v27.tar.gz
