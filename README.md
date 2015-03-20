# Rsoilwat
A R plugin for soilwat.

Clone and get submodules
----------------------
git clone -b Rsoilwat_v31 --single-branch --recursive https://github.com/Burke-Lauenroth-Lab/Rsoilwat.git Rsoilwat_v31

Clone and get submodules
----------------------
git clone https://github.com/Burke-Lauenroth-Lab/Rsoilwat.git Rsoilwat_v31
cd Rsoilwat_v31/
git checkout Rsoilwat_v31
git submodule init
git submodule update
cd ..

Install
----------------------
## Note that TarAndInstall_31.sh works for mac ##
tar -pczf Rsoilwat_v31.tar.gz Rsoilwat_v31
R CMD INSTALL Rsoilwat_v31.tar.gz
