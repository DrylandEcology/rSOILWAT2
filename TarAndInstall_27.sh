#!/bin/bash
oldDir=$PWD
# For Linux
if [ `uname | awk '{ print $1 }'` = "Linux" ]; then
  cd ../
  tar -c Rsoilwat_v27 >> Rsoilwat_v27.tar
  gzip -c Rsoilwat_v27.tar >> Rsoilwat_v27.tar.gz
  rm -rf Rsoilwat_v27.tar
  R CMD INSTALL Rsoilwat_v27.tar.gz;
  rm -rf Rsoilwat_v27.tar.gz;
  cd $oldDir
fi
# For MacOS X
if [ `uname | awk '{ print $1 }'` = "Darwin" ]; then
  wd=`pwd | awk -F"/" '{print $NF}'`
  cd src/
  make clean;
  cd ../../
  R CMD INSTALL $wd
  cd $oldDir
fi
