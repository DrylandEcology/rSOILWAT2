#!/bin/bash
oldDir=$PWD
# For Linux
if [ `uname | awk '{ print $1 }'` = "Linux" ]; then
  cd ../
  tar -c Rsoilwat_v31 >> Rsoilwat.tar
  gzip -c Rsoilwat.tar >> Rsoilwat.tar.gz
  rm -rf Rsoilwat.tar
  R CMD INSTALL Rsoilwat.tar.gz;
  rm -rf Rsoilwat.tar.gz;
  cd $oldDir
fi
# For MacOS X
if [ `uname | awk '{ print $1 }'` = "Darwin" ]; then
  wd=`pwd | awk -F"/" '{print $NF}'`
  cd src/
  make cleaner;
  rm SW_R_lib.o
  cd ../../
  R CMD INSTALL $wd
  cd $wd/src/
  make cleaner;
  cd $oldDir
fi
