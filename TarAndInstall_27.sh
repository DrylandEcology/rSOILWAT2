#!/bin/bash
oldDir=$PWD
# For Linux
if [ `uname | awk '{ print $1 }'` = "Linux" ]; then
  cd ../
  tar -c Rsoilwat >> Rsoilwat.tar
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
  make clean;
  cd ../../
  R CMD INSTALL $wd
  cd $oldDir
fi
