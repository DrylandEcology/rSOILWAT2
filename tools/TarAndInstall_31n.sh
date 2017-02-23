#!/bin/bash
oldDir=$PWD
# For Linux
if [ `uname | awk '{ print $1 }'` = "Linux" ]; then
  cd ../
  tar -c rSOILWAT2 >> rSOILWAT2.tar
  gzip -c rSOILWAT2.tar >> rSOILWAT2.tar.gz
  rm -rf rSOILWAT2.tar
  R CMD INSTALL rSOILWAT2.tar.gz;
  rm -rf rSOILWAT2.tar.gz;
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
