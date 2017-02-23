#!/bin/bash
cd ../
#tar -pczf rSOILWAT2.tar.gz rSOILWAT2
R CMD BUILD rSOILWAT2
R CMD INSTALL rSOILWAT2.tar.gz
rm rSOILWAT2.tar.gz
