#!/bin/sh
set -ex
F90=ifort
F90OPT='-convert big_endian -assume byterecl'
export NETCDF=/opt/soft/netcdf/c_4.3.3.1_cxx_4.2.1_fortran_4.4.2/2/  
export NETCDF_LIB=$NETCDF/lib
export NETCDF_INC=$NETCDF/include
LIB_NETCDF="-L$NETCDF_LIB -lnetcdff"
INC_NETCDF="-I$NETCDF_INC"
###
### read
###
PGM=read
test -f $PGM && rm $PGM
$F90 $F90OPT $INC_NETCDF -c wrfchemi.f90
$F90 -o $PGM *.o $LIB_NETCDF
rm *.o
###
### init_merge
###
###PGM=init_merge
###test -f $PGM && rm $PGM
###$F90 $F90OPT $INC_NETCDF -c init_merge.f90
###$F90 -o $PGM *.o $LIB_NETCDF
###rm *.o
echo "NORMAL END"
