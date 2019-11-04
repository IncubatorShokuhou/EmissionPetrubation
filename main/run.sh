#!/bin/bash
###set -ex
F90=ifort
F90OPT='-convert big_endian -assume byterecl'
export NETCDF=/opt/soft/netcdf/c_4.3.3.1_cxx_4.2.1_fortran_4.4.2/2/  
export NETCDF_LIB=$NETCDF/lib
export NETCDF_INC=$NETCDF/include
LIB_NETCDF="-L$NETCDF_LIB -lnetcdff"
INC_NETCDF="-I$NETCDF_INC"
###determine number of members
read -p "Input the number of members [1~999]:" val echo $val
###change number of members in perl file
sed -i "s|Nmembers=30|Nmembers="$val"|g" ../c*.pl
###change dir to .. and remove all the member files of last run.sh
cd ..
rm -rf ???/
### run perl file to mkdirs and copy the origin file and copy*.sh
perl cr*pl
###change back to default 30 in perl file
sed -i "s|Nmembers="$val"|Nmembers=30|g" c*pl
cd main
echo "NORMAL END perl"
###change number og members in all .f90 files
sed -i "s|Nmembers=30|Nmembers="$val"|g" *.f90
read -p "Determine if LogNormal of Normal. Print 1 if LogNormal. Print other numbers if Normal:" val2 echo $val2
echo $val2
if [[ $val2 = "1" ]];then
    echo "LogNormal"  
   ###
   ### mt19937ar
   ###
   PGM=mt19937ar
   test -f $PGM && rm $PGM
   $F90 $F90OPT $INC_NETCDF -c mt19937ar.f90
   ###
   ### wrfchemi
   ###
   PGM=wrfchemi
   test -f $PGM && rm $PGM
   $F90 $F90OPT $INC_NETCDF -c wrfchemi.f90
   $F90 -o $PGM *.o $LIB_NETCDF
   rm *.o
   echo "NORMAL END"
   ./wrfchemi
   echo "NORMAL END one nc file"
else
    echo "Normal"
    ###change LogNormal to Normal
    sed -i "s|LogNormal=.TRUE.|LogNormal=.FALSE.|g" wrfchemi*.f90
   ###
   ### mt19937ar
   ###
   PGM=mt19937ar
   test -f $PGM && rm $PGM
   $F90 $F90OPT $INC_NETCDF -c mt19937ar.f90 
   ###
   ### wrfchemi
   ###
   PGM=wrfchemi
   test -f $PGM && rm $PGM
   $F90 $F90OPT $INC_NETCDF -c wrfchemi.f90
   $F90 -o $PGM *.o $LIB_NETCDF
   rm *.o
   echo "NORMAL END"
   ./wrfchemi
   echo "NORMAL END one nc file"
   ###change back to default LogNormal
   sed -i "s|LogNormal=.FALSE.|LogNormal=.TRUE.|g" wrfchemi*.f90
fi
###end generating 1 nc file.Time for copy and change Times;copying processes are contained in the changedate.f90 file
###
### changedate11
###
#PGM=changedate11
#test -f $PGM && rm $PGM
#$F90 $F90OPT $INC_NETCDF -c changedate11.f90
#$F90 -o $PGM *.o $LIB_NETCDF
#rm *.o
#echo "NORMAL END changedate11"
###./changedate11
#PGM=changedate12
#test -f $PGM && rm $PGM
#$F90 $F90OPT $INC_NETCDF -c changedate12.f90
#$F90 -o $PGM *.o $LIB_NETCDF
#rm *.o
#echo "NORMAL END changedate12"
###./changedate12
###finish changing
#turn the Nmembers back to default 30 in all f90 files
sed -i "s|Nmembers="$val"|Nmembers=30|g" *.f90
###remove the not-existing 11-31 files
#rm -f ../*/wrfchemi_d01_2016-11-31_*.nc
echo "ALL FINISHED"