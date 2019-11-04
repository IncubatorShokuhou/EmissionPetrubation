#!/usr/bin/perl
# PERL script to build directory structure for the
# aerosol assimilation system LETKF_SP
# author : N.A.J. Schutgens
# affil  : CCSR, University of Tokyo
# History: Jan 31 - Feb , 2008: migration to NIES SX-8
#          Feb 13       , 2008: create_dirs & create_links

# Read argument that contains ensemble size
$Nmembers=30;

# Check for illogical size
if (($Nmembers > 0) and ($Nmembers < 1001)) {

# Create recquired number of directories
   foreach $iens (0..$Nmembers-1) {
      $name = $iens;
      if ($iens < 100) { $name = "0".$name };
      if ($iens < 10)  { $name = "0".$name };
      `mkdir -p $name`;
      `cp /public/daitie/daitietest/lh/EMISSSIONPETRUBATION/origin/1level/wrfchemi_d01_2016-1*-01_00:00:00.nc ./$name`;
      `cp /public/daitie/daitietest/lh/EMISSSIONPETRUBATION/main/copy1*.sh ./$name`;  
   }
 }else {
  print "Error: illegal ensemble size;the number of esembles should be less than 1000.\n";
}
