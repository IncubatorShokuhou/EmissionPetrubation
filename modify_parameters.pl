#!/usr/bin/perl
# PERL script to modify a number of different files to set up
# the aerosol assimilation system LETKF-SP properly
# author  : N.A.J. Schutgens
# affil  : CCSR, University of Tokyo
# history: Jan 31 - Feb , migration to NIES SX-8
#          Jul 14, 2009: support for multiple timeslots
#          Sep 28, 2010: support for perturbations.pl

# Read argument that contains ensemble size
$SPRINTARS = $ARGV[0];
$proj      = $ARGV[1];
$EKALMAN   = $ARGV[2];
$nens      = $ARGV[3];
$nlag      = $ARGV[4];
$nslt      = $ARGV[5];
$iostep    = $ARGV[6];
$patch     = $ARGV[7];
$inflation = $ARGV[8];
$nforecast = $ARGV[9];


# Modify <sprintars.csh> ,< ico2Restart.info >and < perturbations.pl > for each ensemble member
foreach $iens (0..$nens-1) {
   $name = $iens;
   if ($iens < 100) { $name = "0".$name };
   if ($iens < 10)  { $name = "0".$name };
   @lines = &readfile("$SPRINTARS/members/$name/sprintars.csh");
   foreach $line (@lines) {
      if ($line =~ m/DTL\s*=\s*(\d*\.\d+D\d+)/ ) {
         $step = $iostep*3600.0/$1;
      }
      }
   foreach $line (@lines) {
#      $line =~ s/nmrstr\s+(.*),\s*tintv\s*=\s*[0-9]*,/nmrstr $1,tintv=$iostep,/;
#      if ($line =~ m/NMHISD\s*(.*),(.*),\s*step\s*=\s*([0-9])*/) {
#	 if ($step != $3) {     
#            $line =~ s/NMHISD\s*(.*),(.*),\s*step\s*=\s*[0-9]*/NMHISD $1, $2, step = $step/;
#	    print "Changed: step    in 'NMHISD namelist' .\n"
#         } 
#      }
      if ($line =~ m/output_step\s*=\s*([0-9]*)\s*,/) {
	 if ($step != $1) {     
            $line =~ s/output_step\s*=\s*[0-9]*\s*,/output_step        = $step,/;
	    print "Changed: output_step    in 'RESTART namelist' . \n"
         } 
      } 
   }
   &writefile("$SPRINTARS/members/$name/sprintars.csh",@lines);
   

}

#Modify the perturb file###----------------------------------------------------------------------------------------------   
@lines = &readfile("$SPRINTARS/members/perturbations.pl");
foreach $line (@lines) {
   $line =~ s|(\w+)\s+(\d+)\s+(.*)|"`$EKALMAN/perturb.exe $1 ".($2)." $3`;"|e;
}
&writefile("$SPRINTARS/members/perturbations.pl",@lines);
@lines = &readfile("$SPRINTARS/members/perturbations.pl");
foreach $line (@lines) {
   $line =~s/Nens/$nens/;
   $line =~s/Nslot/$nslt*$nforecast/;
   $line =~s/Nlag/$nlag/;
}
&writefile("$SPRINTARS/members/perturbations.pl",@lines);

# Modify $nens, $nprc, $nlag, $nslt, $iostep, $cyclestep in < assimilate.pl >
@lines = &readfile("assimilate.pl");
foreach $line (@lines) {
   if ($line =~ m/nens\s*=\s*([0-9]*)\s*;/) {
      if ($nens != $1) {
         $line =~ s/nens\s*=\s*[0-9]*(\s*);/nens      = $nens$1;/;
         print "   Changed: nens      in 'assimilate.pl'.\n";
      }
   }
   if ($line =~ m/nlag\s*=\s*([0-9]*)\s*;/) {
      if ($nlag != $1 ) {
         $line =~ s/nlag\s*=\s*[0-9]*(\s*);/nlag      = $nlag$1;/;
         print "   Changed: nlag      in 'assimilate.pl'.\n";
      }
   }
   if ($line =~ m/nslt\s*=\s*([0-9]*)\s*;/) {
      if ($nslt != $1 ) {
         $line =~ s/nslt\s*=\s*[0-9]*(\s*);/nslt      = $nslt$1;/;
         print "   Changed: nslt      in 'assimilate.pl'.\n";
      }
   }
   if ($line =~ m/iostep\s*=\s*([0-9]*)\s*;/) {
      if ($iostep != $1 ) {
         $line =~ s/iostep\s*=\s*[0-9]*(\s*);/iostep    = $iostep$1;/;
         print "   Changed: iostep    in 'assimilate.pl'.\n";
      }
   }
   if ($line =~ m/cyclestep\s*=\s*([0-9]*)\s*;/) {
      if ($iostep*$nslt != $1 ) {
         $cyclestep = $nslt*$iostep;
         $line =~ s/cyclestep\s*=\s*[0-9]*(\s*);/cyclestep = $cyclestep$1;/;
         print "   Changed: cyclestep in 'assimilate.pl'.\n";
      }
   }
   if ($line =~ m/nforecast\s*=\s*([0-9]*)\s*;/) {
      if ($nforecast != $1 ) {
         $line =~ s/nforecast\s*=\s*[0-9]*(\s*);/nforecast = $nforecast$1;/;
         print "   Changed: nforecast in 'assimilate.pl'.\n";
      }
   }
}
&writefile("assimilate.pl",@lines);


# Modify nens, addslots and nlag in parameters.f90 
@lines = &readfile("$EKALMAN/src/parameters.f90");
foreach $line (@lines) {
   if ($line =~ m/nens\s*=\s*([0-9]*)\s*!/) {
      if ($nens != $1) {
         $line =~ s/nens\s*=\s*[0-9]*(\s*!)/nens = $nens$1/;
         print "   Changed: nens     in $EKALMAN/src/parameters.f90.\n";
      }
   }
   if ($line =~ m/addslots\s*=\s*([0-9]*)\s*!/) {
      if ($nslt != $1+1) {
         $line =~ s/addslots\s*=\s*[0-9]*(\s*!)/"addslots    = ".($nslt-1)."$1"/e;
         print "   Changed: addslots in EKALMAN/src/parameters.f90.\n";
      }
   }   
   if ($line =~ m/nlag\s*=\s*([0-9]*)\s*!/) {
      if ($nlag != $1) {
         $line =~ s/nlag\s*=\s*[0-9]*(\s*!)/nlag        = $nlag$1/;
         print "   Changed: nlag     in EKALMAN/src/parameters.f90.\n";
      }
   }
   if ($line =~ m/patch\s*=\s*([0-9]*)/) {
      if ($patch != $1 ) {
         $line =~ s/patch\s*=\s*[0-9]*(\.0D0\s*!)/patch = $patch$1/;
         print "   Changed: patch in 'assimilate.pl'.\n";
      }
   }
   if ($line =~ m/inflation\s*=\s*([0-9]*)/) {
      if ($inflation != $1 ) {
         $line =~ s/inflation\s*=\s*0\.03(d0\s*!)/inflation = $inflation$1/;
         print "   Changed: inflation in 'assimilate.pl'.\n";
      }
   }
}
&writefile("$EKALMAN/src/parameters.f90",@lines);

### Added by Tie Dai for sr16000
!system  "cp assimilate.pl ~/short/$proj/"
            or die "Error: Cannot cp assimilation.pl file \n";
!system  "cp start_job ~/short/$proj/start_4";
!system  "cp launch4.pl ~/short/$proj/launch4.pl";
### Modify start_job  
#@lines = &readfile("/short/p11060/$proj/start_$proj");
#foreach $line (@lines) {
#         $line =~ s/^cd\s*\D*\d*\D*/cd \/short\/p11060\/$proj \n/;
#}
#&writefile("/short/p11060/$proj/start_4",@lines);


# SUBROUTINES
#===============================================================================

# read contents of a file
sub readfile {
   my $file = $_[0];
   open INPUT, "<$file";
   @lines = <INPUT>;
   close INPUT;
   @lines;
}


# write contents to (new) file
sub writefile {
   my $file = shift(@_);
   my @lines = @_;
   open OUTPUT, ">$file";
   print OUTPUT @lines;
   close OUTPUT;
}
