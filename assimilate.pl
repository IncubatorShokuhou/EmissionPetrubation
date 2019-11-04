#!/usr/bin/perl -w
# PERL script that administrates the aerosol assimilation system LETKF_SP
# author : Tie Dai
# affil  : CCSR, University of Tokyo
# history: SEP  05, 2012; Based on NICK-SAN's work

#=== USER SPECIFIED PARAMETERS =================================================

# Directory for the NICAM bin

###$TOPPATH="\/home\/p11060\/NICAM\/bin";
$TOPPATH="~/NICAM/bin";

# Start date of assimilation cycle
$sYYYY = '2010' ;  # year  (four digits)
$sMM   = '01' ;    # month (two digits)
$sDD   = '01' ;    # day   (two digits)
$sHH   = '00' ;    # hour  (two digits)

# End date of assimilation cycle
$eYYYY = '2010';  # year  (four digits)
$eMM   = '01';    # month (two digits)
$eDD   = '15';    # day   (two digits)
$eHH   = '00';    # hour  (two digits)

# What to do ?
$exec_PERTURB     = 1 ;  #  0 (no) / 1 (yes)
$exec_SPRINTARS   = 1 ;  #  0 (no) / 1 (yes)
$exec_EKALMAN     = 1 ;  #  0 (no) / 1 (yes)
$exec_FORECAST    = 1 ;  #  0 (no) / 1 (yes) (requires $exec_KALMAN=1)
$exec_SMOOTHER    = 0 ;  #  0 (no) / 1 (yes) (requires $exec_KALMAN=1)


#=== CONSTANTS [DON'T TOUCH UNLESS YOU KNOW WHAT YOU ARE DOING ] ===============
$nens      = 20; # number of ensemble members
$nlag      = 0; # lag of filter [cycles]
$nslt      = 1; # number of timeslots
$iostep    = 6; # step between Restart output       [hours]
$cyclestep = 6; # step between assimilation cycles  [hours]
$nforecast = 1; # step forecast nforecast*iostep = [hours]

$exec_SMOOTHER = ($exec_EKALMAN and $exec_SMOOTHER);  # just to prevent mistakes
if (($nlag!=0) and !($exec_SMOOTHER)) {
   print "You don't use the SMOOTHER, yet NLAG <> 0. Please check your Makefile.\n";
   exit;
}

#=== CLEANUP SPRINTARS-DIR =====================================================
`rm -f SPRINTARS/members/???/SYS*`;
`rm -f SPRINTARS/members/???/ERR*`;
`rm -f SPRINTARS/members/???/apriori`;
`rm -f SPRINTARS/members/???/background`;


#=== Create scripts ============================================================
if ($exec_SPRINTARS) {
   foreach $iens (0..$nens-1) {
      &changedirs($iens);
      system "./sprintars.csh";                         # Create start-up script
      chdir "../../..";
   }
}

if ($exec_PERTURB) {
    chdir "SPRINTARS/members";
    system "./perturbations.pl";  # Create perturbations in model parameters
    print "Create perturbations in model parameters Done. \n";
    !system "rm -f pm_*" or
            die 'Failure to rm the pm files /.\n';
    chdir "../..";
}

if ($exec_SPRINTARS) {
   foreach $iens (0..$nens-1) {
      &changedirs($iens);
      chdir "init";
      `$TOPPATH/ico2ll`;                         # Create start-up script
    !system "rm -f pm_*" or
            die 'Failure to rm the pm files /.\n';
      chdir "../../../..";
   }
}
#=== count the total cycle number ============================================================
$ncycle=0;
#===LETKF_SP loop ============================================================
( $YYYY,$MM,$DD,$HH ) = ( $sYYYY,$sMM,$sDD,$sHH );
while(&notfinished( $YYYY,$MM,$DD,$HH )) {
   print "Executing cycle: ".$HH." hrs, ".$DD."/".$MM."/".$YYYY."\n";
   $ncycle=($ncycle+1);

#--- Define several timestamps -------------------------------------------------
   ( $cYYYY,$cMM,$cDD,$cHH ) = ( $YYYY,$MM,$DD,$HH );  # start of current cycle
   foreach $ilag (-$nlag..-1) {
      foreach (1..$nslt) {
         foreach (1..$iostep) {        # before apriori output for current cycle
            ( $YYYY,$MM,$DD,$HH ) = &increasetime( $YYYY,$MM,$DD,$HH );         
         }
      }
      if ($ilag == -$nlag) {                               # start of next cycle
         ( $nYYYY,$nMM,$nDD,$nHH ) = ( $YYYY,$MM,$DD,$HH );
      }
   }
#   ( $oYYYY,$oMM,$oDD,$oHH ) = ( $YYYY,$MM,$DD,$HH );
#   if ($exec_SMOOTHER) {
#      foreach (1..$iostep) {          # start of apriori output      
#         ( $oYYYY,$oMM,$oDD,$oHH ) = &increasetime( $oYYYY,$oMM,$oDD,$oHH );         
#      }
#   }
#--- Update time, links to obs, append parameters ------------------------------
   @obs = 'XXX';
   foreach $islt (reverse(0..$nslt-1)) {
      foreach (1..$iostep) {         # times of apriori output of current cycle
         ( $YYYY,$MM,$DD,$HH ) = &increasetime( $YYYY,$MM,$DD,$HH );
      }
# Link to relevant observations files
      $name = $islt;
      if ($islt < 100) { $name = "0".$name };
      if ($islt < 10)  { $name = "0".$name };
      $name = "b".$name;
      system "rm -f EKALMAN/obs$name";
      !system "ln -s ~/PrepObs/obs/NRL/obs"."$YYYY$MM$DD$HH"." EKALMAN/obs$name" or warn 'Not able to link';
      print " ~/PrepObs/obs/obs"."$YYYY$MM$DD$HH"." EKALMAN/obs$name.\n";
      push( @obs , "      on $YYYY/$MM/$DD at $HH hrs\n" );      
# Append to parameter files with adjusted timestamps
      #if ($exec_SMOOTHER) {                                                     
      #foreach $iens (0..$nens-1) {
      #&changedirs($iens);
      #chdir "init/orig";
      #chomp(@files = <par*>);
      #      chdir "..";
      #      foreach $file (@files) { # if par-files are present
      #         !system "cat orig/$file >> $file" or warn 'Copy failed !!!';
      #      }
      #      chdir "../../../..";        
      #   }           
      #}
   }
   shift(@obs); ## move out XXX

#--- Calculate SPRINTARS apriori -----------------------------------------------
   print "   Apriori calculation by SPRINTARS.\n";
   foreach $iens (0..$nens-1) {
      &changedirs($iens);
      if ($exec_SPRINTARS) {
         @lines = &readfile("apriori");                 # Modify start-up script
         foreach $line (@lines) {
            if ($line =~ m/DTL\s*=\s*(\d*\.\d+)D(\d+)/ ) {
            my $dtstep = join "e", $1, $2;
            if ($exec_FORECAST) {
	       $lsstep = $cyclestep*3600.0*($nlag+1)*$nforecast/$dtstep;
            } else {
	       $lsstep = $cyclestep*3600.0*($nlag+1)/$dtstep;
            }
	    $EnKS_resstep=($cyclestep*$nlag+$iostep)*3600.0/($dtstep)
            }
         }
         foreach $line (@lines) {
            $line =~ s/ndg_start_yr\s*=\s*([0-9]*)/ndg_start_yr = $cYYYY/;
            $line =~ s/ndg_start_mt\s*=\s*([0-9]*)/ndg_start_mt = $cMM/;
            $line =~ s/ndg_start_dy\s*=\s*([0-9]*)/ndg_start_dy = $cDD/;
            $line =~ s/ndg_start_hr\s*=\s*([0-9]*)/ndg_start_hr = $cHH/;
            $line =~ s/start_year\s*=\s*([0-9]*)/start_year = $cYYYY/;
            $line =~ s/start_month\s*=\s*([0-9]*)/start_month = $cMM/;
            $line =~ s/start_day\s*=\s*([0-9]*)/start_day = $cDD/;
            $line =~ s/start_hour\s*=\s*([0-9]*)/start_hour = $cHH/;
            $line =~ s/LSTEP_MAX\s*=\s*[0-9]*/LSTEP_MAX = $lsstep/;
	    $line =~ s/EnKS_RESTART_SSTEP\s*=\s*[0-9]*/EnKS_RESTART_SSTEP = $EnKS_resstep/; 
         }
         &writefile("apriori",@lines);
         `cp apriori nhm_driver.cnf`;
         !system "mpirun $TOPPATH/nhm_driver" or die "Failure to run SPRINTARS.\n";
         print " Apriori calculation by SPRINTARS DONE. \n";
         if ($exec_SMOOTHER) {                                              
            @files = <init/orig/par*>;
            if ( $#files < 0 ) {# Test if par-files already exist
               `cp init/par* init/orig/`;    # save a copy of original parameters
            }
         }
      } else {
         !system "mv init/restart.rgn* ./" or die 'Failure to move restart to init/';
      }
      chdir "../../..";
   }

#--- Execute LETKF/S ------------------------------------------------------------
   if ($exec_EKALMAN) {
      chdir "EKALMAN";
      print "   Assimilating observations\n";
      print @obs;
      if ($exec_SMOOTHER) {
         !system "mpirun ./letks.exe" or die "Failure to run LETKS.\n";
         chdir "..";
      } else {
         !system "mpirun ./letkf.exe" or die "Failure to run LETKF.\n";
         chdir "..";
         foreach $iens (0..$nens-1) {
            &changedirs($iens);
            !system "rm -f init/restart_*.rgn* \n" or die "Failure to rm restart_*";
            #----------------------save the Restart files --------------------------------------------------
            !system "mkdir -p Restart$YYYY$MM$DD$HH\n" or die "Failure to mkdir Restart";
            !system "mv Restart.rgn* Restart$YYYY$MM$DD$HH\n" or die "Failure to move Restart";
            #----------------------save the restart files --------------------------------------------------
            !system "mkdir -p restart$YYYY$MM$DD$HH\n" or die "Failure to mkdir Restart";
            !system "mv restart*rgn* restart$YYYY$MM$DD$HH\n" or die "Failure to move restart";
            !system "cp restart$YYYY$MM$DD$HH/restart_*.rgn* init/" or die "Failure to copy restart_*";
            chdir "../../..";
         }
      }	 
      !system "rm -f pm_*" or die 'Failure to rm the pm files /.\n';
   } else {
      foreach $iens (0..$nens-1) {
         &changedirs($iens);
         !system "mv restart* init/" or
             die 'Failure to move restart files to init/';
         chdir "../../..";
      }
   }
#------------Calculate SPRINTARS background--------------------------------------
   if ($exec_SMOOTHER) {
      print "   Background calculation by SPRINTARS.\n"; 
      foreach $iens (0..$nens-1) {
         &changedirs($iens);
         @lines = &readfile("background");                 # Modify start-up script
         foreach $line (@lines) {
         if ($line =~ m/DTL\s*=\s*(\d*\.\d+)D(\d+)/ ) {
            my $dtstep = join "e", $1, $2;
	    $lsstep = $cyclestep*3600.0/$dtstep;
	    $EnKS_resstep=($iostep)*3600.0/($dtstep)
            }
         }
         foreach $line (@lines) {
            $line =~ s/ndg_start_yr\s*=\s*([0-9]*)/ndg_start_yr = $cYYYY/;
            $line =~ s/ndg_start_mt\s*=\s*([0-9]*)/ndg_start_mt = $cMM/;
            $line =~ s/ndg_start_dy\s*=\s*([0-9]*)/ndg_start_dy = $cDD/;
            $line =~ s/ndg_start_hr\s*=\s*([0-9]*)/ndg_start_hr = $cHH/;
            $line =~ s/start_year\s*=\s*([0-9]*)/start_year = $cYYYY/;
            $line =~ s/start_month\s*=\s*([0-9]*)/start_month = $cMM/;
            $line =~ s/start_day\s*=\s*([0-9]*)/start_day = $cDD/;
            $line =~ s/start_hour\s*=\s*([0-9]*)/start_hour = $cHH/;
            $line =~ s/LSTEP_MAX\s*=\s*[0-9]*/LSTEP_MAX = $lsstep/;
	    $line =~ s/EnKS_RESTART_SSTEP\s*=\s*[0-9]*/EnKS_RESTART_SSTEP = $EnKS_resstep/; 
         }
         &writefile("background",@lines);
         `cp background nhm_driver.cnf`;
         !system "mpirun $TOPPATH/nhm_driver" 
		 or die "Failure to back run SPRINTARS.\n";
         print " Backgroud calculation by SPRINTARS DONE. \n";
         !system "rm -f pm_*" or die 'Failure to rm the pm files /.\n';
	 !system "../../../EKALMAN/shift_slot.exe $nslt $nlag" 
		 or die "Failure to run shift_slot \n";
         !system "rm -f pm_*" or die 'Failure to rm the pm files /.\n';
	 !system "mv restart* init/" 
		 or die 'Failure to move restart files to init/.\n';
	 chdir "../../..";
      }
# Calculate ensemble statististics
      chdir "EKALMAN";                                                          
      print "   Calculating observables\n";
      !system "mpirun ./state.exe" or die "Failure to run STATE.\n";
      !system "rm -f pm_*" or die 'Failure to rm the pm files /.\n';
      chdir "..";

# Update time for next cycle
      ( $YYYY,$MM,$DD,$HH ) = ( $nYYYY,$nMM,$nDD,$nHH ); 
   }
} # end of assimilation cycle loop

####################################################################################
#-----------Execute ico2ll----------------------------------------------------------
foreach $iens (0..$nens-1) {
   &changedirs($iens);
   if ($exec_SPRINTARS) {
       @lines = &readfile("nhm_driver.cnf");   
       foreach $line (@lines) {
         if ($line =~ m/step\s*=\s*(\d+)/ ) {
            $step= $1;
         }
         if ($line =~ m/DTL\s*=\s*(\d*\.\d+)D(\d+)/ ) {
            my $dtstep = join "e", $1, $2;
	    $tintv=$dtstep*($step);
         }
       }
       @lines = &readfile("history.info");   
       foreach $line (@lines) {
            $line =~ s/\s*\d+\s+($tintv\.\d*)/  $ncycle   $1/;
       }
       &writefile("history.info",@lines);
#       !system "$TOPPATH/ico2ll"
#           or die "Failure to ico2ll.\n";
#       !system "rm pm_*" or
#	    die 'Failure to rm the pm files /.\n';
  }
   chdir "../../..";
} # end of ico2ll

print "ncycle=.$ncycle.\n";
print "Finished";


chdir "EKALMAN/observables/analysis/";
@lines = &readfile("history.info");   
foreach $line (@lines) {
$line =~ s/\s*\d+\s+(\d+\.\d*)/  $ncycle     $1/;
}
&writefile("history.info",@lines);
chdir "../../..";

# Moves to directory for member $iens, verbose
sub changedirv{
   my $iens = $_[0]; # ensemble nr
   my $name = $iens;
   if ($iens < 100) { $name = "0".$name };
   if ($iens < 10)  { $name = "0".$name };
   print "Changing to directory for member ".$name."\n"; 
   chdir "SPRINTARS/members/$name/init";
} 

#
# Moves to directory for member $iens, silent
sub changedirs{
   my $iens = $_[0]; # ensemble nr
   my $name = $iens;
   if ($iens < 100) { $name = "0".$name };
   if ($iens < 10)  { $name = "0".$name };
   print "Changing to directory for member ".$name."\n"; 
   chdir "SPRINTARS/members/$name";
} 

#
# Compares current time to end date
sub notfinished{
   my( $YYYY,$MM,$DD,$HH );
   ( $YYYY,$MM,$DD,$HH ) = @_;
   $finished = 1;  # NOT finished
   if ($YYYY > $eYYYY) {
      $finished = 0;
   } elsif ($YYYY == $eYYYY) {
      if ($MM > $eMM) {
    $finished = 0;
      } elsif ($MM == $eMM) {
         if ($DD > $eDD) {
       $finished = 0;
    } elsif ($DD == $eDD) {
       if ($HH >= $eHH) {
          $finished = 0;
       }
    }
      }
   }
   $finished;
}


#
# Calculates new date after cyclestep
sub increasetime {
   my( $YYYY,$MM,$DD,$HH );
   ( $YYYY,$MM,$DD,$HH ) = @_;
   $HH = $HH + 1;
   if ($HH >= 24) {
      $HH = $HH -24;
      $DD = $DD + 1;
      if ($DD > &dayspermonth($YYYY,$MM,$DD,$HH)) {
         $DD = $DD - &dayspermonth($YYYY,$MM,$DD,$HH);
         $MM = $MM + 1;
         if ($MM >= 13) {
             $MM   = 1;
             $YYYY = $YYYY + 1;
         }
      }
   }
   $MM =~ s/^([0-9])$/0$1/;
   $DD =~ s/^([0-9])$/0$1/;
   $HH =~ s/^([0-9])$/0$1/;
   ( $YYYY,$MM,$DD,$HH );
}


#
# Calculates the days in a given month/year
sub dayspermonth {
   my( $YYYY,$MM,$DD,$HH );
   ( $YYYY,$MM,$DD,$HH ) = @_;
   @dpm = (31,28,31,30,31,30,31,31,30,31,30,31); # days per month

#...I use simple leap year logic that is good enough for 1901 - 2099...........
   if (($YYYY % 4 == 0) && ($MM==2)) {
      $days = 29;
   } else {
      $days = $dpm[$MM-1];
   }
}


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
