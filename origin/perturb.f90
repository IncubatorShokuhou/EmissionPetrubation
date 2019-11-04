!===============================================================================
!
! PURPOSE:      Creates a NICAM files with perturbed parameter values 
!
! HISTORY:      created on Oct 26, 2011 by Tie Dai
!               Imported from prg_ico2ll in NICAM/tool
!               Version-2.0 ensemble factors created  by Tie Dai 2013/01/21
!===============================================================================

PROGRAM perturb
   USE mt19937
   IMPLICIT NONE
   
   INTEGER                    :: rlevelon=0
   INTEGER                    :: glevelon=5
   INTEGER                    :: l,lat,nm,it
   INTEGER                    :: lall,gall
   CHARACTER(LEN=4)           :: member='xxx/'           ! template name
   INTEGER,PARAMETER          :: out=30
   CHARACTER(LEN=128)         :: dir='init/'
   CHARACTER(LEN=128)         :: file_base,distribution,fname
   CHARACTER(LEN=80)          :: dummy
   CHARACTER(LEN=16)          :: item
   REAL(8) ,allocatable       :: perturbation(:,:,:)
   REAL(8) ,allocatable       :: pfactors(:,:,:)
   INTEGER                    :: seed,nlon,nlat,Nmem
   INTEGER                    :: iargc
   INTEGER                    :: ierr
   REAL(8)                    :: mean,stddev
   REAL(8)                    :: mean_c,stddev_c ! current
   REAL(8)                    :: mean_o,stddev_o ! original 
   LOGICAL                    :: LogNormalon=.FALSE.!,Shift=.FALSE. 
!  INTEGER                    :: ilon,ilat
!  Added by Tie Dai 2015/7/13 supported nlag, nslot
   INTEGER                    :: recnum
   INTEGER                    :: nlag,nslot
   INTEGER                    :: nlg,ns
   INTEGER                    :: niteration
   namelist /ADMPARAM/      & 
             rlevel,        &
             glevel

! ...Read command line input.....................................................
   IF (iargc().EQ.0) THEN
      WRITE(*,*) '--- SYNTAX ---------------------------------------------'
      WRITE(*,*) 'perturb <ITEM> <SEED> <MEAN> <STDDEV> <DISTRIB> <NREG> <NIJ> <Nmembers> <FILE> '
      WRITE(*,*) '   ITEM   : name of variable'
      WRITE(*,*) '   SEED   : seed for the random number generator'
      WRITE(*,*) '   MEAN   : mean of the random numbers'
      WRITE(*,*) '   STDDEV : standard deviation of the random numbers'
      WRITE(*,*) '   DISTRIB: distribution (normal or lognormal)'
      WRITE(*,*) '   NREG   : region size'
      WRITE(*,*) '   NIJ    : region grid size'
      WRITE(*,*) '   FILE   : name of parameter file'
      WRITE(*,*) '   nslot  : nslot'
      WRITE(*,*) '   nlag   : nlag'
      WRITE(*,*) '--- SYNTAX ---------------------------------------------'
      STOP
   ENDIF
   IF (iargc().EQ.11) THEN
      CALL getarg(1,item)
      CALL getarg(2,dummy)
      READ(dummy,*) seed
      CALL getarg(3,dummy)
      READ(dummy,*) mean
      CALL getarg(4,dummy)
      READ(dummy,*) stddev
      CALL getarg(5,distribution)
      CALL getarg(6,dummy)
      READ(dummy,*) nlon
      CALL getarg(7,dummy)
      READ(dummy,*) nlat
      CALL getarg(8,dummy)
      READ(dummy,*) Nmem
      CALL getarg(9,file_base)
      CALL getarg(10,dummy)
      READ(dummy,*) nslot
      CALL getarg(11,dummy)
      READ(dummy,*) nlag
   ELSE
      WRITE(*,*) '!!! ERROR [ main ]: too few or too many input variables!'
      WRITE(*,*) '--- SYNTAX ----------------------------------------------'
      WRITE(*,*) 'perturb <FILE> <ITEM> <SEED> <MEAN> <STDDEV> <DISTRIB> <NREG>'
      WRITE(*,*) '   ITEM   : name of variable'
      WRITE(*,*) '   SEED   : seed for the random number generator'
      WRITE(*,*) '   MEAN   : mean of the random numbers'
      WRITE(*,*) '   STDDEV : standard deviation of the random numbers'
      WRITE(*,*) '   DISTRIB: distribution (normal or lognormal)'
      WRITE(*,*) '   NREG   : region size'
      WRITE(*,*) '   NIJ    : region grid size'
      WRITE(*,*) '   Nmem   : ensemble members size'
      WRITE(*,*) '   FILE   : name of parameter file'
      WRITE(*,*) '   nslot  : nslot'
      WRITE(*,*) '   nlag   : nlag'
      WRITE(*,*) '--- SYNTAX ----------------------------------------------'
      STOP
   ENDIF
   niteration=0
   IF (distribution.EQ.'lognormal')   LogNormal = .TRUE.
!   IF (nlon.LT.0) Shift     = .TRUE.
   nlon = ABS(nlon)

! ...Check if correct input values...............................................
   IF (mean.LE.0) THEN
      WRITE(*,*) 'perturb.f90'
      WRITE(*,*) '   ERROR: MEAN should be > 0'
      STOP 'Aborted'
   ENDIF
! .. Changed by Tie Dai for no perturb...............................................
   IF (stddev.LT.0) THEN
      WRITE(*,*) 'perturb.f90'
      WRITE(*,*) '   ERROR: STDDEV should be > 0'
      STOP 'Aborted'
   ENDIF
   IF ((distribution.ne.'normal').AND.(distribution.ne.'lognormal')) THEN
      WRITE(*,*) 'perturb.f90'
      WRITE(*,*) '   ERROR: DISTRIB should be normal or lognormal'
      STOP 'Aborted'
   ENDIF
!   IF ((nlon.ne.1 ).AND.(nlon.ne.2 ).AND.(nlon.ne.4 ).AND.(nlon.ne.8 ).AND.&
!       (nlon.ne.16).AND.(nlon.ne.32).AND.(nlon.ne.64)) THEN
!      WRITE(*,*) 'perturb.f90'
!      WRITE(*,*) '   ERROR: ABS(NREG) should be 1, 2, 4, 8, 16, 32 or 64'
!      STOP 'Aborted'
!   ENDIF
   open (10, file='mkperturb.cnf',form='formatted',status='old',iostat=ierr)
   if (ierr/=0) then 
       write(*,*) 'Cannot open PARAMETER file !' 
       stop
   end if
   rewind(10)
   read (10,nmlon=ADMPARAM)
   close(10)
   open (20, file='perturb.log',access='sequential',position='append',form='formatted',status='unknown')

   lallon=(2**rlevel)**2*10
   gallon=(2**(glevel-rlevel)+2)**2
   allocate( pfactors(NIJ,NREG,Nmem))
   allocate( perturbation(gall,lall,Nmem))
! ...Check if file is already present............................................
!   INQUIRE(FILE=dir//TRIM(file), EXIST=FileExists)
!   IF (FileExists) STOP

   mean_o=mean
   stddev_o=stddev
   mean_c=0.
   stddev_c=0.
! ...Scale noise to correct mean and standard deviation..........................
   IF (LogNormal) THEN
      stddev = (stddev/mean)**2 + 1.d0
      mean   = LOG(mean/SQRT(stddev))
      stddev = SQRT(LOG(stddev))
   ENDIF   
   if (stddev_o==.0D0.or.Nmem.lt.10) then
! ...Generate uncorrelated Gaussian noise........................................
     do lat=1,nlat
       do lon=1,nlon
          CALL init_genrand(seed)
          seed=seed+1
! the next line may require the -pvctl noassume compile option
          pfactors (lat,lon,:) =  gengauss(  Nmem ) 
       end do
     end do

! ...Create spatially correlated noise...........................................
     DO lat=1,gall,nlat
        DO lon=1,lall,nlon
          perturbation(lat:lat+nlat-1,lon:lon+nlon-1,:) = pfactors(:,:,:)
        ENDDO     
     ENDDO
!   IF (Shift.AND.(SUM(gengauss(1)).LT.0)) THEN
!      tmp = perturbation
!      DO ilon=1,nLon
!         DO ilat=1,nLat
!            perturbation(MOD(ilon-1+nlon/2,nLon)+1,MOD(ilat-1+nlon/2,nLat)+1) = tmp(ilon,ilat)
!         ENDDO
!      ENDDO
!   ENDIF


     pfactors = mean + stddev * pfactors
     perturbation = mean + stddev * perturbation
   
! ...Transform Gaussian noise to log-normal noise................................
     IF (LogNormal) THEN
       pfactors = EXP(pfactors)
       perturbation = EXP(perturbation)
     ENDIF
     mean_c=SUM(pfactors)/REAL(NIJ*NREG*Nmem)
!     stddev_c=SQRT(SUM(pfactors**2)/REAL(NIJ*NREG*Nmem)-(SUM(pfactors)/REAL(NIJ*NREG*Nmem))**2)
     stddev_c=SQRT(SUM((pfactors-mean_c)**2)/(REAL(NIJ*NREG*Nmem)-1))
   else 
     do while((abs(stddev_c/stddev_o-1.)>=0.1).or.(abs(mean_c/mean_o-1.)>=0.05).or.maxval(perturbation)>=15.)
! ...Generate uncorrelated Gaussian noise........................................
       do lat=1,nlat
         do lon=1,nlon
            CALL init_genrand(seed)
            seed=seed+1
! the next line may require the -pvctl noassume compile option
            pfactors (lat,lon,:) =  gengauss(  Nmem ) 
         end do
       end do

! ...Create spatially correlated noise...........................................
       DO lat=1,gall,nlat
          DO lon=1,lall,nlon
             perturbation(lat:lat+nlat-1,l:l+nlon-1,:) = pfactors(:,:,:)
          ENDDO     
       ENDDO
!   IF (Shift.AND.(SUM(gengauss(1)).LT.0)) THEN
!      tmp = perturbation
!      DO ilon=1,nLon
!         DO ilat=1,nLat
!            perturbation(MOD(ilon-1+nlon/2,nLon)+1,MOD(ilat-1+nlon/2,nLat)+1) = tmp(ilon,ilat)
!         ENDDO
!      ENDDO
!   ENDIF


       pfactors = mean + stddev * pfactors
       perturbation = mean + stddev * perturbation
   
! ...Transform Gaussian noise to log-normal noise................................
       IF (LogNormal) THEN
          pfactors = EXP(pfactors)
          perturbation = EXP(perturbation)
       ENDIF
       mean_c=SUM(pfactors)/REAL(NIJ*NREG*Nmem)
!     stddev_c=SQRT(SUM(pfactors**2)/REAL(NIJ*NREG*Nmem)-(SUM(pfactors)/REAL(NIJ*NREG*Nmem))**2)
       stddev_c=SQRT(SUM((pfactors-mean_c)**2)/(REAL(NIJ*NREG*Nmem)-1))
       niteration=niteration+1
!       if (niteration.gt.20) exit
     end do
   end if
   write(20,*) trim(file_base)
   write(20,*) "pfactors="
   write(20,*)  pfactors
   write(20,*) "Mean=", mean_c,mean_o,mean_c/mean_o-1.
   write(20,*) "STD=",  stddev_c,stddev_o,abs(stddev_c/stddev_o-1.)
! ...Ensure noise is positive or zero............................................
!   WHERE (perturbation.LT.0) perturbation = 0.d0   
     
! ...Write (overwrite if present) file...........................................
   do nm=1,Nmem
      WRITE(member(1:3),'(I3.3)') nm-1
      do lon=1,lall
         recnum=0
         call make_idstr(fname,trim(member//trim(dir)//'/'//trim(file_base)),'rgn',l)
         OPEN(Unit=out, FILE=TRIM(fname),ACCESS='direct', &
                  FORM='UNFORMATTED', reclon=gall*8,STATUS='REPLACE',ACTION='WRITE')
         do nlg = 1, nlag+1
            do ns = 1, nslot
               recnum=recnum+1
               WRITE(out,rec=recnum) perturbation(:,lon,nm)
            end do 
         end do
         CLOSE(out)
      end do
   end do
! ...Check statistics............................................................
   WRITE(20,*) "Mean=",SUM(perturbation)/REAL(gall*lall*Nmem)
   WRITE(20,*) "stddev",SQRT(SUM(perturbation**2)/REAL(gall*lall*Nmem)-(SUM(perturbation)/REAL(gall*lall*Nmem))**2)
   deallocate(pfactors)
   deallocate(perturbation)
   close (20)
   stop

END PROGRAM perturb
  !-----------------------------------------------------------------------------
  subroutine make_idstr( &
       str,                   & !--- OUT : file name ! 05/11/15 M.Satoh
       headstr,               & !--- IN  : header name
       ext,                   & !--- IN  : extention string
       rank )                   !--- IN  : ID number
    !
    implicit none
    !
    character(*), intent(out) :: str     !--- strings ! 05/11/15 M.Satoh
    character(*), intent(in) :: headstr  !--- header strings
    character(*), intent(in) :: ext      !--- extention( eg. ***.dat ) 
    INTEGER, intent(in) :: rank          !--- number(+1)
    !
    character(128) :: cnum
    character(5) cnum1
    !
    write(cnum,'(I128.128)') rank-1
    cnum1(1:5) = cnum(124:128)
    str=headstr//'.'//trim(ext)//cnum1
    !
  end subroutine make_idstr
