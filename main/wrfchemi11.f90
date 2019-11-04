PROGRAM wrfchemi
USE mt19937
IMPLICIT NONE
include 'netcdf.inc'
! -------------------------------------------------------
!    Using below command line to get the head information of the NetCDF file:
!    Please execute " ncdump -h wrfchemi_d01_2016-11-01_00:00:00.nc > output.txt " 
! -------------------------------------------------------
!     Define Variables.
!     Variable ids run sequentially from 1 to nvars=40 ! number of variables
     INTEGER*4  ncid,status,io   ! file control
! -------------------------------------------------------------
     INTEGER(4),PARAMETER :: Time=1
     INTEGER(4),PARAMETER :: emissions_zdim_stag=1
     INTEGER(4),PARAMETER :: south_north=99
     INTEGER(4),PARAMETER :: west_east=159
     INTEGER(4),PARAMETER :: DateStrLen=19
     INTEGER(4),PARAMETER :: Nmembers=30 ! the number of the members;also used in creating DIRs
     REAL(4),PARAMETER    :: pi=3.14159
     LOGICAL              :: LogNormal=.TRUE. ! determine whether lognormal or normal
     REAL(8)              :: pfactors(Nmembers)
!     INTEGER(4),PARAMETER :: Nmonths=2 ! the number of numbers(aka the number of different wrfchemi files)
! ----    below 40 variables is the data in netCDF file
     CHARACTER*1    ::  Times( DateStrLen,Time )
     REAL*4         ::  XLONG( west_east,south_north )
     REAL*4         ::  XLAT( west_east,south_north )
     REAL*4       E_CO( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ECI( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ECJ( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ECC( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO2( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO2( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORGI( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORGJ( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORGC( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO4I( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO4J( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO4C( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO3I( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO3J( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO3C( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_PM25I( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_PM25J( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_PM10( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NH3( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ALD( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_OL2( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HCHO( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ISO( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ETH( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_XYL( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_TOL( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_OLI( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_CH3OH( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_C2H5OH( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HC3( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HC5( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HC8( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_OLT( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_CSL( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_KET( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORA2( west_east,south_north,emissions_zdim_stag,Time )
! ----    above40 variables is the data in netCDF file
! -------------------------------------------------------------
     REAL*4       E_CO_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ECI_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ECJ_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ECC_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO2_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO2_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORGI_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORGJ_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORGC_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO4I_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO4J_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO4C_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO3I_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO3J_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO3C_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_PM25I_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_PM25J_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_PM10_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NH3_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ALD_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_OL2_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HCHO_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ISO_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ETH_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_XYL_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_TOL_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_OLI_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_CH3OH_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_C2H5OH_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HC3_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HC5_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HC8_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_OLT_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_CSL_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_KET_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORA2_mean( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_CO_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ECI_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ECJ_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ECC_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO2_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO2_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORGI_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORGJ_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORGC_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO4I_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO4J_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_SO4C_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO3I_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO3J_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NO3C_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_PM25I_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_PM25J_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_PM10_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_NH3_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ALD_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_OL2_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HCHO_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ISO_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ETH_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_XYL_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_TOL_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_OLI_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_CH3OH_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_C2H5OH_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HC3_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HC5_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_HC8_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_OLT_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_CSL_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_KET_stddev( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4       E_ORA2_stddev( west_east,south_north,emissions_zdim_stag,Time )
! -------------------------------------------------------------
     REAL*4       point_around_corner(3)
     REAL*4       point_around_edge(5)
     REAL*4       point_around_mid(8)
! -------------------------------------------------------------
     INTEGER*4   :: start(10)
     INTEGER*4   :: count(10)
     INTEGER*4   :: dimids(10)! allow up to 10 dimensions
     INTEGER*4   :: ndim,xtype
     INTEGER*4   :: natts,len
     CHARACTER(len=31) :: dummy
     INTEGER*4   :: i,j,k,NNmembersNN,NNmonthsNN ! member cycle
     character(len=1) ::NNmonthsNN1,NNmonthsNN2
     INTEGER     :: seed  ! for gengrating ranDOm values for Gaussian noise.
     CHARACTER(42):: filename
     character (len=100) :: line
     real :: xmonth(100)
     REAL*4      :: mean,stddev
! ----------------------------------------------------------------
!  Generate uncorrelated Gaussian noise
! ----------------------------------------------------------------
! DO i=1,south_north
!   DO j=1,west_east
      CALL init_genrand(seed)
      seed=seed+1
      pfactors =  gengauss(Nmembers) ! one dimension:Nmembers. No lat or lon (yet).
      PRINT*,pfactors  ! Temporary statement for checking if pfactors successfully generated
!   END DO
! END DO
! ----------------------------------------------------------------
! Start to cycle according to the filename
! ----------------------------------------------------------------
  DO NNmonthsNN=11,12,1 ! determine the start and end month here
    NNmembersNN=0
    call date_hour_string(NNmonthsNN,NNmonthsNN1,NNmonthsNN2) ! turn the integer to strings.IMPORTANT!!!
    CALL system ('dir ../*/wrfchemi_d01_2016-'//TRIM(NNmonthsNN1)//''//TRIM(NNmonthsNN2)//'-01_00:00:00.nc  > '//TRIM(NNmonthsNN1)//''//TRIM(NNmonthsNN2)//'.txt' )
    OPEN(500,file=''//TRIM(NNmonthsNN1)//''//TRIM(NNmonthsNN2)//'.txt')
    DO NNmembersNN=1,Nmembers
! READ the name of file
        READ(500,'(a)') filename
        PRINT*,filename
! ----------------------------------------------------------------
!  Define "scale_factor" and "add_offset" variables
! ----------------------------------------------------------------
! Open netCDF file.
      status=nf_OPEN(filename,nf_write,ncid)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)

! ----------------------------------------------------
!   Retrieve data for Variable 'Times'
      status=nf_inq_var(ncid,  1,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_text       (ncid,  1,start,count,Times)

! ----------------------------------------------------
!   Retrieve data for Variable 'XLONG'
!   Units of 'XLONG' is 'degree east'
      status=nf_inq_var(ncid,  2,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid,  2,start,count,XLONG)

! ----------------------------------------------------
!   Retrieve data for Variable 'XLAT'
!   Units of 'XLAT' is 'degree north'
      status=nf_inq_var(ncid,  3,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid,  3,start,count,XLAT)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_CO'
!   Units of 'E_CO' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  4,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid,  4,start,count,E_CO)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ECI'
!   Units of 'E_ECI' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  5,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid,  5,start,count,E_ECI)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ECJ'
!   Units of 'E_ECJ' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  6,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid,  6,start,count,E_ECJ)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ECC'
!   Units of 'E_ECC' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  7,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid,  7,start,count,E_ECC)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_NO'
!   Units of 'E_NO' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  8,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid,  8,start,count,E_NO)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_NO2'
!   Units of 'E_NO2' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  9,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid,  9,start,count,E_NO2)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_SO2'
!   Units of 'E_SO2' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 10,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 10,start,count,E_SO2)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ORGI'
!   Units of 'E_ORGI' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 11,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 11,start,count,E_ORGI)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ORGJ'
!   Units of 'E_ORGJ' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 12,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 12,start,count,E_ORGJ)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ORGC'
!   Units of 'E_ORGC' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 13,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 13,start,count,E_ORGC)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_SO4I'
!   Units of 'E_SO4I' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 14,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 14,start,count,E_SO4I)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_SO4J'
!   Units of 'E_SO4J' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 15,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 15,start,count,E_SO4J)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_SO4C'
!   Units of 'E_SO4C' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 16,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 16,start,count,E_SO4C)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_NO3I'
!   Units of 'E_NO3I' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 17,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 17,start,count,E_NO3I)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_NO3J'
!   Units of 'E_NO3J' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 18,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 18,start,count,E_NO3J)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_NO3C'
!   Units of 'E_NO3C' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 19,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 19,start,count,E_NO3C)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_PM25I'
!   Units of 'E_PM25I' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 20,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 20,start,count,E_PM25I)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_PM25J'
!   Units of 'E_PM25J' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 21,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 21,start,count,E_PM25J)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_PM10'
!   Units of 'E_PM10' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid, 22,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 22,start,count,E_PM10)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_NH3'
!   Units of 'E_NH3' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 23,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 23,start,count,E_NH3)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ALD'
!   Units of 'E_ALD' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 24,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 24,start,count,E_ALD)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_OL2'
!   Units of 'E_OL2' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 25,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 25,start,count,E_OL2)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_HCHO'
!   Units of 'E_HCHO' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 26,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 26,start,count,E_HCHO)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ISO'
!   Units of 'E_ISO' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 27,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 27,start,count,E_ISO)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ETH'
!   Units of 'E_ETH' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 28,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 28,start,count,E_ETH)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_XYL'
!   Units of 'E_XYL' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 29,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 29,start,count,E_XYL)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_TOL'
!   Units of 'E_TOL' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 30,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 30,start,count,E_TOL)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_OLI'
!   Units of 'E_OLI' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 31,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 31,start,count,E_OLI)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_CH3OH'
!   Units of 'E_CH3OH' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 32,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 32,start,count,E_CH3OH)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_C2H5OH'
!   Units of 'E_C2H5OH' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 33,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 33,start,count,E_C2H5OH)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_HC3'
!   Units of 'E_HC3' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 34,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 34,start,count,E_HC3)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_HC5'
!   Units of 'E_HC5' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 35,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 35,start,count,E_HC5)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_HC8'
!   Units of 'E_HC8' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 36,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 36,start,count,E_HC8)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_OLT'
!   Units of 'E_OLT' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 37,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 37,start,count,E_OLT)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_CSL'
!   Units of 'E_CSL' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 38,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 38,start,count,E_CSL)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_KET'
!   Units of 'E_KET' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 39,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 39,start,count,E_KET)

! ----------------------------------------------------
!   Retrieve data for Variable 'E_ORA2'
!   Units of 'E_ORA2' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid, 40,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      DO j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      END DO
      status=nf_get_vara_REAL(ncid, 40,start,count,E_ORA2)


! ----------------------------------------------------
!  Begin writing statements to use the data.
! ----------------------------------------------------

! ----------------------------------------------------
!  put data in nc file into Variable mean
! ----------------------------------------------------
      E_CO_mean    =  E_CO
      E_ECI_mean   =  E_ECI
      E_ECJ_mean   =  E_ECJ
      E_ECC_mean   =  E_ECC
      E_NO_mean    =  E_NO
      E_NO2_mean   =  E_NO2
      E_SO2_mean   =  E_SO2
      E_ORGI_mean  =  E_ORGI
      E_ORGJ_mean  =  E_ORGJ
      E_ORGC_mean  =  E_ORGC
      E_SO4I_mean  =  E_SO4I
      E_SO4J_mean  =  E_SO4J
      E_SO4C_mean  =  E_SO4C
      E_NO3I_mean  =  E_NO3I
      E_NO3J_mean  =  E_NO3J
      E_NO3C_mean  =  E_NO3C
      E_PM25I_mean =  E_PM25I
      E_PM25J_mean =  E_PM25J
      E_PM10_mean  =  E_PM10
      E_NH3_mean   =  E_NH3
      E_ALD_mean   =  E_ALD
      E_OL2_mean   =  E_OL2
      E_HCHO_mean  =  E_HCHO
      E_ISO_mean   =  E_ISO
      E_ETH_mean   =  E_ETH
      E_XYL_mean   =  E_XYL
      E_TOL_mean   =  E_TOL
      E_OLI_mean   =  E_OLI
      E_CH3OH_mean =  E_CH3OH
      E_C2H5OH_mean=  E_C2H5OH
      E_HC3_mean   =  E_HC3
      E_HC5_mean   =  E_HC5
      E_HC8_mean   =  E_HC8
      E_OLT_mean   =  E_OLT
      E_CSL_mean   =  E_CSL
      E_KET_mean   =  E_KET
      E_ORA2_mean  =  E_ORA2
! ----------------------------------------------------
!  calculate Variable stddev
! ----------------------------------------------------
      CALL calculate_stddev(E_CO,E_CO_stddev)
      CALL calculate_stddev(E_ECI,E_ECI_stddev)
      CALL calculate_stddev(E_ECJ,E_ECJ_stddev)
      CALL calculate_stddev(E_ECC,E_ECC_stddev)
      CALL calculate_stddev(E_NO,E_NO_stddev)
      CALL calculate_stddev(E_NO2,E_NO2_stddev)
      CALL calculate_stddev(E_SO2,E_SO2_stddev)
      CALL calculate_stddev(E_ORGI,E_ORGI_stddev)
      CALL calculate_stddev(E_ORGJ,E_ORGJ_stddev)
      CALL calculate_stddev(E_ORGC,E_ORGC_stddev)
      CALL calculate_stddev(E_SO4I,E_SO4I_stddev)
      CALL calculate_stddev(E_SO4J,E_SO4J_stddev)
      CALL calculate_stddev(E_SO4C,E_SO4C_stddev)
      CALL calculate_stddev(E_NO3I,E_NO3I_stddev)
      CALL calculate_stddev(E_NO3J,E_NO3J_stddev)
      CALL calculate_stddev(E_NO3C,E_NO3C_stddev)
      CALL calculate_stddev(E_PM25I,E_PM25I_stddev)
      CALL calculate_stddev(E_PM25J,E_PM25J_stddev)
      CALL calculate_stddev(E_PM10,E_PM10_stddev)
      CALL calculate_stddev(E_NH3,E_NH3_stddev)
      CALL calculate_stddev(E_ALD,E_ALD_stddev)
      CALL calculate_stddev(E_OL2,E_OL2_stddev)
      CALL calculate_stddev(E_HCHO,E_HCHO_stddev)
      CALL calculate_stddev(E_ISO,E_ISO_stddev)
      CALL calculate_stddev(E_ETH,E_ETH_stddev)
      CALL calculate_stddev(E_XYL,E_XYL_stddev)
      CALL calculate_stddev(E_TOL,E_TOL_stddev)
      CALL calculate_stddev(E_OLI,E_OLI_stddev)
      CALL calculate_stddev(E_CH3OH,E_CH3OH_stddev)
      CALL calculate_stddev(E_C2H5OH,E_C2H5OH_stddev)
      CALL calculate_stddev(E_HC3,E_HC3_stddev)
      CALL calculate_stddev(E_HC5,E_HC5_stddev)
      CALL calculate_stddev(E_HC8,E_HC8_stddev)
      CALL calculate_stddev(E_OLT,E_OLT_stddev)
      CALL calculate_stddev(E_CSL,E_CSL_stddev)
      CALL calculate_stddev(E_KET,E_KET_stddev)
      CALL calculate_stddev(E_ORA2,E_ORA2_stddev)
! ----------------------------------------------------
! calculate
! ----------------------------------------------------
    DO i=1,west_east
      DO j=1,south_north
        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_CO_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_CO(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_CO_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_CO_stddev(i,j,1,1)
          E_CO(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_CO_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ORGC_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ORGC(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ORGC_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ORGC_stddev(i,j,1,1)
          E_ORGC(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ORGC_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_SO4I_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_SO4I(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_SO4I_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_SO4I_stddev(i,j,1,1)
          E_SO4I(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_SO4I_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_SO4J_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_SO4J(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_SO4J_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_SO4J_stddev(i,j,1,1)
          E_SO4J(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_SO4J_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_SO4C_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_SO4C(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_SO4C_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_SO4C_stddev(i,j,1,1)
          E_SO4C(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_SO4C_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_NO3I_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_NO3I(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_NO3I_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_NO3I_stddev(i,j,1,1)
          E_NO3I(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_NO3I_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_NO3J_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_NO3J(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_NO3J_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_NO3J_stddev(i,j,1,1)
          E_NO3J(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_NO3J_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_NO3C_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_NO3C(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_NO3C_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_NO3C_stddev(i,j,1,1)
          E_NO3C(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_NO3C_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_PM25I_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_PM25I(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_PM25I_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_PM25I_stddev(i,j,1,1)
          E_PM25I(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_PM25I_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_PM25J_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_PM25J(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_PM25J_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_PM25J_stddev(i,j,1,1)
          E_PM25J(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_PM25J_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_PM10_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_PM10(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_PM10_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_PM10_stddev(i,j,1,1)
          E_PM10(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_PM10_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ECI_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ECI(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ECI_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ECI_stddev(i,j,1,1)
          E_ECI(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ECI_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_NH3_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_NH3(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_NH3_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_NH3_stddev(i,j,1,1)
          E_NH3(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_NH3_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ALD_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ALD(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ALD_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ALD_stddev(i,j,1,1)
          E_ALD(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ALD_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_OL2_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_OL2(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_OL2_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_OL2_stddev(i,j,1,1)
          E_OL2(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_OL2_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_HCHO_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_HCHO(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_HCHO_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_HCHO_stddev(i,j,1,1)
          E_HCHO(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_HCHO_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ISO_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ISO(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ISO_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ISO_stddev(i,j,1,1)
          E_ISO(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ISO_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ETH_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ETH(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ETH_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ETH_stddev(i,j,1,1)
          E_ETH(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ETH_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_XYL_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_XYL(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_XYL_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_XYL_stddev(i,j,1,1)
          E_XYL(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_XYL_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_TOL_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_TOL(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_TOL_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_TOL_stddev(i,j,1,1)
          E_TOL(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_TOL_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_OLI_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_OLI(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_OLI_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_OLI_stddev(i,j,1,1)
          E_OLI(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_OLI_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_CH3OH_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_CH3OH(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_CH3OH_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_CH3OH_stddev(i,j,1,1)
          E_CH3OH(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_CH3OH_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ECJ_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ECJ(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ECJ_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ECJ_stddev(i,j,1,1)
          E_ECJ(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ECJ_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_C2H5OH_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_C2H5OH(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_C2H5OH_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_C2H5OH_stddev(i,j,1,1)
          E_C2H5OH(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_C2H5OH_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_HC3_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_HC3(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_HC3_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_HC3_stddev(i,j,1,1)
          E_HC3(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_HC3_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_HC5_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_HC5(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_HC5_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_HC5_stddev(i,j,1,1)
          E_HC5(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_HC5_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_HC8_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_HC8(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_HC8_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_HC8_stddev(i,j,1,1)
          E_HC8(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_HC8_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_OLT_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_OLT(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_OLT_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_OLT_stddev(i,j,1,1)
          E_OLT(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_OLT_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_CSL_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_CSL(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_CSL_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_CSL_stddev(i,j,1,1)
          E_CSL(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_CSL_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_KET_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_KET(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_KET_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_KET_stddev(i,j,1,1)
          E_KET(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_KET_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ORA2_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ORA2(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ORA2_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ORA2_stddev(i,j,1,1)
          E_ORA2(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ORA2_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ECC_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ECC(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ECC_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ECC_stddev(i,j,1,1)
          E_ECC(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ECC_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_NO_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_NO(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_NO_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_NO_stddev(i,j,1,1)
          E_NO(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_NO_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_NO2_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_NO2(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_NO2_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_NO2_stddev(i,j,1,1)
          E_NO2(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_NO2_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_SO2_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_SO2(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_SO2_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_SO2_stddev(i,j,1,1)
          E_SO2(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_SO2_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ORGI_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ORGI(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ORGI_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ORGI_stddev(i,j,1,1)
          E_ORGI(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ORGI_mean(i,j,1,1)
        END IF

        IF (LogNormal .EQ. .TRUE.) THEN
          stddev=0.0
          mean=1.0
          stddev = (E_ORGJ_stddev(i,j,1,1)/mean)**2 + 1.0
          mean   = LOG(mean/SQRT(stddev))
          stddev = SQRT(LOG(stddev))
          E_ORGJ(i,j,1,1) = EXP(mean + stddev * pfactors(NNmembersNN))*E_ORGJ_mean(i,j,1,1)
        ELSE
          stddev=0.0
          mean=1.0
          stddev=E_ORGJ_stddev(i,j,1,1)
          E_ORGJ(i,j,1,1) = (mean + stddev * (pfactors(NNmembersNN)))*E_ORGJ_mean(i,j,1,1)
        END IF
      END DO
    END DO
    ! -------------------------------------------------------- 
    ! put value back to ncfile 
    ! -------------------------------------------------------- 
      status=NF_PUT_VAR(ncid,1,Times) 
      status=NF_PUT_VAR(ncid,2,XLONG) 
      status=NF_PUT_VAR(ncid,3,XLAT) 
      status=NF_PUT_VAR(ncid,4,E_CO) 
      status=NF_PUT_VAR(ncid,5,E_ECI) 
      status=NF_PUT_VAR(ncid,6,E_ECJ) 
      status=NF_PUT_VAR(ncid,7,E_ECC) 
      status=NF_PUT_VAR(ncid,8,E_NO) 
      status=NF_PUT_VAR(ncid,9,E_NO2) 
      status=NF_PUT_VAR(ncid,10,E_SO2) 
      status=NF_PUT_VAR(ncid,11,E_ORGI) 
      status=NF_PUT_VAR(ncid,12,E_ORGJ) 
      status=NF_PUT_VAR(ncid,13,E_ORGC) 
      status=NF_PUT_VAR(ncid,14,E_SO4I) 
      status=NF_PUT_VAR(ncid,15,E_SO4J) 
      status=NF_PUT_VAR(ncid,16,E_SO4C) 
      status=NF_PUT_VAR(ncid,17,E_NO3I) 
      status=NF_PUT_VAR(ncid,18,E_NO3J) 
      status=NF_PUT_VAR(ncid,19,E_NO3C) 
      status=NF_PUT_VAR(ncid,20,E_PM25I) 
      status=NF_PUT_VAR(ncid,21,E_PM25J) 
      status=NF_PUT_VAR(ncid,22,E_PM10) 
      status=NF_PUT_VAR(ncid,23,E_NH3) 
      status=NF_PUT_VAR(ncid,24,E_ALD) 
      status=NF_PUT_VAR(ncid,25,E_OL2) 
      status=NF_PUT_VAR(ncid,26,E_HCHO) 
      status=NF_PUT_VAR(ncid,27,E_ISO) 
      status=NF_PUT_VAR(ncid,28,E_ETH) 
      status=NF_PUT_VAR(ncid,29,E_XYL) 
      status=NF_PUT_VAR(ncid,30,E_TOL) 
      status=NF_PUT_VAR(ncid,31,E_OLI) 
      status=NF_PUT_VAR(ncid,32,E_CH3OH) 
      status=NF_PUT_VAR(ncid,33,E_C2H5OH) 
      status=NF_PUT_VAR(ncid,34,E_HC3) 
      status=NF_PUT_VAR(ncid,35,E_HC5) 
      status=NF_PUT_VAR(ncid,36,E_HC8) 
      status=NF_PUT_VAR(ncid,37,E_OLT) 
      status=NF_PUT_VAR(ncid,38,E_CSL) 
      status=NF_PUT_VAR(ncid,39,E_KET) 
      status=NF_PUT_VAR(ncid,40,E_ORA2) 
      status=nf_close(ncid) 
    END DO
  !  CALL system ('rm -f 1.txt' )
  END DO  
! ----------------------------------------------------
  End Program wrfchemi
  
  SUBROUTINE stddev_mid(x,y)
    IMPLICIT NONE
    INTEGER::i,N
    REAL x(8)
    REAL*8::sumX,sumXX,meanX
    REAL :: y
    ! average
    sumX=0
    sumXX=0
    N=size(x)
    DO i=1,N
        sumX=sumX+x(i)
    END DO
    meanX= sumX/real(N,8)
    x(:)=x(:)-meanX 
    ! stddev
    y=min(SQRT(sum(x*x)/(real(N-1,8))),2.0)
  END SUBROUTINE stddev_mid
  
  SUBROUTINE stddev_edge(x,y)
    IMPLICIT NONE
    INTEGER::i,N
    REAL x(5)
    REAL*8::sumX,sumXX,meanX
    REAL :: y
    ! average
    sumX=0
    sumXX=0
    N=size(x)
    DO i=1,N
        sumX=sumX+x(i)
    END DO
    meanX= sumX/real(N,8)
    x(:)=x(:)-meanX 
    ! stddev
    y=min(SQRT(sum(x*x)/(real(N-1,8))),2.0)
  END SUBROUTINE stddev_edge
  
  SUBROUTINE stddev_corner(x,y)
    IMPLICIT NONE
    INTEGER::i,N
    REAL x(3)
    REAL*8::sumX,sumXX,meanX
    REAL :: y
    ! average
    sumX=0
    sumXX=0
    N=size(x)
    DO i=1,N
        sumX=sumX+x(i)
    END DO
    meanX= sumX/real(N,8)
    x(:)=x(:)-meanX 
    ! stddev
    y=min(SQRT(sum(x*x)/(real(N-1,8))),2.0)
  END SUBROUTINE stddev_corner
  
  SUBROUTINE calculate_stddev(varin,varout)
  IMPLICIT NONE
  INTEGER(4),PARAMETER :: Time=1
  INTEGER(4),PARAMETER :: emissions_zdim_stag=1
  INTEGER(4),PARAMETER :: south_north=99
  INTEGER(4),PARAMETER :: west_east=159
  INTEGER(4),PARAMETER :: DateStrLen=19
  INTEGER(4),PARAMETER :: Nmembers=40 ! the number of the members;also used in creating DIRs
  REAL(4),PARAMETER :: pi=3.14159
  INTEGER::i,j
  REAL varin( west_east,south_north,emissions_zdim_stag,Time )
  REAL varout( west_east,south_north,emissions_zdim_stag,Time )
     REAL*4     point_around_corner(3)
     REAL*4     point_around_edge(5)
     REAL*4     point_around_mid(8)
  ! ----------------------------------------------------
  !  mid
  ! ----------------------------------------------------
  DO i=2,west_east-1
	  DO j=2,south_north-1
			point_around_mid(1)=varin(i-1,j-1,1,1)
			point_around_mid(2)=varin(i-1,j,1,1)
			point_around_mid(3)=varin(i-1,j+1,1,1)
			point_around_mid(4)=varin(i+1,j-1,1,1)
			point_around_mid(5)=varin(i+1,j,1,1)
			point_around_mid(6)=varin(i+1,j+1,1,1)
			point_around_mid(7)=varin(i,j-1,1,1)
			point_around_mid(8)=varin(i,j+1,1,1)
			CALL stddev_mid(point_around_mid,varout(i,j,1,1))
	  END DO
  END DO
! ----------------------------------------------------
!  edge*4
! ----------------------------------------------------
DO i=1,1
	DO j=2,south_north-1
		  point_around_edge(1)=varin(1,j-1,1,1)
			point_around_edge(2)=varin(1,j+1,1,1)
			point_around_edge(3)=varin(2,j+1,1,1)
			point_around_edge(4)=varin(2,j-1,1,1)
			point_around_edge(5)=varin(2,j,1,1)
			CALL stddev_edge(point_around_edge,varout(i,j,1,1))
  END DO
END DO
DO i=west_east,west_east
	DO j=2,south_north-1
		  point_around_edge(1)=varin(west_east,j-1,1,1)
			point_around_edge(2)=varin(west_east,j+1,1,1)
			point_around_edge(3)=varin(west_east-1,j+1,1,1)
			point_around_edge(4)=varin(west_east-1,j-1,1,1)
			point_around_edge(5)=varin(west_east-1,j,1,1)
			CALL stddev_edge(point_around_edge,varout(i,j,1,1))
  END DO		
END DO	
DO i=2,west_east-1
	DO j=1,1
		  point_around_edge(1)=varin(i+1,1,1,1)
			point_around_edge(2)=varin(i-1,1,1,1)
			point_around_edge(3)=varin(i-1,2,1,1)
			point_around_edge(4)=varin(i,2,1,1)
			point_around_edge(5)=varin(i+1,2,1,1)
			CALL stddev_edge(point_around_edge,varout(i,j,1,1))
  END DO
END DO
DO i=2,west_east-1
	DO j=south_north,south_north
		  point_around_edge(1)=varin(i+1,south_north,1,1)
			point_around_edge(2)=varin(i-1,south_north,1,1)
			point_around_edge(3)=varin(i-1,south_north-1,1,1)
			point_around_edge(4)=varin(i,south_north-1,1,1)
			point_around_edge(5)=varin(i+1,south_north-1,1,1)
			CALL stddev_edge(point_around_edge,varout(i,j,1,1))
  END DO
END DO
! ----------------------------------------------------
!  corner*4
! ----------------------------------------------------
      point_around_corner(1)=varin(1,2,1,1)
			point_around_corner(2)=varin(2,1,1,1)
			point_around_corner(3)=varin(2,2,1,1)
			CALL stddev_corner(point_around_corner,varout(1,1,1,1))
			
			point_around_corner(1)=varin(2,south_north,1,1)
			point_around_corner(2)=varin(1,south_north-1,1,1)
			point_around_corner(3)=varin(2,south_north-1,1,1)
			CALL stddev_corner(point_around_corner,varout(1,south_north,1,1))
			
			point_around_corner(1)=varin(west_east-1,1,1,1)
			point_around_corner(2)=varin(west_east-1,2,1,1)
			point_around_corner(3)=varin(west_east,2,1,1)
			CALL stddev_corner(point_around_corner,varout(west_east,1,1,1))
			
			point_around_corner(1)=varin(west_east-1,south_north,1,1)
			point_around_corner(2)=varin(west_east-1,south_north-1,1,1)
			point_around_corner(3)=varin(west_east,south_north-1,1,1)
			CALL stddev_corner(point_around_corner,varout(west_east,south_north,1,1))

  END SUBROUTINE calculate_stddev
  SUBROUTINE date_hour_string(input,output1,output2)
    IMPLICIT NONE
      INTEGER::input,input10,input1
      character::output1,output2
      IF (input .LT. 10) THEN
        output1="0"
        write(output2,'(i1)')input
      END IF
      IF (input .GT. 9) THEN
      	input1=MOD(input,10)
      	input10=(input-input1)/10
      	write(output1,'(i1)')input10
      	write(output2,'(i1)')input1    	
      END IF
  END SUBROUTINE date_hour_string