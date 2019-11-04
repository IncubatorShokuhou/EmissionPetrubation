!-------------------------------------------------------
!  
!  wrfchemi.f90
!    This file is a fortran template file designed to read the given
!     netCDF file 'wrfchemi_d01_2016-11-01_00:00:00.nc' into memory.
!  
!  History: 
!  Date       Name          Action
!-------------------------------------------------------
!  
!  ?? Oct 1993  B. Schwartz   Created.
!  24 Aug 1994  E. Boer       Modified dimension output.
!  29 Jul 1997  S. Rupert     Standardized and reinstated required include of netcdf.inc. 
!  30 Jul 1997  S. Rupert     Added usage message and command line inputs.
!  03 Apr 2003  H. Yan        Change output to fortran 90 format, modified maximum dimesions to 5.
!  30 Mar 2004  H. Yan        Added usage message, retrieve "scale_factor" and "add_offset" parameters.
!  18 May 2006  H. Yan        Fixed bugs of dimensions definition and remark lines. 
!-------------------------------------------------------
!  Do not forget to include the -I path_to_netcdf_  includes in your compile statement Required includes.
!  Also note: need 'netcdf.lib' or 'netcdfs.lib' when link .
      include 'netcdf.inc'
!
!-------------------------------------------------------
!
!    Using below command line to get the head information of the NetCDF file:
!    Please execute " ncdump -h wrfchemi_d01_2016-11-01_00:00:00.nc > output.txt " 
!-------------------------------------------------------
!
!     Define Variables.
!     Variable ids run sequentially from 1 to nvars=40 ! number of variables
      integer*4  ncid, status    ! file control
!-------------------------------------------------------------
 !     Below 40 variables is the data in netCDF file
      character*1    ::  Times( 19, 1 )
      real*4         ::  XLONG( 159, 99 )
      real*4         ::  XLAT( 159, 99 )
      real*4       E_CO( 159, 99, 1, 1 )
      real*4       E_ECI( 159, 99, 1, 1 )
      real*4       E_ECJ( 159, 99, 1, 1 )
      real*4       E_ECC( 159, 99, 1, 1 )
      real*4       E_NO( 159, 99, 1, 1 )
      real*4       E_NO2( 159, 99, 1, 1 )
      real*4       E_SO2( 159, 99, 1, 1 )
      real*4       E_ORGI( 159, 99, 1, 1 )
      real*4       E_ORGJ( 159, 99, 1, 1 )
      real*4       E_ORGC( 159, 99, 1, 1 )
      real*4       E_SO4I( 159, 99, 1, 1 )
      real*4       E_SO4J( 159, 99, 1, 1 )
      real*4       E_SO4C( 159, 99, 1, 1 )
      real*4       E_NO3I( 159, 99, 1, 1 )
      real*4       E_NO3J( 159, 99, 1, 1 )
      real*4       E_NO3C( 159, 99, 1, 1 )
      real*4       E_PM25I( 159, 99, 1, 1 )
      real*4       E_PM25J( 159, 99, 1, 1 )
      real*4       E_PM10( 159, 99, 1, 1 )
      real*4       E_NH3( 159, 99, 1, 1 )
      real*4       E_ALD( 159, 99, 1, 1 )
      real*4       E_OL2( 159, 99, 1, 1 )
      real*4       E_HCHO( 159, 99, 1, 1 )
      real*4       E_ISO( 159, 99, 1, 1 )
      real*4       E_ETH( 159, 99, 1, 1 )
      real*4       E_XYL( 159, 99, 1, 1 )
      real*4       E_TOL( 159, 99, 1, 1 )
      real*4       E_OLI( 159, 99, 1, 1 )
      real*4       E_CH3OH( 159, 99, 1, 1 )
      real*4       E_C2H5OH( 159, 99, 1, 1 )
      real*4       E_HC3( 159, 99, 1, 1 )
      real*4       E_HC5( 159, 99, 1, 1 )
      real*4       E_HC8( 159, 99, 1, 1 )
      real*4       E_OLT( 159, 99, 1, 1 )
      real*4       E_CSL( 159, 99, 1, 1 )
      real*4       E_KET( 159, 99, 1, 1 )
      real*4       E_ORA2( 159, 99, 1, 1 )
!     above40 variables is the data in netCDF file
!-------------------------------------------------------------
     integer*4   :: start(10)
     integer*4   :: count(10)
     integer*4   :: dimids(10)! allow up to 10 dimensions
     integer*4   :: ndim, xtype
     integer*4   :: natts,j,len
     character(len=31) :: dummy
!----------------------------------------------------------------
!  Define "scale_factor" and "add_offset" variables
!----------------------------------------------------------------

! Open netCDF file.
      status=nf_open('wrfchemi_d01_2016-11-01_00:00:00.nc',nf_nowrite,ncid)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)

!----------------------------------------------------
!   Retrieve data for Variable 'Times'
      status=nf_inq_var(ncid,   1,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_text       (ncid,   1,start,count,Times)

!----------------------------------------------------
!   Retrieve data for Variable 'XLONG'
!   Units of 'XLONG' is 'degree east'
      status=nf_inq_var(ncid,   2,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,   2,start,count,XLONG)

!----------------------------------------------------
!   Retrieve data for Variable 'XLAT'
!   Units of 'XLAT' is 'degree north'
      status=nf_inq_var(ncid,   3,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,   3,start,count,XLAT)

!----------------------------------------------------
!   Retrieve data for Variable 'E_CO'
!   Units of 'E_CO' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,   4,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,   4,start,count,E_CO)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ECI'
!   Units of 'E_ECI' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,   5,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,   5,start,count,E_ECI)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ECJ'
!   Units of 'E_ECJ' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,   6,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,   6,start,count,E_ECJ)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ECC'
!   Units of 'E_ECC' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,   7,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,   7,start,count,E_ECC)

!----------------------------------------------------
!   Retrieve data for Variable 'E_NO'
!   Units of 'E_NO' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,   8,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,   8,start,count,E_NO)

!----------------------------------------------------
!   Retrieve data for Variable 'E_NO2'
!   Units of 'E_NO2' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,   9,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,   9,start,count,E_NO2)

!----------------------------------------------------
!   Retrieve data for Variable 'E_SO2'
!   Units of 'E_SO2' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  10,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  10,start,count,E_SO2)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ORGI'
!   Units of 'E_ORGI' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  11,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  11,start,count,E_ORGI)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ORGJ'
!   Units of 'E_ORGJ' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  12,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  12,start,count,E_ORGJ)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ORGC'
!   Units of 'E_ORGC' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  13,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  13,start,count,E_ORGC)

!----------------------------------------------------
!   Retrieve data for Variable 'E_SO4I'
!   Units of 'E_SO4I' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  14,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  14,start,count,E_SO4I)

!----------------------------------------------------
!   Retrieve data for Variable 'E_SO4J'
!   Units of 'E_SO4J' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  15,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  15,start,count,E_SO4J)

!----------------------------------------------------
!   Retrieve data for Variable 'E_SO4C'
!   Units of 'E_SO4C' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  16,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  16,start,count,E_SO4C)

!----------------------------------------------------
!   Retrieve data for Variable 'E_NO3I'
!   Units of 'E_NO3I' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  17,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  17,start,count,E_NO3I)

!----------------------------------------------------
!   Retrieve data for Variable 'E_NO3J'
!   Units of 'E_NO3J' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  18,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  18,start,count,E_NO3J)

!----------------------------------------------------
!   Retrieve data for Variable 'E_NO3C'
!   Units of 'E_NO3C' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  19,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  19,start,count,E_NO3C)

!----------------------------------------------------
!   Retrieve data for Variable 'E_PM25I'
!   Units of 'E_PM25I' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  20,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  20,start,count,E_PM25I)

!----------------------------------------------------
!   Retrieve data for Variable 'E_PM25J'
!   Units of 'E_PM25J' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  21,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  21,start,count,E_PM25J)

!----------------------------------------------------
!   Retrieve data for Variable 'E_PM10'
!   Units of 'E_PM10' is 'ug m^-2 s^-1'
      status=nf_inq_var(ncid,  22,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  22,start,count,E_PM10)

!----------------------------------------------------
!   Retrieve data for Variable 'E_NH3'
!   Units of 'E_NH3' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  23,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  23,start,count,E_NH3)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ALD'
!   Units of 'E_ALD' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  24,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  24,start,count,E_ALD)

!----------------------------------------------------
!   Retrieve data for Variable 'E_OL2'
!   Units of 'E_OL2' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  25,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  25,start,count,E_OL2)

!----------------------------------------------------
!   Retrieve data for Variable 'E_HCHO'
!   Units of 'E_HCHO' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  26,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  26,start,count,E_HCHO)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ISO'
!   Units of 'E_ISO' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  27,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  27,start,count,E_ISO)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ETH'
!   Units of 'E_ETH' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  28,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  28,start,count,E_ETH)

!----------------------------------------------------
!   Retrieve data for Variable 'E_XYL'
!   Units of 'E_XYL' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  29,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  29,start,count,E_XYL)

!----------------------------------------------------
!   Retrieve data for Variable 'E_TOL'
!   Units of 'E_TOL' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  30,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  30,start,count,E_TOL)

!----------------------------------------------------
!   Retrieve data for Variable 'E_OLI'
!   Units of 'E_OLI' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  31,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  31,start,count,E_OLI)

!----------------------------------------------------
!   Retrieve data for Variable 'E_CH3OH'
!   Units of 'E_CH3OH' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  32,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  32,start,count,E_CH3OH)

!----------------------------------------------------
!   Retrieve data for Variable 'E_C2H5OH'
!   Units of 'E_C2H5OH' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  33,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  33,start,count,E_C2H5OH)

!----------------------------------------------------
!   Retrieve data for Variable 'E_HC3'
!   Units of 'E_HC3' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  34,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  34,start,count,E_HC3)

!----------------------------------------------------
!   Retrieve data for Variable 'E_HC5'
!   Units of 'E_HC5' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  35,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  35,start,count,E_HC5)

!----------------------------------------------------
!   Retrieve data for Variable 'E_HC8'
!   Units of 'E_HC8' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  36,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  36,start,count,E_HC8)

!----------------------------------------------------
!   Retrieve data for Variable 'E_OLT'
!   Units of 'E_OLT' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  37,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  37,start,count,E_OLT)

!----------------------------------------------------
!   Retrieve data for Variable 'E_CSL'
!   Units of 'E_CSL' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  38,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  38,start,count,E_CSL)

!----------------------------------------------------
!   Retrieve data for Variable 'E_KET'
!   Units of 'E_KET' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  39,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  39,start,count,E_KET)

!----------------------------------------------------
!   Retrieve data for Variable 'E_ORA2'
!   Units of 'E_ORA2' is 'mol km^-2 hr^-1'
      status=nf_inq_var(ncid,  40,dummy,xtype,ndim,dimids,natts)
          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      do j=1,ndim
      status=nf_inq_dim(ncid,dimids(j),dummy,len)
           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
      start(j)=1 ; count(j)=len
      end do
      status=nf_get_vara_real(ncid,  40,start,count,E_ORA2)

! -----------   Some useful advices  --------------
! If dimensions of a variable exceed 3, there can be an error (or warning)
! "warning LNK4084: total image size 382214144 exceeds max (268435456); image may not run"
! when link this program. The best way to resolve it: decrease dimensions
! of the variable, use "do ...  end do" cycle to get little data at one time.
! See NetCDF mannual to look for how to control the dimensions.
! ------------   End suggestions   -------------

!----------------------------------------------------
!  Begin writing statements to use the data.


!     Here write your own code please!
print*,E_CO(50,50,1,1)

!----------------------------------------------------
!  End Program
        stop
        end
