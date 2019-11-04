PROGRAM read_wrfchemi
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'
  INTEGER(4),PARAMETER :: imt_grd_cnst = 10
  INTEGER(4),PARAMETER :: imt_grd      = 11
  INTEGER(4) :: Time,DateStrLen,south_north,west_east,emissions_zdim_stag,ierr
  INTEGER(4) :: vname,xtype,ndims,natts,dimname,len,len_file
  INTEGER(4) :: ncid,varid,dimids(4),shape(3)
  INTEGER(4) :: reclength,irec
  INTEGER(4) :: i,j,k
  INTEGER(4),ALLOCATABLE :: start(:),count(:)
  
  CHARACTER(1),ALLOCATABLE :: Times(:,:)
  REAL(4),ALLOCATABLE :: XLONG(:,:)
  REAL(4),ALLOCATABLE :: XLAT(:,:)
  REAL(4),ALLOCATABLE :: E_CO(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ECI(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ECJ(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ECC(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_NO(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_NO2(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_SO2(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ORGI(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ORGJ(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ORGC(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_SO4I(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_SO4J(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_SO4C(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_NO3I(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_NO3J(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_NO3C(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_PM25I(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_PM25J(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_PM10(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_NH3(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ALD(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_OL2(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_HCHO(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ISO(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ETH(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_XYL(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_TOL(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_OLI(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_CH3OH(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_C2H5OH(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_HC3(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_HC5(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_HC8(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_OLT(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_CSL(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_KET(:,:,:,:)
  REAL(4),ALLOCATABLE :: E_ORA2(:,:,:,:)

! count the length of the filename:     A123456789B123456789C123456789D123456789E123456789F
  CHARACTER(35) :: ncin                = 'wrfchemi_d01_2016-11-01_00:00:00.nc'
!  CHARACTER(9) :: grdout_cnst         = 'const.grd'
!  CHARACTER(9) :: grdout              = 'guess.grd'
! 
! initialization
! E_CO:
! name of           1 is  west_east       length=         159
! name of           2 is  south_north     length=          99
! name of           3 is  emissions_zdim_ length=           1
! name of           4 is  Time            length=           1
! XLAT:                                              
! name of           1 is  west_east       length=         159
! name of           2 is  south_north     length=          99
! Times:
! name of           1 is  DateStrLen      length=          19
! name of           2 is  Time            length=           1

! initialization
! 4-D var(emission)
  ! read the nc file;
  CALL check_io(NF_OPEN(ncin,NF_NOWRITE,ncid))
  ! read a 4-D variable of the nc file;
  CALL check_io(NF_INQ_VARID(ncid,'E_CO',varid))
  CALL check_io(NF_INQ_VARDIMID(ncid,varid,dimids))
  DO i=1,4 ! the number 4 means that there are 4 dimensions in the var
    CALL check_io(NF_INQ_DIMLEN(ncid,dimids(i),shape(i)))
  END DO
  west_east = shape(1)
  south_north = shape(2)
  emissions_zdim_stag = shape(3)
  Time = shape(4)
  WRITE(6,'(A)') '*** grid information:E_CO ***'
  WRITE(6,'(3(2X,A,I5))') 'west_east =',west_east,'south_north =',south_north,'emissions_zdim_stag =',emissions_zdim_stag,'Time=',Time
! 2-D var:XLONG,XLAT
  ! read the nc file;
  CALL check_io(NF_OPEN(ncin,NF_NOWRITE,ncid))
  ! read a 2-D variable of the nc file;
  CALL check_io(NF_INQ_VARID(ncid,'XLONG',varid))
  CALL check_io(NF_INQ_VARDIMID(ncid,varid,dimids))
  DO i=1,2 ! the number 2 means that there are 2 dimensions in the var
    CALL check_io(NF_INQ_DIMLEN(ncid,dimids(i),shape(i)))
  END DO
  west_east = shape(1)
  south_north = shape(2)
  WRITE(6,'(A)') '*** grid information:XLONG ***'
  WRITE(6,'(3(2X,A,I5))') 'west_east =',west_east,'south_north =',south_north
! 2-D var:Times
  ! read the nc file;
  CALL check_io(NF_OPEN(ncin,NF_NOWRITE,ncid))
  ! read a 2-D variable of the nc file;
  CALL check_io(NF_INQ_VARID(ncid,'Times',varid))
  CALL check_io(NF_INQ_VARDIMID(ncid,varid,dimids))
  DO i=1,2 ! the number 2 means that there are 2 dimensions in the var
    CALL check_io(NF_INQ_DIMLEN(ncid,dimids(i),shape(i)))
  END DO
  DateStrLen = shape(1)
  Time = shape(2)
  WRITE(6,'(A)') '*** grid information:XLONG ***'
  WRITE(6,'(3(2X,A,I5))') 'DateStrLen =',DateStrLen,'Time =',Time


! ALLOCATE valiables
!
  ALLOCATE(Times(Time,DateStrLen))
  ALLOCATE(XLONG(south_north,west_east))
  ALLOCATE(XLAT(south_north,west_east))
  ALLOCATE(E_CO(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ECI(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ECJ(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ECC(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_NO(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_NO2(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_SO2(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ORGI(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ORGJ(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ORGC(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_SO4I(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_SO4J(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_SO4C(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_NO3I(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_NO3J(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_NO3C(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_PM25I(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_PM25J(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_PM10(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_NH3(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ALD(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_OL2(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_HCHO(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ISO(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ETH(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_XYL(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_TOL(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_OLI(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_CH3OH(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_C2H5OH(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_HC3(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_HC5(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_HC8(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_OLT(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_CSL(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_KET(Time,emissions_zdim_stag,south_north,west_east))
  ALLOCATE(E_ORA2(Time,emissions_zdim_stag,south_north,west_east))  
! Start to READ












END PROGRAM read_wrfchemi
!-----------------------------------------------------------------------
! Check the status of netcdf io
!-----------------------------------------------------------------------
SUBROUTINE check_io(status)
  IMPLICIT NONE
  INCLUDE 'netcdf.inc'  
  INTEGER(4),INTENT(IN) :: status
  
  IF(status /= nf_noerr) THEN
    WRITE(6,*) TRIM(nf_strerror(status))
    STOP 10
  ENDIF
  
  RETURN
END SUBROUTINE check_io
!-----------------------------------------------------------------------
! Monit 3-D variables
!-----------------------------------------------------------------------
SUBROUTINE monit_3d(elem,south_north,west_east,emissions_zdim_stag,var)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: elem
  INTEGER(4),INTENT(IN) :: south_north,west_east,emissions_zdim_stag
  REAL(4),INTENT(IN) :: var(south_north,west_east,emissions_zdim_stag)
  INTEGER(4) :: ied,jed,ked,k
  
  IF(elem == 'U') THEN
    ied = west_east
    jed = south_north - 1
    ked = emissions_zdim_stag - 1
  ELSE IF(elem == 'V') THEN
    ied = west_east - 1
    jed = south_north
    ked = emissions_zdim_stag - 1
  ELSE IF(elem == 'W') THEN
    ied = west_east - 1
    jed = south_north - 1
    ked = emissions_zdim_stag
  ELSE
    ied = west_east - 1
    jed = south_north - 1
    ked = emissions_zdim_stag - 1
  END IF

  WRITE(6,'(3a)') ' === minmax monitor of ',elem,' ==='
  IF(elem == 'P') THEN
    DO k=1,ked
      WRITE(6,'(i5,2f12.5)') k,minval(var(1:ied,1:jed,k)) / 100.0,maxval(var(1:ied,1:jed,k))/ 100.0
    END DO
  ELSE
    DO k=1,ked
      WRITE(6,'(i5,2f12.5)') k,minval(var(1:ied,1:jed,k)),maxval(var(1:ied,1:jed,k))
    END DO
  END IF
    
  RETURN
END SUBROUTINE monit_3d

SUBROUTINE read_var_nc(rfile,lat,lon,VAR)
IMPLICIT NONE
INCLUDE 'netcdf.inc'            ！这里用到了netcdf库
INTEGER,PARAMETER :: nx=720,ny=361,nt=4
INTEGER ncid,ierr,varidx,varidy,varid,err
CHARACTER(len=50) rfile
REAL*4 lat(ny),lon(nx),VAR(nx,ny,nt)
ierr=NF_OPEN(trim(rfile),NF_NOWRITE,ncid)
ierr=NF_INQ_VARID(ncid,'longitude',varidx)
ierr=NF_INQ_VARID(ncid,'latitude',varidy)
ierr=NF_GET_VAR_REAL(ncid,varidx,lon)
ierr=NF_GET_VAR_REAL(ncid,varidy,lat)
ierr=NF_INQ_VARID(ncid,'APCP_surface',varid)
ierr=NF_GET_VAR_REAL(ncid,varid,VAR)
! print *,VAR(1,1,1),VAR(720,361,4)
ierr=NF_CLOSE(ncid)
END SUBROUTINE read_rain_nc