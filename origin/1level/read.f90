PROGRAM read_wrfchemi
      implicit none
      integer, parameter :: LLm=145, MMm=96, Ndas=25
      integer ierr,ncid,varid,len_file,xtype,ndims,dimids(3),natts
      real  h(LLm,MMm,Ndas)
      character*299 file_wrfout
      character*50 vname
      integer i,j,k,len
      character*15 dimname
      integer Time,emissions_zdim_stag,south_north,west_east,DateStrLen
      include 'netcdf.inc'

      
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

! initialization
!
      file_wrfout='wrfchemi_d01_2016-11-01_00:00:00.nc'

      len_file=len_trim(file_wrfout)
      ierr=nf_open(trim(file_wrfout),nf_nowrite,ncid)
      ierr=nf_inq_varid (ncid, 'Times', varid)
      ierr=nf_inq_var(ncid,varid,vname,xtype,ndims,dimids,natts)   !获取变量信息
      print*,'ierr=',ierr                   
      print*,'vname=',vname     !变量名
      print*,'xtype=',xtype          !变量类型，4表示整型，5表示实型，6表示双精度
      print*,'ndims=',ndims        !变量维数
      print*,'dimids=',dimids      !每一维的ID

      do i=1,ndims
         ierr=nf_inq_dim(ncid,dimids(i),dimname,len)
         print*,'name of',i,'is  ',  dimname,' length=',len
      enddo
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
