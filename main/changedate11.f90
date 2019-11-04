PROGRAM changedate
IMPLICIT NONE
include 'netcdf.inc'
!-------------------------------------------------------
!    Using below command line to get the head information of the NetCDF file:
!    Please execute " ncdump -h ./origin/1level/wrfchemi_d01_2016-11-01_00:00:00.nc > output.txt " 
!-------------------------------------------------------
!     Define Variables.
!     Variable ids run sequentially from 1 to nvars=40 ! number of variables
      integer*4  ncid, status    ! file control
!-------------------------------------------------------------
 !     Below 40 variables is the data in netCDF file
      character*1    ::  Times( 19, 1 )
      INTEGER(4),PARAMETER :: Nmembers=10 ! the number of the members;also used in creating DIRs
!     above40 variables is the data in netCDF file
!-------------------------------------------------------------
     integer*4   :: start(10)
     integer*4   :: count(10)
     integer*4   :: dimids(10)! allow up to 10 dimensions
     integer*4   :: ndim, xtype,date,hour
     integer*4   :: natts,j,len
     character(len=31) :: dummy
     character(len=1) :: date1,date2,hour1,hour2,NNmembersNN1,NNmembersNN2,NNmembersNN3
     INTEGER*4   :: NNmembersNN
!----------------------------------------------------------------
!  Define "scale_factor" and "add_offset" variables
!----------------------------------------------------------------
do NNmembersNN=0,Nmembers-1,1
	   call Nmembers_string(NNmembersNN,NNmembersNN1,NNmembersNN2,NNmembersNN3)
	   call system ('cd ../'//Trim(NNmembersNN1)//''//Trim(NNmembersNN2)//''//Trim(NNmembersNN3)//'/; bash copy11.sh' )
  do date=1,31,1
	  do hour=0,23,1
		  call date_hour_string(date,date1,date2)
		  call date_hour_string(hour,hour1,hour2)
! Open netCDF file.
      status=nf_open('../'//Trim(NNmembersNN1)//''//Trim(NNmembersNN2)//''//Trim(NNmembersNN3)//'/wrfchemi_d01_2016-11-'//Trim(date1)//''//Trim(date2)//'_'//Trim(hour1)//''//Trim(hour2)//':00:00.nc',nf_write,ncid)
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
!  Begin writing statements to use the data.
      Times(9,1)=date1
      Times(10,1)=date2
      Times(12,1)=hour1
      Times(13,1)=hour2
      status=NF_PUT_VAR(ncid,1,Times) 
      status=nf_close(ncid)
    end do
  end do
end do
!----------------------------------------------------
End Program changedate
SUBROUTINE date_hour_string(input,output1,output2)
  implicit none
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
SUBROUTINE Nmembers_string(input,output1,output2,output3)
  implicit none
    INTEGER::input,input100,input10,input1,inputx0
    character::output1,output2,output3
    IF (input .LT. 10) THEN
      output1="0"
      output2="0"
      write(output3,'(i1)')input
    END IF
    IF (input .GT. 9 .AND. input .LT. 100) THEN
    	input1=MOD(input,10)
    	input10=(input-input1)/10
    	output1="0"
    	write(output2,'(i1)')input10
    	write(output3,'(i1)')input1    	
    END IF
    IF (input .GT. 99 .AND. input .LT. 1000) THEN
    	input1=MOD(input,10)
    	inputx0=(input-input1)/10
    	input10=MOD(inputx0,10)
    	input100=(input-input1-input10*10)/100
    	write(output1,'(i1)')input100
    	write(output2,'(i1)')input10
    	write(output3,'(i1)')input1    	
    END IF
END SUBROUTINE Nmembers_string