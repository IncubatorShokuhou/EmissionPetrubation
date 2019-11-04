program nc2f90
!-------------------------------------------------------------------------
!
!  nc2f90.f90 - This file contains code to generate Fortran code to 
!                     read in the data of a specified NetCDF file.
!  compile:
!  in Visual FORTRAN: need NetCDF library
!  in Unix : f77 -i/user/netcdf/include -lib/user/netcdf/lib/netcdf.lib -o
!  Usage:  (command line)
!  nc2f90 input_netcdf_filename.nc <output_read_netcdf.f90>
!
!  History:
!  Date         Name          Action
!  -----------  ------------  -------------------------------------------------
!  ?? Oct 1993  B. Schwartz   Created.
!  24 Aug 1994  E. Boer       Modified dimension output.
!  29 Jul 1997  S. Rupert     Standardized and reinstated required include of netcdf.inc. 
!  30 Jul 1997  S. Rupert     Added usage message and command line inputs.
!  03 Apr 2003  H. Yan        Change output to fortran 90 format, modified maximum dimesions to 5.
!  30 Mar 2004  H. Yan        Added usage message, retrieve "scale_factor" and "add_offset" parameters.
!  18 May 2006  H. Yan        Fixed bugs of dimensions definition and remark lines. 
!  17 Nov 2008  H. Yan        Fixed some bugs find by Dr. Liancong Luo, and remove num2char function
!-------------------------------------------------------------------------

!  Program_description
!  This program will ask the user for the name of a netcdf file
!  to read. it will open that file and using the proper netCDF
!  calls, get info on the variables and their dimensions. it then
!  generates a Fortran program that can be used to actually read the
!  netcDF file and fills the variables with data. This program
!  can be used to read any netCDF file. The user only has to
!  write fortran statements to print the data or pass to another
!  program. Once you have generated a program, you may use it
!  to read any file of that data type; i.e., the program is general
!  until the powers to be change variable attributes. 
    
!  Required includes.  
include 'netcdf.inc'

! Define variables.
  integer,parameter    :: mvrlen=4      ! max number of valid-range values
  integer,parameter    :: mtlen=80      ! max length of title attributes
  integer,parameter    :: maxvars=200,maxatts=20 !max number of variable and its attributes
  integer              :: ncid            ! netCDF file handle
  integer              :: rcode           ! read code
  integer              :: ndims           ! # of dimensions
  integer              :: nvars           ! # of variables
  integer              :: natts           ! # of attributes
  integer              :: recdim          ! record dimension
  integer              :: dimid           ! dimension handle
  integer              :: varid           ! variable handle
  integer              :: vartyp(200)     ! netCDF variable type
  integer              :: vdims(200)      ! variable dimensions
  integer              :: vvdims(200,10)  ! variable 
  integer              :: vsize, status
  integer              :: fsize
  integer              :: nvatts(200)     ! # of attributes assoc to given var
  character(len=31)    :: dimnam(200)     ! array holding dimension names
  character(len=31)    :: varnam(200)     ! array holding variable names
  character(len=31)    :: attnam(200,20)  ! array holding attribute names
  integer              :: dimsiz(200)     ! array holding dimension sizes
  integer              :: nvdims(200)     ! # of variable dimensions
  integer              :: attype(200,20)  ! netCDF attribute type
  integer              :: attlen(200,20)  ! attribute length
!                                  ! Note:  max # of variables is 200
!                                  !        max # of attributes per var=20 
!                                           max # of dimentions of per var is 5 now
  character(len=11)    :: avartyp(6),getvartyp(6)      ! attribute variable type
  character(len=11)    :: avt(200),avtgetvar(200)       !
  character(len=1024)  :: input_file      ! full path to input file
  character(len=1024)  :: output_file     ! full path to output file, defaults to readnet.f
  character(len=30)    :: nfcall
  data avartyp /'logical*1','character*1','integer*2','integer*4','real*4   ','real*8   '/ 
  data getvartyp /'logical','text','int2','int','real','double'/ 
  integer unlimdimid,ngatts
  character(len=1024):: text   !attribute's text value
  character(len=1024):: ch
  character(len=1024):: chdim(5)  !change dimsion digital to character to output

!     Verify command line.
      if ((iargc() .lt. 1) .or. (iargc() .gt. 2)) then
	     write(*,*) ''
		 write(*,*) 'Purpose: produce F90 program to read special NetCDF format data file'
		 write(*,*) ''
         write(*,*) 'Usage  :  nc2f90 input_file <output_file>'
         write(*,*) '          input_file :  full path to input file'
         write(*,*) '          output_file:  optional F90 output filename'
         call exit()
      endif

!     Retrieve input file path.
      call getarg(1,input_file)
!	input_file='e:\netdata\cdc.pressure\slp.2001.nc'
!     Open output file.
      if (iargc() .eq. 2) then
         call getarg(2,output_file)
      else
         output_file = 'rd_netcdf_model.f90'
      endif
      open(unit=10,file=trim(output_file),status='unknown')

!     Open netcdf file.
	    status=nf_open(trim(input_file),nf_nowrite,ncid)
		if ( status/=nf_noerr ) write (*,*) nf_strerror(status) 

!     Inquire about the number of dimensions, varaibles, and attributes.
!     Dimension ids run sequentially from 1 to ndims.
!recdim--Returned ID of the unlimited dimension, if there is one for this netCDF dataset.
!If no unlimited length dimension has been defined, -1 is returned.
	   status=nf_inq(ncid,ndims,nvars,ngatts,recdim)  !recdim=id of unlimdimid
       if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
	   
!     Store the dimension names and sizes in arrays.
      do i=1,ndims !number of dimensions returned from ncinq
         dimid=i
	      status=nf_inq_dim(ncid,dimid,dimnam(i),dimsiz(i))
	      if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
!		         Dimension ids are i, i.e, 1,ndims
!        dimnam are dimension names (character ids)
!        dimsiz is the size of each dimension
!        recdim is the id of the record dimension
         if (recdim.ne.-1) numrecs=dimsiz(recdim)
      end do

!                           Variables
!     Variables like dimensions..run sequentially from 1 to nvars.
      do i=1,nvars 
         varid=i
!        ncvinq gets variable names, their types and their shapes.
	status=nf_inq_var(ncid,varid,varnam(i),vartyp(i),nvdims(i),vdims,nvatts(I))
	if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
	
!        be careful...vdims is an array (size nvdims(i) thus the 
!        use of 2nd array
         if (nvdims(i).ne.0) then 
            do k=1,nvdims(i)
               vvdims(i,k)=vdims(k) !vvdims contains the dimension id's
            end do
         endif  
!        varnam=variable names
!        vartyp=variable types
!        nvdims=number of dimensions for variable
!        vvdims=nvdims dimension ids for this variable
!        nvatts=number of attributes for variable
      end do

!     Get info on the variable attributes.
      do i=1,nvars !get attributes for all variables
         varid=i
         do j=1,nvatts(i)
!           Get attribute names types and length. .
	status=nf_inq_attname(ncid,varid,j,attnam(i,j))
	if ( status/=nf_noerr ) write (*,*) nf_strerror(status) 
	status=nf_inq_atttype(ncid,varid,attnam(i,j),attype(i,j))
	if ( status/=nf_noerr ) write (*,*) nf_strerror(status) 
	status=nf_inq_attlen(ncid,varid,attnam(i,j),attlen(i,j))
	if ( status/=nf_noerr ) write (*,*) nf_strerror(status) 
        end do
      end do 

!c     Now that the data have been retrieved, generate the fortran program 
!c     interface to read the data into memory.
      do i=1,nvars
         varid=i
         avt(i)=avartyp(vartyp(varid)) !avt is the character name of vartyp
		 avtgetvar(i)=getvartyp(vartyp(varid))
      end do

!     Generate the fortran template with variable types, names, and dims.
!     Allow variables to have 5 dimensions.
      write(10,'(9a)') '!-------------------------------------------------------'
      write(10,'(9a)') '!  '
      write(10,'(9a)') '!  ',trim(output_file)
      write(10,'(9a)') '!    This file is a fortran template file designed to read the given'
      write(10,'(9a)') '!     netCDF file ',"'",trim(input_file),"'",' into memory.'
      write(10,'(9a)') '!  '
      write(10,'(9a)') '!  History: '
      write(10,'(9a)') '!  Date       Name          Action'
      write(10,'(9a)') '!-------------------------------------------------------'
      write(10,'(9a)') '!  '
      write(10,'(9a)') '!  ?? Oct 1993  B. Schwartz   Created.'
	  write(10,'(9a)') '!  24 Aug 1994  E. Boer       Modified dimension output.'
      write(10,'(9a)') '!  29 Jul 1997  S. Rupert     Standardized and reinstated required include of netcdf.inc. '
      write(10,'(9a)') '!  30 Jul 1997  S. Rupert     Added usage message and command line inputs.'
      write(10,'(9a)') '!  03 Apr 2003  H. Yan        Change output to fortran 90 format, modified maximum dimesions to 5.'
      write(10,'(9a)') '!  30 Mar 2004  H. Yan        Added usage message, retrieve "scale_factor" and "add_offset" parameters.'
      write(10,'(9a)') '!  18 May 2006  H. Yan        Fixed bugs of dimensions definition and remark lines. '
      write(10,'(9a)') '!-------------------------------------------------------'
      write(10,'(9a)') '!  Do not forget to include the -I path_to_netcdf_  includes in your compile statement Required includes.'
	  write(10,'(9a)') '!  Also note: need ',"'",'netcdf.lib',"'",' or ',"'",'netcdfs.lib',"'", ' when link .'
      write(10,'(9a)') "      include 'netcdf.inc'"
      write(10,'(9a)') '!'
	  write(10,'(9a)') '!-------------------------------------------------------'
	  write(10,'(9a)') '!'
	  write(10,'(9a)') '!    Using below command line to get the head information of the NetCDF file:'
	  write(10,'(9a)') '!    Please execute " ncdump -h ',trim(input_file),' > output.txt " '
	  write(10,'(9a)') '!-------------------------------------------------------'
	  write(10,'(9a)') '!'
      write(10,'(9a)') '!     Define Variables.'
      write(ch,*) nvars
      write(10,'(9a)') '!     Variable ids run sequentially from 1 to nvars=',trim(adjustl(ch)), ' ! number of variables'
      if(recdim .ne. -1) write(ch,*) dimsiz(recdim)
      if (recdim.ne.-1) write(10,'(9a)') '      integer,parameter :: nrec=',trim(adjustl(ch)),'  ! change this to generalize' 
      write(10,'(9a)') '      integer*4  ncid, status','    ! file control'
!      if (recdim.ne.-1) write(10,'(9a)') '      integer*4 recdim ','  ! record dimension'
	  write(10,'(9a)') '!-------------------------------------------------------------'
	  write(ch,*) nvars
	  write(10,'(9a)') ' !     Below ',trim(adjustl(ch)),' variables is the data in netCDF file'
      
	  
	  do i=1,nvars
	     varid=i
         vsize=index(varnam(varid),' ')-1
         if (nvdims(varid).eq.0) write(10,'(9a)')  '      ',avt(varid),'        ',trim(adjustl(varnam(varid)))
         !Define the output dimesion (digitals to characters). Add by Yan
             if(nvdims(varid)>=1) then
                    do j=1,nvdims(varid)
                    write(chdim(j),*) dimsiz(vvdims(varid,j))
!                    chdim(j)=num2cha( dimsiz(vvdims(varid,j)) )
                    chdim(j)=adjustl(chdim(j))
                    enddo
              endif      
         if (nvdims(varid).eq.1) then ! single dimension variable
            if (recdim.ne.-1) then 
               if (vvdims(varid,1).eq.recdim) then
                  write(10,'(9a)') '      ',avt(varid),'    ','::','  ',trim(adjustl(varnam(varid))),'(nrec)'
               endif
               if (vvdims(varid,1).ne.recdim) then    
                  write(10,'(9a)') '      ',avt(varid),'    ','::','  ',trim(adjustl(varnam(varid))),'( ',trim(chdim(1)),' )' !dimsiz(vvdims(varid,1)),')'
               endif
            endif 
            if (recdim.eq.-1) write(10,581) avt(varid),&
     trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid))  !(dimsiz(vvdims(varid,j)),j=1,nvdims(varid))
     !sometimes vsize is less than 1, so here use vsize, not 1:vsize
  581       format(6x,a,2x,a,'( ',a,' )')
!  581       format(6x,a,2x,a,'(',i9,')')
         endif 
         if (nvdims(varid).eq.2) then ! double dimension variable
            if (recdim.ne.-1) then 
             if (nvdims(varid).eq.recdim) then
      write(10,585) avt(varid),trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid)-1)  !&
!     (dimsiz(vvdims(varid,j)),j=1,nvdims(varid)-1)
  585          format(6x,a,4x,'::',2x,a,'( ',a,', nrec )')
!  585          format(6x,a,4x,'::',2x,a,'(',i9,',nrec)')
             else
      write(10,584) avt(varid),trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid))  !&
!     (dimsiz(vvdims(varid,j)),j=1,nvdims(varid))
  584          format(6x,a,4x,'::',2x,a,'( ',a,', ',a,' )')
!   584          format(6x,a,4x,'::',2x,a,'(',i9,',',i9,')')

             endif
            endif
            if (recdim.eq.-1) write(10,586) avt(varid),trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid))  !&
!     (dimsiz(vvdims(varid,j)),j=1,nvdims(varid))
  586          format(6x,a,4x,'::',2x,a,'( ',a,', ',a,' )') 
!    586          format(6x,a,4x,'::',2x,a,'(',i9,',',i9,')') 

         endif
         if (nvdims(varid).eq.3) then ! triple dimension variable
            if (recdim.ne.-1) write(10,590) avt(varid),trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid)-1)   !&
!     (dimsiz(vvdims(varid,j)),j=1,nvdims(varid)-1)
  590          format(6x,a,4x,'::',2x,a,'( ',a,', ',a,', nrec )')
!    590          format(6x,a,4x,'::',2x,a,'(',i9,',',i9,',nrec)')

            if (recdim.eq.-1) write(10,591) avt(varid),trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid))   !&
!     (dimsiz(vvdims(varid,j)),j=1,nvdims(varid))
  591          format(6x,a,4x,'::',2x,a,'( ',a,', ',a,', ',a,' )')
!    591          format(6x,a,4x,'::',2x,a,'(',i9,',',i9,',',i9,')')
         endif
         if (nvdims(varid).eq.4) then !variable with 4 dimensions 
            if (recdim.ne.-1) write(10,595) avt(varid),trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid)-1)   !&
!     (dimsiz(vvdims(varid,j)),j=1,nvdims(varid)-1) 
  595          format(6x,a,2x,a,'( ',a,', ',a,', ',a,', nrec )')
!    595          format(6x,a,2x,a,'(',i9,',',i9,',',i9,',nrec)')
            if (recdim.eq.-1) write(10,596) avt(varid),trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid))   !&
!     (dimsiz(vvdims(varid,j)),j=1,nvdims(varid))
  596          format(6x,a,2x,a,'( ',a,', ',a,', ',a,', ',a,' )')
!  596          format(6x,a,2x,a,'(',i9,',',i9,',',i9,',',i9,')')
         endif 
		 if (nvdims(varid).eq.5) then !variable with 5 dimensions (rare)
            if (recdim.ne.-1) write(10,597) avt(varid),trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid)-1)   !&
!     (dimsiz(vvdims(varid,j)),j=1,nvdims(varid)-1) 
  597          format(6x,a,2x,a,'( ',a,', ',a,', ',a,', ',a,', nrec )')
!  597          format(6x,a,2x,a,'(',i9,',',i9,',',i9,',',i9,',nrec)')
            if (recdim.eq.-1) write(10,598) avt(varid),trim(adjustl(varnam(varid))),(trim(chdim(j)),j=1,nvdims(varid))   !&
!     (dimsiz(vvdims(varid,j)),j=1,nvdims(varid))
  598          format(6x,a,2x,a,'( ',a,', ',a,', ',a,', ',a,', ',a,' )')
!  598          format(6x,a,2x,a,'(',i9,',',i9,',',i9,',',i9,',',i9,')')
         endif 
      end do
      write(ch,*) nvars  !ch=num2cha(nvars)
	  write(10,'(9a)') '!     above',trim(adjustl(ch)),' variables is the data in netCDF file'
	  write(10,'(9a)') '!-------------------------------------------------------------'
      write(10,'(9a)') '     integer*4   :: start(10)'
	  write(10,'(9a)') '     integer*4   :: count(10)'
      write(10,'(9a)') '     integer*4   :: dimids(10)',  '! allow up to 10 dimensions'
	  write(10,'(9a)') '     integer*4   :: ndim, xtype'
  	  write(10,'(9a)') '     integer*4   :: natts,j,len'
      write(10,'(9a)') '     character(len=31) :: dummy'
      write(10,'(9a)') '!----------------------------------------------------------------'
	  write(10,'(9a)') '!  Define "scale_factor" and "add_offset" variables'

	  	do i=1,nvars;do j=1,20
			 if(attnam(i,j)=='scale_factor') then
			 write(ch,*) i
			 write(10,*) '      ',avartyp(attype(i,j)), '::', '  scale'//TRIM(adjustl(ch))//'(1)',",",' add'//TRIM(adjustl(ch))//'(1)'
			 endif
		enddo;enddo

	write(10,'(9a)') '!----------------------------------------------------------------'


!c     Write the statement to open the netCDF file.
      write(10,'(9a)') ''
      write(10,'(9a)') '! Open netCDF file.' 
      write(10,'(9a)') "      status=nf_open('",trim(input_file),"'",',nf_nowrite,ncid)'
	  write(10,'(9a)') '          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)'
 
!c     Get info on the record dimension for this file.
      if (recdim.ne.-1) then
         write(10,*)
       !  write(10,*) '!    Get info on the record dimension for this file.'
       !  write(10,*) '     status=nf_inq(ncid,ndims,nvars,ngatts,recdim)'
	   !  write(10,*) '          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)'
       !  write(10,*) '     status=nf_inq_dim(ncid,dimid,dummy,len)'
	   !  write(10,*) '          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)' 
       !  write(10,*)'!    len now contains the # of dimensions for this file'
       endif   
      
!c     Get info on the dimensions; recdim will contain the id of the 
!c     record dimension.
!c     Generate call statements to fill variables with values.
!c     In order to make the generated program usable, we need info
!c     on the dimensions of the variables. if we do this in the pgm,
!c     the only variable not with a constant dimension is the record
!c     variable.
      do i=1,nvars
         write(10,'(9a)')
		 write(10,'(9a)') '!----------------------------------------------------'
		 write(10,'(9a)') '!   Retrieve data for Variable ', "'",trim(varnam(i)),"'"
	  	 do j=1,20   !get attributes 'long_name' and 'units'
		 if(attnam(i,j)=='long_name') then
		 status=nf_get_att_text(ncid,i,'long_name',text)
		 ch='!   Long_name of '// "'"//trim(varnam(i))//"'"//' is '//"'"//trim(text(1:attlen(i,j)))//"'"
		 write(10,'(a)')  trim(adjustl(ch))
		 elseif(attnam(i,j)=='units') then
	     status=nf_get_att_text(ncid,i,'units',text)
	     ch='!   Units of '// "'"//trim(varnam(i))//"'"//' is '//"'"//trim(text(1:attlen(i,j)))//"'"
	 	 write(10,'(a)')  trim(adjustl(ch))
		 endif
		 enddo

         lenstr=1
         k=0
         write(10,1015) i
 1015    format(6x,'status=nf_inq_var(ncid,',i4,',dummy,xtype,ndim,dimids,natts)')
		 write(10,'(9a)') '          if ( status/=nf_noerr ) write (*,*) nf_strerror(status)' 
!        Get number of dims(ndim) and their ids ndim and dimids.
         write(10,'(9a)')  '      do j=1,ndim'
         write(10,'(9a)') '      status=nf_inq_dim(ncid,dimids(j),dummy,len)'
!c        Get the size of each nvdim dimension in ndsize.
		 write(10,'(9a)') '           if ( status/=nf_noerr ) write (*,*) nf_strerror(status)' 
         write(10,'(9a)') '      start(j)=1 ; count(j)=len' 
         write(10,'(9a)') '      end do'
            
	     if (vartyp(i).eq.2) then !character variables
		 nfcall='status=nf_get_vara_'//trim(avtgetvar(i))
            write(10,1250) nfcall,i,trim(varnam(i))
 1250       format(6x,a,'(ncid,',i4,',start',',count,',a,')')
         else
		 nfcall='status=nf_get_vara_'//trim(avtgetvar(i))
		 		 
            write(10,1350) trim(nfcall),i,trim(varnam(i))
 1350       format(6x,a,'(ncid,',i4,',start',',count,',a,')')
         endif

		 !if there are attributes 'scale_factor' and 'add_offset' get the value
		 iiflag=0
		 do j=1,20
		 if(attnam(i,j)=='scale_factor' .or. attnam(i,j)=='add_offset') then
		   iiflag=iiflag+1
			if(iiflag==1) then
			 write(10,'(9a)') ''
			 write(ch,*) i
			 write(10,'(9a)') '      scale'//TRIM(adjustl(ch))//'(1)',' =0.0',' ; ','add'//TRIM(adjustl(ch))//'(1)',' =0.0'
			 write(10,'(9a)') '! Scale_factor and add_offset for variable ', "'", trim(varnam(i)), "'"
			endif

		    nfcall='status=nf_get_att_'//trim(getvartyp(attype(i,j)))
			text=attnam(i,j)
		    if(attnam(i,j)=='scale_factor') then
		    write(ch,*) i
			write(10,1360) trim(nfcall),i,trim(text),'scale'//TRIM(adjustl(ch))//'(1)'
			else
			write(10,1360) trim(nfcall),i,trim(text),'add'//TRIM(adjustl(ch))//'(1)'
			endif
 1360       format(6x,a,'(ncid,',i4,',',"'",a,"'",',',a,')')
		 endif
		 enddo

		 if(iiflag/=0) then
		  write(10,'(9a)') ''
		  write(10,'(9a)') '! add scale_factor and add_offset to get true value'
		  ch='! Caution: variable type of '//"'"//trim(varnam(i))//"'"//' may not as the same as the type of'
		  write(10,'(a)') trim(adjustl(ch))
		  write(ch,*) i
		  write(10,'(9a)') '! ',"'",'scale'//trim(adjustl(ch))//'(1)',"'", ' and ', "'",'add'//trim(adjustl(ch))//'(1)',"'",', you must change it youself!'
		  write(10,'(9a)') '      ',trim(varnam(i)),'=',trim(varnam(i)),'*','scale'//trim(adjustl(ch))//'(1)','+','add'//trim(adjustl(ch))//'(1)'
		 endif
		  
      end do
	  write(10,'(9a)') ''
	  write(10,'(9a)') '! -----------   Some useful advices  --------------'
	  write(10,'(9a)') '! If dimensions of a variable exceed 3, there can be an error (or warning)'
	  write(10,'(9a)') '! "warning LNK4084: total image size 382214144 exceeds max (268435456); image may not run"'
	  write(10,'(9a)') '! when link this program. The best way to resolve it: decrease dimensions'
	  write(10,'(9a)') '! of the variable, use "do ...  end do" cycle to get little data at one time.'
	  write(10,'(9a)') '! See NetCDF mannual to look for how to control the dimensions.'
	  write(10,'(9a)') '!------------   End suggestions   -------------'
	  write(10,'(9a)') ''
	  write(10,'(9a)') '!----------------------------------------------------'
      write(10,'(9a)') '!  Begin writing statements to use the data.'
      write(10,'(9a)') ''
	  write(10,'(9a)') ''
	  write(10,'(9a)') '!     Here write your own code please!'
      write(10,'(9a)') ''
	  write(10,'(9a)') ''
	  write(10,'(9a)') '!----------------------------------------------------'
	  write(10,'(9a)') '!  End Program'
	  write(10,'(9a)') '        stop'
	  write(10,'(9a)') '        end'
!c
      write(*,*)
	  write(*,'(9a)')'Generated F90 program named ',"'",trim(output_file),"'"
	  write(*,*) 

!c
stop

!contains
!
!      FUNCTION NUM2CHA(NUM,IDIGITAL) RESULT (NUM2CHAR_RESULT) 
!!  FUNCTION TO GET CHARACTER FROM NUMBER 	  
!!  NOTE: THE MAX LENGTH OF NUM IS 10 DIGITALS AND NUM<2.1E9
!!  18 May, 2006 added by Dr.  H. Yan
!!  Email: yhmyhm@gmail.com
!!
!	  IMPLICIT NONE
!      INTEGER*4,INTENT(IN):: NUM
!	  INTEGER*4,OPTIONAL,INTENT(IN):: IDIGITAL
!	  CHARACTER(LEN=12) NUM2CHAR_RESULT
!
!	  INTEGER*4 C,NUM2,D,DIG,MAXLEN,I,II,NUM_LEN,IDIGITAL0
!	  PARAMETER(MAXLEN=12)
!	  CHARACTER(LEN=MAXLEN) CHA,CHA0
!	  
!	  IF(PRESENT(IDIGITAL)) THEN
!	  IDIGITAL0=IDIGITAL  ! ADD '0' BEFORE NUMBER
!		  IF(IDIGITAL0>12) THEN
!		  PRINT *, 'ERROR IN FUNCTION NUM2CHAR(2nd parameter),THE MAXIMUM DIGITAL IS 12'
!		  STOP
!		  ENDIF
!	  ELSE
!	  IDIGITAL0=0
!	  ENDIF
!	  
!	  DO I=MAXLEN,2,-1   ! GET THE LENGTH OF NUM
!	  	  
!	  IF(NUM<10) then
!	  NUM_LEN=1
!	  exit
!	  endif
!      
!	  if(i>=7) then  !only 10**9 is correct, when calculate 10**10,it gives a wrong value
!	  II=NUM/1000/(10**(I-1-3)) ! so here we use another form of (10**(i-1)) to avoid error
!	  else
!	  ii=num/(10**(i-1))
!	  endif
!
!	  IF(II/=0) THEN
!	  NUM_LEN=I
!	  EXIT
!	  ENDIF
!	  ENDDO
!
!	  NUM2=NUM
!      DO C=1,NUM_LEN 
!           D=NUM_LEN-C 
!           DIG=NUM2/(10**D) 
!           CHA(C:C)=CHAR(48+DIG) 
!           NUM2=NUM2-DIG*10**D 
!      END DO
!	  
!	  
!	  IF(IDIGITAL0<=NUM_LEN) THEN
!	  NUM2CHAR_RESULT=CHA(1:NUM_LEN) 
!	  ELSE
!	  IDIGITAL0=IDIGITAL0-NUM_LEN
!	  DO I=1,IDIGITAL0
!	  CHA0(I:I)=CHAR(48)
!	  ENDDO
!
!	  NUM2CHAR_RESULT=CHA0(1:IDIGITAL0)//CHA(1:NUM_LEN)
!
!	  ENDIF
!
!	  return	        
!	  END FUNCTION NUM2CHA



end program nc2f90
        



