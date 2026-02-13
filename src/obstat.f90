program obstat

  use params, only : n_data_ids, n_vert_ids, n_stat_ids, n_data_elements
  implicit none
  
  character(len=200) :: cline, infile, carg
  character(len=100), dimension(n_data_ids) :: obtype_name
  character(len=100) :: obtype

  integer :: i_rtype, i_satid, i_instid, i_obstyp, i_varno, i_vertc, i_tslot, i_time
  integer :: i_rtype_varno, i_status
  integer :: data_id, vert_id, vertind_inv
  integer :: i, j, k, lok, ind1, ind2, jtype, jline, iargc, n_effective, tslot

  real, dimension(n_data_elements) :: data_array
  real, dimension(n_data_ids,n_vert_ids,n_stat_ids) :: stat_array

  logical :: fok, activeonly

! integer, parameter :: n_ign_reportypes=8
! integer, dimension(n_ign_reportypes), parameter :: ign=(/ &
!      16001, & ! Automatic Land SYNOP
!      16002, & ! Manual Land SYNOP
!      16006, & ! Automatic SHIP
!      16026, & ! AIREP
!      16074, & ! BUFR SHIP SYNOP
!      16076, & ! BUFR LAND SYNOP
!      16083, & ! BUFR MOORED BUOYS
!      16084 /) ! BUFR DRIFTING BUOYS
! integer :: j_ign, icount_ign(n_ign_reportypes)
! character(len=20), dimension(n_ign_reportypes) :: c_ign
       

! Unsupported data type names
! c_ign(:) = (/ &
!      'Automatic Land SYNOP', &
!      'Manual Land SYNOP   ', &
!      'Automatic SHIP      ', &
!      'AIREP               ', &
!      'BUFR SHIP SYNOP     ', &
!      'BUFR LAND SYNOP     ', &
!      'BUFR MOORED BUOYS   ', &
!      'BUFR DRIFTING BUOYS ' /)


  if (iargc()<1) then
     n_effective=n_data_elements-1
     tslot=0
  else
     call getarg(1,carg)
     read (carg,*,iostat=i_status) tslot
     if (i_status/=0 .or. tslot==-1) then
        n_effective=n_data_elements-1
        tslot=0
     else
        n_effective=n_data_elements
     endif
  endif

  activeonly=.true.
  if (iargc()>1) activeonly=.false.
  
! Initialize
  stat_array(:,:,:)=0.0
  stat_array(:,:,4)=9.9e9
  stat_array(:,:,5)=-9.9e9
  stat_array(:,:,8)=9.9e9
  stat_array(:,:,9)=-9.9e9
  stat_array(:,:,12)=9.9e9
  stat_array(:,:,13)=-9.9e9
  stat_array(:,:,16)=9.9e9
  stat_array(:,:,17)=-9.9e9
  obtype_name(:)=''
! icount_ign(:)=0

!
  open (111,file='obstat.out',status='replace')
  open (112,file='obstat.log',status='replace')
  write (112,'(a)') '---Start of header---'


!  obtype_loop: do jtype=1,5
  obtype_loop: do jtype=1,3

! Input ASCII file
     if (jtype==1) then
        infile='extract_conv.dat'
     elseif(jtype==2) then
        infile='extract_radar.dat'
     elseif(jtype==3) then
        infile='extract_nonconv.dat'
     else
        cycle obtype_loop
     endif

     inquire (file=trim(infile),exist=fok)
     if (fok) then
        write (112,'(a)') 'Reading input file: ' // trim(infile)
        open (11,file=trim(infile), status='old')
        jline=0
     else
        write (112,'(a)') 'Input file not found: ' // trim(infile)
        cycle
     endif

     reading: do

! Read new entry
        read (11,'(a)',iostat=lok) cline
        if (lok<0) exit reading

        jline=jline+1
        if (lok>0) cycle reading

        ind1=INDEX(cline,'NULL')
        do while (ind1>0)
           write (cline(ind1:ind1+3),'(a)') '-9.9'
           ind1=INDEX(cline,'NULL')
        enddo

        data_array(n_data_elements)=0.0
        read (cline,*,iostat=lok) data_array(1:n_effective)
        if (lok>0) then
           if (INDEX(cline,'lon@hdr')>0) cycle
           write (112,'(a,i8)') 'Error reading input file on line ', jline
           write (112,'(a)') '---'
           write (112,'(a)') trim(cline)
           write (112,'(a)') '---'
           exit reading
        endif

! Extract data
        if (tslot==0) then
           i_tslot=0
        else
           i_tslot=nint(data_array(n_data_elements))
           if (i_tslot==-3) then
              i_time=nint(data_array(14))
              call tslot_from_time(i_time,i_tslot)
           endif
        endif

        if (i_tslot/=tslot) cycle
        
        i_status=nint(data_array(9))

        i_rtype=nint(data_array(1))
        i_satid=nint(data_array(2))
        i_instid=nint(data_array(3))
        i_obstyp=nint(data_array(4))
        i_varno=nint(data_array(5))
        i_vertc=nint(data_array(6))

        if (activeonly) then
           if (i_status/=1) cycle ! Only care about active data !
        else
           if (i_status>3) cycle ! Include both active and passive
        endif

        if (i_varno==119) then   ! Radiance
           if (jtype<3) cycle    ! Read radiances only on 3rd
           i_rtype=0
           i_rtype_varno=i_satid*1000+i_instid
          !
        elseif (i_obstyp==3) then
           if (jtype<3) cycle
           i_rtype=0
           i_rtype_varno=i_varno*1000+i_satid
        elseif (i_obstyp==13) then ! Radar
           if (jtype/=2) cycle     ! Radar data in only on 2nd round
           if (i_varno/=29 .and. i_varno/=192 .and. i_varno/=195) cycle
           i_rtype_varno=i_obstyp*1000+i_varno
        else
           if (jtype/=1) cycle  ! Conventional in only on 1st round
           if (i_rtype<0) cycle ! Require sensible reportype for conv and scatterometer data
           i_rtype_varno=i_rtype*1000+i_varno
        endif


! Ignore unsupported data types
!       if (i_rtype==-10) cycle
!       do j_ign=1,n_ign_reportypes
!          if (i_rtype==ign(j_ign)) then
!             icount_ign(j_ign)=icount_ign(j_ign)+1
!             cycle reading
!          endif
!       enddo

!        write (*,*) i_rtype_varno
        call identify(i_rtype_varno,i_vertc,data_id,obtype)
!        write (*,*) data_id, obtype

        if (data_id==-1) then
           write (112,*) '---'
           write (112,'(a)') 'Unknown data type:'
           write (112,'(a)') trim(cline)
           write (112,*) '---'
           cycle reading
        else if (data_id==0) then
           write (112,*) '---'
           write (112,'(a)') 'Unsupported data type:'
           write (112,'(a)') trim(cline)
           write (112,*) '---'
           cycle reading
        endif

        obtype_name(data_id)=obtype
        call vertind(data_id,i_vertc,vert_id)

        if (vert_id>n_vert_ids) then
           write (112,'(a)') 'Vertical index out of range'
           write (112,'(a,i8)') 'data_id=',data_id
           write (112,'(a,i8)') 'i_vertc=',i_vertc
           write (112,'(a,i8)') 'vert_id=',vert_id
           exit
        endif

        if (vert_id<1) cycle

! Save some information

        ! Radar reflectivities not very meaningful for now
        if (data_array(12)<-9.89 .and. data_id==72) cycle reading

        ! Data count
        stat_array(data_id,vert_id,1) = &
             stat_array(data_id,vert_id,1) + 1.0

        ! Mean(obsvalue)
        stat_array(data_id,vert_id,2) = &
             stat_array(data_id,vert_id,2) + data_array(10)
        ! RMS(obsvalue)
        stat_array(data_id,vert_id,3) = &
             stat_array(data_id,vert_id,3) + data_array(10)**2
        ! min(obsvalue)
        stat_array(data_id,vert_id,4) = &
             min(stat_array(data_id,vert_id,4),data_array(10))
        ! max(obsvalue)
        stat_array(data_id,vert_id,5) = &
             max(stat_array(data_id,vert_id,5),data_array(10))

        ! Mean(fg_depar)
        stat_array(data_id,vert_id,6) = &
             stat_array(data_id,vert_id,6) + data_array(11)
        ! RMS(fg_depar)
        stat_array(data_id,vert_id,7) = &
             stat_array(data_id,vert_id,7) + data_array(11)**2
        ! min(fg_depar)
        stat_array(data_id,vert_id,8) = &
             min(stat_array(data_id,vert_id,8),data_array(11))
        ! max(fg_depar)
        stat_array(data_id,vert_id,9) = &
             max(stat_array(data_id,vert_id,9),data_array(11))

        ! Mean(an_depar)
        stat_array(data_id,vert_id,10) = &
             stat_array(data_id,vert_id,10) + data_array(12)
        ! RMS(an_depar)
        stat_array(data_id,vert_id,11) = &
             stat_array(data_id,vert_id,11) + data_array(12)**2
        ! min(an_depar)
        stat_array(data_id,vert_id,12) = &
             min(stat_array(data_id,vert_id,12),data_array(12))
        ! max(an_depar)
        stat_array(data_id,vert_id,13) = &
             max(stat_array(data_id,vert_id,13),data_array(12))

        ! Mean(biascorr_fg)
        stat_array(data_id,vert_id,14) = &
             stat_array(data_id,vert_id,14) + data_array(13)
        ! RMS(biascorr_fg)
        stat_array(data_id,vert_id,15) = &
             stat_array(data_id,vert_id,15) + data_array(13)**2
        ! min(biascorr_fg)
        stat_array(data_id,vert_id,16) = &
             min(stat_array(data_id,vert_id,16),data_array(13))
        ! max(biascorr_fg)
        stat_array(data_id,vert_id,17) = &
             max(stat_array(data_id,vert_id,17),data_array(13))

     enddo reading


! Closing
     close (11)
     write (112,'(a,i8,a)') 'Read ', jline, ' input lines'

  enddo obtype_loop


! Divide (mean, RMS) by data count
  where (stat_array(:,:,1)>0.5) &
       stat_array(:,:,2) = stat_array(:,:,2)/stat_array(:,:,1)
  where (stat_array(:,:,1)>0.5) &
       stat_array(:,:,3) = sqrt(stat_array(:,:,3)/stat_array(:,:,1))

  where (stat_array(:,:,1)>0.5) &
       stat_array(:,:,6) = stat_array(:,:,6)/stat_array(:,:,1)
  where (stat_array(:,:,1)>0.5) &
       stat_array(:,:,7) = sqrt(stat_array(:,:,7)/stat_array(:,:,1))

  where (stat_array(:,:,1)>0.5) &
       stat_array(:,:,10) = stat_array(:,:,10)/stat_array(:,:,1)
  where (stat_array(:,:,1)>0.5) &
       stat_array(:,:,11) = sqrt(stat_array(:,:,11)/stat_array(:,:,1))

  where (stat_array(:,:,1)>0.5) &
       stat_array(:,:,14) = stat_array(:,:,14)/stat_array(:,:,1)
  where (stat_array(:,:,1)>0.5) &
       stat_array(:,:,15) = sqrt(stat_array(:,:,15)/stat_array(:,:,1))


! Report
  do i=1,n_data_ids
     if (all(stat_array(i,:,1)<0.5)) cycle
     if (len_trim(obtype_name(i))>0) then
        write (111,'(a)') ''
        write (111,'(a)') trim(obtype_name(i))
     endif
     write (111,'(a)') 'OmB: Observation minus Background statistics'
     do j=1,n_vert_ids
        if (stat_array(i,j,1)<0.5) cycle
        write (111,'(3(1x,i6),5(1x,f16.8))') &
             i,j,vertind_inv(i,j), stat_array(i,j,1), stat_array(i,j,6:9)
     enddo
     write (111,'(a)') 'OmA: Observation minus Analysis statistics'
     do j=1,n_vert_ids
        if (stat_array(i,j,1)<0.5) cycle
        write (111,'(3(1x,i6),5(1x,f16.8))') &
             i,j,vertind_inv(i,j), stat_array(i,j,1), stat_array(i,j,10:13)
     enddo
     write (111,'(a)') 'Bcor: Bias correction statistics'
     do j=1,n_vert_ids
        if (stat_array(i,j,1)<0.5) cycle
        write (111,'(3(1x,i6),5(1x,f16.8))') &
             i,j,vertind_inv(i,j), stat_array(i,j,1), stat_array(i,j,14:17)
     enddo
  enddo

! write (112,'(a)') 'Unsupported data counts:'
! do j_ign=1,n_ign_reportypes
!    write (112,'(2(1x,i6),a)') ign(j_ign), icount_ign(j_ign), &
!         ' '//trim(c_ign(j_ign))
! enddo
  write (112,'(a)') '---End of header---'

  close (111)
  close (112)

end program obstat



subroutine vertind(data_id,ix,vert_id)
  use params, only : ntemplev, templev, niasich, iasichannels, ncrisch, crischannels
  implicit none
  integer, intent(in) :: data_id, ix
  integer, intent(out) :: vert_id
  integer :: i
  real :: rxx

  vert_id=-1

  if (data_id>=1 .and. data_id<=10) then      ! Conventional surface-based
     vert_id=data_id

  elseif (data_id>=11 .and. data_id<=20) then ! Conventional upper-air
     rxx=ix/100.0
     if (rxx<templev(1)) then
        vert_id=1
     elseif (rxx>templev(ntemplev)) then
        vert_id=ntemplev
     else
        vert_id=1
        do i=2,ntemplev
           if ( abs(rxx-templev(i)) < &
                abs(rxx-templev(vert_id)) ) vert_id=i
        enddo
     endif

  elseif (data_id>=21 .and. data_id<=30) then ! AMSU-A
     vert_id = ix

  elseif (data_id>=31 .and. data_id<=40) then ! MHS
     vert_id = ix

  elseif (data_id>=41 .and. data_id<=50) then ! MWHS2
     vert_id = ix

  elseif (data_id>=51 .and. data_id<=60) then ! IASI
     vert_id=0
     do i=1,niasich
        if (iasichannels(i)==ix) then
           vert_id=i
        endif
     enddo

  elseif (data_id>=61 .and. data_id<=70) then ! Scatterometer wind
     vert_id=data_id

  elseif (data_id>=71 .and. data_id<=80) then ! Radar
     rxx=ix/100.0
     if (rxx<templev(1)) then
        vert_id=1
     elseif (rxx>templev(ntemplev)) then
        vert_id=ntemplev
     else
        vert_id=1
        do i=2,ntemplev
           if ( abs(rxx-templev(i)) < &
                abs(rxx-templev(vert_id)) ) vert_id=i
        enddo
     endif

  elseif (data_id>=81 .and. data_id<=90) then ! CRIS
     vert_id=0
     do i=1,ncrisch
        if (crischannels(i)==ix) then
           vert_id=i
        endif
     enddo

  elseif (data_id>=91 .and. data_id<=100) then ! ATMS
     vert_id = ix

  elseif (data_id>=101 .and. data_id<=110) then ! SEVIRI
     vert_id = ix

  elseif (data_id>=111 .and. data_id<=120) then ! AMV
     rxx=ix/100.0
     if (rxx<templev(1)) then
        vert_id=1
     elseif (rxx>templev(ntemplev)) then
        vert_id=ntemplev
     else
        vert_id=1
        do i=2,ntemplev
           if ( abs(rxx-templev(i)) < &
                abs(rxx-templev(vert_id)) ) vert_id=i
        enddo
     endif

  else
     return

  endif

end subroutine vertind

integer function vertind_inv(data_id,vert_id)
  use params, only : ntemplev, templev, niasich, iasichannels, ncrisch, crischannels
  implicit none
  integer :: data_id, vert_id

  if (data_id>=1 .and. data_id<=10) then      ! Conventional surface-based
     vertind_inv=data_id

  elseif (data_id>=11 .and. data_id<=20) then ! Conventional upper-air
     vertind_inv=templev(vert_id)

  elseif (data_id>=21 .and. data_id<=30) then ! AMSU-A
     vertind_inv = vert_id

  elseif (data_id>=31 .and. data_id<=40) then ! MHS
     vertind_inv = vert_id

  elseif (data_id>=41 .and. data_id<=50) then ! MWHS2
     vertind_inv = vert_id

  elseif (data_id>=51 .and. data_id<=60) then ! IASI
     vertind_inv = iasichannels(vert_id)

  elseif (data_id>=61 .and. data_id<=70) then ! Scatterometer wind
     vertind_inv=data_id

  elseif (data_id>=71 .and. data_id<=80) then ! Radar
     vertind_inv=templev(vert_id)

  elseif (data_id>=81 .and. data_id<=90) then ! CRIS
     vertind_inv = crischannels(vert_id)

  elseif (data_id>=91 .and. data_id<=100) then ! ATMS
     vertind_inv = vert_id

  elseif (data_id>=101 .and. data_id<=110) then ! SEVIRI
     vertind_inv = vert_id

  elseif (data_id>=111 .and. data_id<=120) then ! AMV
     vertind_inv=templev(vert_id)

  else
     vertind_inv=-1
  endif

end function vertind_inv


subroutine identify(i_rtype_varno,i_vertc,data_id,obtype_name)
  implicit none
  integer :: i_rtype_varno, i_vertc
  integer :: data_id
  character(len=100) :: obtype_name

  select case (i_rtype_varno)

! Conventional surface-based observations
  case (16001001,16002001,16005001,16006001,16008001,16074001,16076001,16083001,16084001)
     data_id=1   ! Geopotential
     obtype_name = 'Zs'
  case (16001039,16002039,16005039,16006039,16008039,16074039,16076039,16083039,16084039,16022039,16045039)
     data_id=2   ! 2-meter temperature
     obtype_name = 'T2m'
  case (16001058,16002058,16006058,16074058,16076058,16083058,16084058,16022058,16045058)
     data_id=3   ! 2-meter relative humidity
     obtype_name = 'RH2m'
  case (16001007,16002007,16006007,16008007,16074007,16076007,16083007,16084007)
     data_id=4   ! 2-meter specific humidity
     obtype_name = 'Q2m'
  case (16001041,16002041,16005041,16006041,16008041,16074041,16076041,16083041,16084041)
     data_id=5   ! 10-meter u component
     obtype_name = 'U10m'
  case (16001042,16002042,16005042,16006042,16008042,16074042,16076042,16083042,16084042)
     data_id=6   ! 10-meter v component
     obtype_name = 'V10m'
  case (16001011,16002011,16005011,16006011,16008011,16074011,16076011,16083011,16084011)
     data_id=7   ! surface temperature
     obtype_name = 'Ts'
  case (16012128)
     data_id=8   ! GNSS ZTD
     obtype_name = "Ground-based GNSS"

! Conventional upper-air observations
  case (16029002,16082002,16026002)
     data_id=11 ! AMDAR/AIREP TEMPERATURE
     obtype_name = 'AMDAR T'
  case (16029003,16082003,16026003)
     data_id=12 ! AMDAR/AIREP U COMPONENT
     obtype_name = 'AMDAR U'
  case (16029004,16082004,16026004)
     data_id=13 ! AMDAR/AIREP V COMPONENT
     obtype_name = 'AMDAR V'
  case (16022002,16045002,16019002)
     data_id=14 ! TEMP TEMPERATURE
     obtype_name = 'TEMP T'
  case (16022003,16045003,16019003)
     data_id=15 ! TEMP U COMPONENT
     obtype_name = 'TEMP U'
  case (16022004,16045004,16019004)
     data_id=16 ! TEMP V COMPONENT
     obtype_name = 'TEMP V'
  case (16022007,16045007,16019007)
     data_id=17 ! TEMP SPECIFIC HUMIDITY
     obtype_name = 'TEMP Q'
  case (16022029,16045029,16019029)
     data_id=18 ! TEMP RELATIVE HUMIDITY
     obtype_name = 'TEMP RH'
  case (16022001,16045001,16019001)
     data_id=19 ! TEMP GEOPOTENTIAL
     obtype_name = 'TEMP Z'

!----------------------------------
! Automatic Land SYNOP
! 16001 1 651    ! geopotential
! 16001 7 649    ! specific humidity
! 16001 39 888   ! 2m temperature
! 16001 41 812   ! 10m u component
! 16001 42 812   ! 10m v component
! 16001 58 842   ! 2m rel. humidity
! 16001 91 200   ! total amount of clouds
! 16001 108 651  ! mean sea-level pressure
! 16001 110 651  ! surface pressure
!
! Manual Land SYNOP
! 16002 1 228    ! geopotential
! 16002 7 228    ! specific humidity
! 16002 39 237   ! 2m temperature
! 16002 41 233   ! 10m u component
! 16002 42 233   ! 10m v component
! 16002 58 236   ! 2m rel. humidity
! 16002 91 224   ! total amount of clouds
! 16002 108 228  ! mean sea-level pressure
! 16002 110 228  ! surface pressure
!
! Automatic SHIP
! 16006 1 229    ! geopotential
! 16006 7 11     ! specific humidity
! 16006 11 47    ! surface temperature
! 16006 39 235   ! 2m temperature
! 16006 41 185   ! 10m u component
! 16006 42 185   ! 10m v component
! 16006 58 206   ! 2m rel. humidity
! 16006 91 1     ! total amount of clouds
! 16006 108 11   ! mean sea-level pressure
! 16006 110 229  ! surface pressure
!
! SHIP
! 16008 1 15     ! geopotential
! 16008 11 2     ! surface temperature
! 16008 39 15    ! 2m temperature
! 16008 41 15    ! 10m u component
! 16008 42 15    ! 10m v component
! 16008 58 14    ! 2m rel. humidity
! 16008 91 10    ! total amount of clouds
! 16008 108 2    ! mean sea-level pressure
! 16008 110 15   ! surface pressure
!
! Land TEMP
! 16022 1 515    ! geopotential
! 16022 2 632    ! upper air temperature
! 16022 3 867    ! upper air u component
! 16022 4 867    ! upper air v component
! 16022 7 647    ! specific humidity
! 16022 29 632   ! upper air relative humidity
! 16022 39 15    ! 2m temperature
! 16022 41 15    ! 10m u component
! 16022 42 15    ! 10m v component
! 16022 58 15    ! 2m rel. humidity
!
! BUFR SHIP SYNOP
! 16074 1 362    ! geopotential
! 16074 7 123    ! specific humidity
! 16074 11 109   ! surface temperature
! 16074 39 377   ! 2m temperature
! 16074 41 311   ! 10m u component
! 16074 42 311   ! 10m v component
! 16074 58 343   ! 2m rel. humidity
! 16074 91 12    ! total amount of clouds
! 16074 108 91   ! mean sea-level pressure
! 16074 110 362  ! surface pressure
!
! BUFR LAND SYNOP
! 16076 1 1909   ! geopotential
! 16076 7 1906   ! specific humidity
! 16076 39 2544  ! 2m temperature
! 16076 41 2308  ! 10m u component
! 16076 42 2308  ! 10m v component
! 16076 58 2474  ! 2m rel. humidity
! 16076 80 1483  ! 6hr rain
! 16076 91 1093  ! total amount of clouds
! 16076 92 403   ! 6hr snowfall
! 16076 108 1909 ! mean sea-level pressure
! 16076 110 1909 ! surface pressure
!
! BUFR MOORED BUOYS
! 16083 1 4      ! geopotential
! 16083 39 4     ! 2m temperature
! 16083 110 4    ! surface pressure
!
! BUFR DRIFTING BUOYS
! 16084 1 120    ! geopotential
! 16084 11 166   ! surface temperature
! 16084 41 30    ! 10m u component
! 16084 42 30    ! 10m v component
! 16084 110 120  ! surface pressure
!----------------------------------
!
! WIGOS AMDAR
! 16082



! AMSU-A
  case (4570,4003)
     data_id=21    ! Metop-A AMSU-A
     obtype_name = 'Metop-A AMSU-A'
  case (3570,3003)
     data_id=22    ! Metop-B AMSU-A
     obtype_name = 'Metop-B AMSU-A'
  case (5570,5003)
     data_id=23    ! Metop-C AMSU-A
     obtype_name = 'Metop-C AMSU-A'
  case (206570,206003)
     data_id=24    ! NOAA-15 AMSU-A
     obtype_name = 'NOAA-15 AMSU-A'
  case (209570,209003)
     data_id=25    ! NOAA-18 AMSU-A
     obtype_name = 'NOAA-18 AMSU-A'
  case (223570,223003)
     data_id=26    ! NOAA-19 AMSU-A
     obtype_name = 'NOAA-19 AMSU-A'
!  case (4990)
!     if (i_vertc<6) then
!        data_id=33    ! Metop-C MHS
!        obtype_name = 'Metop-C MHS'
!     elseif (i_vertc<16) then
!        data_id=23    ! Metop-C AMSU-A
!        obtype_name = 'Metop-C AMSU-A'
!     elseif (i_vertc<9000) then
!        data_id=53    ! Metop-C IASI
!        obtype_name = 'Metop-C IASI'
!     endif


! MHS
  case (4203,4015)
     data_id=31    ! Metop-A MHS
     obtype_name = 'Metop-A MHS'
  case (3203,3015)
     data_id=32    ! Metop-B MHS
     obtype_name = 'Metop-B MHS'
  case (5203,5015)
     data_id=33    ! Metop-C MHS
     obtype_name = 'Metop-C MHS'
  case (223203,223015)
     data_id=34    ! NOAA-19 MHS
     obtype_name = 'NOAA-19 MHS'

! MWHS2
  case (521990,522620,522073)
     data_id=41    ! FY3C MWHS2
     obtype_name = 'FY-3C MWHS2'
  case (522990,523620,523953,523073)
     data_id=42    ! FY3D MWHS2
     obtype_name = 'FY-3D MWHS2'

! IASI
  case (4221,4016)
     data_id=51    ! Metop-A IASI
     obtype_name = 'Metop-A IASI'
  case (3221,3016)
     data_id=52    ! Metop-B IASI
     obtype_name = 'Metop-B IASI'
  case (5221,5016)
     data_id=53    ! Metop-C IASI
     obtype_name = 'Metop-C IASI'

! ASCAT
  case (9005125)
     data_id=61
     obtype_name = 'Metop-A ASCAT u component'
  case (9005124)
     data_id=62
     obtype_name = 'Metop-A ASCAT v component'
  case (9007125)
     data_id=63
     obtype_name = 'Metop-B ASCAT u component'
  case (9007124)
     data_id=64
     obtype_name = 'Metop-B ASCAT v component'
  case (9011125)
     data_id=65
     obtype_name = 'Metop-C ASCAT u component'
  case (9011124)
     data_id=66
     obtype_name = 'Metop-C ASCAT v component'

! RADAR
  case (13029)
     data_id=71
     obtype_name = "Radar RH"
  case (13192)
     data_id=72
     obtype_name = "Radar reflectivity"
  case (13195)
     data_id=73
     obtype_name = "Radar Doppler wind"

! CrIS
  case (224620,224027)
     data_id=81    ! S-NPP CrIS
     obtype_name = 'S-NPP CrIS'
  case (225620,225027)
     data_id=82    ! NOAA-20 CrIS
     obtype_name = 'NOAA-20 CrIS'

! ATMS
  case (224621,224019)
     data_id=91    ! S-NPP ATMS
     obtype_name = 'S-NPP ATMS'
  case (225621,225019)
     data_id=92    ! NOAA-20 ATMS
     obtype_name = 'NOAA-20 ATMS'

!
! SEVIRI

  case (57029)
     data_id=101
     obtype_name = 'Meteosat-10 SEVIRI'
  case (74207,73990,70029)
     data_id=102
     obtype_name = 'Meteosat-11 SEVIRI'

!
! AMV
  case (3005)
     data_id=111
     obtype_name = 'Metop-C AMV U component'

  case (4005)
     data_id=112
     obtype_name = 'Metop-C AMV V component'

  case (3852)
     data_id=113
     obtype_name = 'Metop-BC AMV U component'

  case (4852)
     data_id=114
     obtype_name = 'Metop-BC AMV V component'

  case (3070)
     data_id=115
     obtype_name = 'Meteosat-11 AMV U component'

  case (4070)
     data_id=116
     obtype_name = 'Meteosat-11 AMV V component'

  case default
     data_id=-1
  end select

end subroutine identify

subroutine tslot_from_time(i_time,i_tslot)
  implicit none
  integer :: i_time, i_tslot, i_timesec
  integer, parameter :: idelta=10800
  integer :: hh, mm, ss, idum

  idum=i_time
  ss=mod(idum,100)
  idum=idum/100
  mm=mod(idum,100)
  idum=idum/100
  hh=idum

  do while (hh>3)
     hh=hh-3
  enddo  

  idum=hh*3600 + mm*60 + ss
  idum=idum-1860

  i_tslot=idum/1200

end subroutine tslot_from_time
