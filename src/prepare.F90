! Check that input parameters are meaningful.
! Determine timeslots, prepare obsgroup names etc.

subroutine prepare( flag )

  use parameters, only : &
       jpim, maxtimes, interval, niasich, nobgroups, uid, &
       l_uncorrected, l_chisq_conf_intervals, iasi_chanback, &
       niasich_metcoop, iasich_metcoop, satinst

  use array_definitions, only : &
       ntimeslots, idate_array, groupid, &
       cdate_first, cdate_last, &
       expid, conid, &
       levels_temp, levels_rttov, &
       iasi_peakpres

  implicit none
  integer(kind=jpim) :: flag

! Local variables
  character(len=10) :: cdate
  character(len=200) :: filename
  integer(kind=jpim) :: i, id1, id2
  integer(kind=jpim) :: yy, mm, dd, hh, dpm(12)
  logical :: fok
  namelist / satellite_instruments / satinst

  l_uncorrected=.false.
!  l_uncorrected=.true.
  l_chisq_conf_intervals=.false.   ! Consistent with iver & obstat (ECMWF tools)

  flag=0
  do i=1,10
     if (ichar(cdate_first(i:i))<48 .or. ichar(cdate_first(i:i))>57) then
        flag=1
     else if (ichar(cdate_last(i:i))<48 .or. ichar(cdate_last(i:i))>57) then
        flag=2
     end if
     if (flag/=0) exit
  end do
  if (flag/=0) return

  read (cdate_first,*) id1
  read (cdate_last,*) id2

  if (id2<id1) then
    flag=3
    return
  endif

  ntimeslots=0
  i=id1

  do while (id2>=i)

     ntimeslots=ntimeslots+1
     if (ntimeslots>maxtimes) exit
     idate_array(ntimeslots)=i

     hh=mod(i,100)
     i=i/100
     dd=mod(i,100)
     i=i/100
     mm=mod(i,100)
     i=i/100
     yy=i
     dpm(1:12) = (/ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
     if (mod(yy,4)/=0 .or. ( mod(yy,100)==0 .and. mod(yy,400)/=0 ) ) then
        dpm(2)=28
     end if

     hh=hh+interval
     if (hh>23) then
        dd=dd+1
        hh=hh-24
        if (dd>dpm(mm)) then
           mm=mm+1
           dd=1
           if (mm>12) then
              yy=yy+1
              mm=1
           end if
        end if
     end if
     write (cdate,'(i4.4,3i2.2)') yy,mm,dd,hh
     read (cdate,*) i

  end do


! Verify that available OBSTAT files cover the desired time interval in full

  do i=1,ntimeslots

    write (cdate,'(i10.10)') idate_array(i)

    filename= 'depstat_tmp_'//expid//'/'// &
      expid//'_'//cdate//'.txt'
    inquire (file=trim(filename),exist=fok)
    if (.not. fok) then
      flag=4
      write (*,*) trim(filename)
      exit
    endif

    filename= 'depstat_tmp_'//conid//'/'// &
      conid//'_'//cdate//'.txt'
    inquire (file=trim(filename),exist=fok)
    if (.not. fok) then
      flag=5
      exit
    endif
  end do

  if (flag/=0) return

! Define observation groups

  groupid(1)='tempu'
  groupid(2)='tempv'
  groupid(3)='tempuv'
  groupid(4)='tempt'
  groupid(5)='tempq'
  groupid(6)='temprh'
  groupid(7)='airepu'
  groupid(8)='airepv'
  groupid(9)='airepuv'
  groupid(10)='airept'
  groupid(11)='tpau'
  groupid(12)='tpav'
  groupid(13)='tpauv'
  groupid(14)='tat'
  groupid(15)='surface'
  groupid(16)='amsua'
  groupid(17)='atms'
  groupid(18)='mhs'
  groupid(19)='mwhs2'
  groupid(20)='iasilw'
  groupid(21)='iasiwv'
  groupid(22)='iasilw_wn'
  groupid(23)='crislw'
  groupid(24)='criswv'
  groupid(25)='crislw_wn'
  groupid(26)='radarrh'
  groupid(27)='radarrefl'
  groupid(28)='seviri'
  groupid(29)='satobu'
  groupid(30)='satobv'
  groupid(31)='satobuv'
  groupid(32)='geoamvu'
  groupid(33)='geoamvv'
  groupid(34)='geoamvuv'
  groupid(35)='polamvu'
  groupid(36)='polamvv'
  groupid(37)='polamvuv'
  groupid(38)='ma-iasi'
  groupid(39)='mb-iasi'
  groupid(40)='mc-iasi'
  groupid(41)='npp-cris'
  groupid(42)='n20-cris'
  groupid(43)='radarwind'
  groupid(44)='tempz'

! Define peak pressures for IASI channels
  open (11,file='dat/iasi_peak_pressures.dat',status='old')
  read (11,*) iasi_peakpres(1:niasich)
  close (11)
  

! Define TEMP pressure levels
  levels_temp = (/  &
         5.0,  10.0,  15.0,  20.0,  25.0,  30.0,  40.0,  50.0,  65.0,   70.0, &
        85.0, 100.0, 125.0, 150.0, 175.0, 200.0, 225.0, 250.0, 275.0,  300.0, &
       350.0, 400.0, 450.0, 500.0, 600.0, 700.0, 800.0, 850.0, 925.0, 1000.0 /)

! Define RTTOV pressure levels
  levels_rttov = (/ &
    0.10,    0.29,    0.69,    1.42,    2.611, &
    4.407,   6.95,   10.37,   14.81,   20.40, &
   27.26,   35.51,   45.29,   56.73,   69.97, &
   85.18,  102.05,  122.04,  143.84,  167.95, &
  194.36,  222.94,  253.71,  286.60,  321.50, &
  358.28,  396.81,  436.95,  478.54,  521.46, &
  565.54,  610.60,  656.43,  702.73,  749.12, &
  795.09,  839.95,  882.80,  922.46,  957.44, &
  985.88, 1005.43, 1013.25 /)

  iasi_chanback(:)=0
  do i=1,niasich_metcoop
     iasi_chanback(iasich_metcoop(i))=i
  enddo

!  where (iasi_chanback(:)==0) iasi_peakpres(:)=0.0

! Default use of satellite instruments (i.e. use none)
  satinst%noaa15_amsua=.false.
  satinst%noaa16_amsua=.false.
  satinst%noaa17_amsua=.false.
  satinst%noaa18_amsua=.false.
  satinst%noaa19_amsua=.false.
  satinst%metopa_amsua=.false.
  satinst%metopb_amsua=.false.
  satinst%metopc_amsua=.false.
  satinst%noaa18_amsub=.false.
  satinst%noaa19_mhs=.false.
  satinst%metopa_mhs=.false.
  satinst%metopb_mhs=.false.
  satinst%metopc_mhs=.false.
  satinst%fy3c_mwhs2=.false.
  satinst%fy3d_mwhs2=.false.
  satinst%snpp_atms=.false.
  satinst%noaa20_atms=.false.
  satinst%metopa_iasi=.false.
  satinst%metopb_iasi=.false.
  satinst%metopc_iasi=.false.
  satinst%snpp_cris=.false.
  satinst%noaa20_cris=.false.

! Read in namelist if it exists
  filename='nam/satellite_instruments.nml'
  inquire (file=trim(filename),exist=fok)
  if (fok) then
     open (111,file=trim(filename),status='old')
     read (111,nml=satellite_instruments)
     close (111)
  endif
  
end subroutine prepare
