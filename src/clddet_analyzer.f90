!
! (c) Reima.Eresmaa@fmi.fi
!     02022022
!

program clddet_analyzer
  implicit none
  integer, parameter :: n_max_chans=9999
  character(len=20) :: instrument_name
  integer :: iargc
  integer :: i_num_chans, i_chans(n_max_chans), i_window_width
  real :: r_bt_thres
  logical :: l_settings_read_ok, l_channels_filtered, l_sorted
  integer :: count_obs
  real :: lonmin, lonmax, latmin, latmax
  integer :: chanid(n_max_chans)
  real :: meandepar(n_max_chans), meanrank(n_max_chans)
  

!-----------------------------------------------------------------------------
! Find out which instrument settings we are looking for
  if (iargc()<1) then
     write (*,'(a)') 'Usage: ./clddet_analyzer.x INSTRUMENT_NAME'
     stop
  endif
  call getarg(1,instrument_name)

  select case (trim(instrument_name))
  case ("AIRS", "CRIS", "IASI")
     continue
  case default
     write (*,'(/,A,/)') 'Unknown instrument name: '//trim(instrument_name)
     stop
  end select


!-----------------------------------------------------------------------------
! Read the cloud detection channel list and tuning parameter values  
  l_settings_read_ok=.false.
  write (*,'(/,a)',advance='no') &
       'Fetching cloud detection settings for '//trim(instrument_name) //' ...'
  call read_clddet_settings( &
       n_max_chans, &
       instrument_name, i_num_chans, i_chans, i_window_width, r_bt_thres, &
       l_settings_read_ok)
  if (.not. l_settings_read_ok) then
     write (*,'(a)') ' ... FAILED!'
     stop
  else
     write (*,'(a)') ' ... done'
  endif

!-----------------------------------------------------------------------------
! Filter input ASCII file to include only relevant channels
  l_channels_filtered=.false.
  write (*,'(/,a)',advance='no') &
       'Filtering to retain only those channels that are used during ' // &
       'the cloud detection ...'
  call channel_filter( &
       n_max_chans, l_channels_filtered, i_num_chans, i_chans, &
       count_obs, lonmin, lonmax, latmin, latmax, &
       chanid, meandepar, meanrank)
  if (.not. l_channels_filtered) then
     write (*,'(a)') ' ... FAILED!'
     stop
  else
     write (*,'(a)') ' ... done'
  endif
  
!-----------------------------------------------------------------------------
! Sort the data
  l_sorted=.false.
  write (*,'(/,a,/)') &
       'Ranking in vertical and smoothing departure arrays'
  call heapsort_and_smooth( &
       n_max_chans, l_sorted,i_num_chans,i_window_width, &
       r_bt_thres, &
       count_obs, lonmin, lonmax, latmin, latmax, &
       chanid, meandepar, meanrank)
  if (.not. l_sorted) stop

end program clddet_analyzer


subroutine read_clddet_settings( &
     n_max_chans, &
     instrument_name, i_num_chans, i_chans, i_window_width, r_bt_thres, &
     l_settings_read_ok )
  implicit none
  integer :: n_max_chans
  character(len=20) :: instrument_name
  character(len=200) :: filename
  integer :: i_num_chans, i_chans(n_max_chans), i_window_width
  real :: r_bt_thres
  logical :: l_settings_read_ok
  logical :: l_logfile_read, l_source_code_read, l_namelist_read


!-----------------------------------------------------------------------------
! Initial settings
  i_num_chans=0
  i_chans(:)=0
  i_window_width=0
  r_bt_thres=-9.9
  l_settings_read_ok=.false.
  l_logfile_read=.false.
  l_source_code_read=.false.
  l_namelist_read=.false.
  

!-----------------------------------------------------------------------------
! Read the list of cloud detection channels from a Screening log file
! (a required input)
  call read_logfile( &
       n_max_chans, &
       instrument_name, i_num_chans, i_chans, l_logfile_read)
  if (.not. l_logfile_read) then
     write (*,'(/,a)') 'Problem reading the log file'
     return
  endif

!-----------------------------------------------------------------------------
! Read the default cloud detection tuning parameter values from a Fortran
! source code file (a required input).
  call read_source_code( &
       l_source_code_read, &
       instrument_name, i_window_width, r_bt_thres)
  if (.not. l_source_code_read) then
     write (*,'(/,a)') 'Problem reading the source code file'
     return
  endif

!-----------------------------------------------------------------------------
! Read the namelist file if provided, since that may contain parameter
! values that take precedence over default settings.
  call read_namelist_settings( &
       l_namelist_read, &
       instrument_name, i_window_width, r_bt_thres)
  l_settings_read_ok=.true.

end subroutine read_clddet_settings


!-----------------------------------------------------------------------------
! Read the cloud detection channel list from a log file produced by
! HARMONIE Screening task.
!
subroutine read_logfile( &
     n_max_chans, &
     instrument_name, i_num_chans, i_chans, l_logfile_read)
  character(len=20) :: instrument_name  ! AIRS/CRIS/IASI
                                        ! (has been read from command line)
  integer :: i_num_chans, i_chans(n_max_chans)
  logical :: l_logfile_read
  character(len=200) :: filename, line_of_chars
  integer :: io_unit, IOS, isensor, iband, inum
  logical :: l_file_exists, l_in_screening, l_html


  io_unit=333

  l_logfile_read=.false.
  filename='HM_Date.html'
  inquire (file=trim(filename),exist=l_file_exists)
  if (.not. l_file_exists) then
    write (*,'(/,a,/)') 'FATAL: Found no log file '//trim(filename)
    return
  endif

  open (io_unit,file=trim(filename),status='old')
  l_in_screening=.false.
  l_html=.false.
  do
     read (io_unit,'(a)',iostat=IOS) line_of_chars
     if (IOS<0) exit
     if (IOS>0) cycle
     if (index(line_of_chars,'<HTML>')>0) l_html=.true.

! Only care about what is printed out during Screening. However note
! that there may be several reruns of Screening included in the same
! ASCII output file, so continue reading even after the first run is
! completed.
     if ( ( l_html .and. index(line_of_chars,'Screening')>0 .and. &
          index(line_of_chars,'<A NAME=')>0 ) .or. &
          ( .not. l_html .and. index(line_of_chars,'Screening')>0) ) then
        l_in_screening=.true. ! Entering log of a Screening task
     endif

     if ( l_in_screening .and. &
          index(line_of_chars,'This runtime falls within')>0) then
        l_in_screening=.false. ! Exiting log of a Screening task
     endif

     if (.not. l_in_screening) cycle

! Cloud detection channels are listed immediately after a line that goes
! "Sensor XX Band YY has ZZ Cloud Detection Channels"
!
! Sensor IDs are AIRS=11; IASI=16; CRIS=27 (these are defined in the RTTOV code)
!
! Band 1 is (usually) the only one that matters.

     if ( index(line_of_chars,'Cloud Detection Channels')>0) then
        read (line_of_chars,'(15x,i5,14x,i5,12x,i5)') isensor, iband, inum
        if ( iband==1 .and. ( &
             (trim(instrument_name)=='AIRS' .and. isensor==11) .or. &
             (trim(instrument_name)=='IASI' .and. isensor==16) .or. &
             (trim(instrument_name)=='CRIS' .and. isensor==27) ) ) then
           i_num_chans=inum
           read (io_unit,*) i_chans(1:i_num_chans)
           l_logfile_read=.true.
        endif
     endif

  enddo
  close (io_unit)
  
end subroutine read_logfile


!-----------------------------------------------------------------------------
! Read default cloud detection tuning parameter values from the
! HARMONIE source code file cloud_detect_setup.F90 (don't read the
! cloud detection channel list because that has been read in earlier
! from a log file)
!
subroutine read_source_code( &
     l_source_code_read, &
     instrument_name, i_window_width, r_bt_thres)
  implicit none
  logical :: l_source_code_read
  character(len=20) :: instrument_name
  integer :: i_window_width
  real :: r_bt_thres
  character(len=200) :: filename, line_of_chars
  integer :: io_unit, IOS, ipos, jpos
  logical :: l_file_exists, l_inrecord, l_in_lecmwf

  filename='cloud_detect_setup.F90'
  inquire (file=trim(filename),exist=l_file_exists)
  if (.not. l_file_exists) then
     write (*,'(/,a,/)') 'FATAL: Found no source code file '//trim(filename)
     return
  endif

  open (io_unit,file=trim(filename),status='old')
  l_inrecord=.false.
  l_in_lecmwf=.false.
  do
     read (io_unit,'(a)',iostat=IOS) line_of_chars
     if (IOS<0) exit
     if (IOS>0) cycle

! Concentrate only in the section that corresponds to the instrument
! in question. Skip the lines that come before the section, and stop
! reading after the section has been read through.
     if (index(line_of_chars,'Set up '//trim(instrument_name)//' ')>0) then
        l_inrecord=.true.
     endif

     if (l_inrecord .and. index(line_of_chars,'CASE')>0) then
        l_inrecord=.false.
        exit
     endif

     if (.not. l_inrecord) cycle

! Do not care about those lines that are specific to ECMWF     
     if (index(line_of_chars,'LECMWF')>0) then
        l_in_lecmwf=.true.
     endif

     if (l_in_lecmwf .and. index(line_of_chars,'ELSE')>0) then
        l_in_lecmwf=.false.
     endif

     if (l_in_lecmwf) cycle

! Boxcar-window averaging width
     if (i_window_width<1 .and. index(line_of_chars,'N__WINDOW_WIDTH')>0) then
        ipos=index(line_of_chars,'(/')+2
        jpos=index(line_of_chars,',')-1
        if (ipos<1 .or. jpos<1) cycle
        read (line_of_chars(ipos:jpos),*) i_window_width
     endif

! Brightness temperature O-B departure threshold
     if (r_bt_thres<0.0 .and. index(line_of_chars,'R__BT_THRESHOLD')>0) then
        ipos=index(line_of_chars,'(/')+2
        jpos=index(line_of_chars,',')-1
        if (ipos<1 .or. jpos<1) cycle
        read (line_of_chars(ipos:jpos),*) r_bt_thres
     endif

  enddo

  close (io_unit)
  l_source_code_read=.true.

end subroutine read_source_code


!-----------------------------------------------------------------------------
! Search for overriding specifications in a namelist file if one is
! provided.
!
subroutine read_namelist_settings( &
     l_namelist_read, &
     instrument_name, i_window_width, r_bt_thres)
  implicit none
  logical :: l_namelist_read
  character(len=20) :: instrument_name
  integer :: i_window_width
  real :: r_bt_thres
  character(len=200) :: filename, line_of_chars
  integer :: io_unit, IOS, ipos, jpos
  logical :: l_file_exists

  filename=trim(instrument_name)//'_CLDDET.NL'
  inquire (file=trim(filename),exist=l_file_exists)
  if (.not. l_file_exists) then
     write (*,'(/,a,/)') 'Warning: Found no cloud detection namelist file ' // &
          trim(filename)
     return
  endif

  open (io_unit,file=trim(filename),status='old')
  do
     read (io_unit,'(a)',iostat=IOS) line_of_chars
     if (IOS<0) exit
     if (IOS>0) cycle

! Boxcar-window averaging width
     if ( index(line_of_chars,'WIDTH')>0 .or. &
          index(line_of_chars,'Width')>0 .or. &
          index(line_of_chars,'width')>0 ) then
        ipos=index(line_of_chars,'=')+1
        jpos=index(line_of_chars,',')-1
        if (jpos<1) jpos=len_trim(line_of_chars)
        if (ipos<1 .or. jpos<1) cycle
        read (line_of_chars(ipos:jpos),*) i_window_width
     endif

! Brightness temperature O-B departure threshold
     if ( index(line_of_chars,'BT_THRES')>0 .or. &
          index(line_of_chars,'BT_Thres')>0 .or. &
          index(line_of_chars,'bt_thres')>0 ) then
        ipos=index(line_of_chars,'=')+1
        jpos=index(line_of_chars,',')-1
        if (jpos<1) jpos=len_trim(line_of_chars)
        if (ipos<1 .or. jpos<1) cycle
        read (line_of_chars(ipos:jpos),*) r_bt_thres
     endif

  enddo

  close (io_unit)
  l_namelist_read=.true.

end subroutine read_namelist_settings


!-----------------------------------------------------------------------------
! Remove unnecessary channels from input
subroutine channel_filter( &
     n_max_chans, l_channels_filtered, i_num_chans, i_chans, &
     count_obs, lonmin, lonmax, latmin, latmax, &
     chanid_out, meandepar, meanrank )
  implicit none
  integer :: n_max_chans
  logical :: l_channels_filtered
  integer :: i_num_chans, i_chans(n_max_chans)
  integer :: i_chans_back(n_max_chans)
  integer :: i, io_unit, oi_unit, IOS, satid, chanid, icld
  real :: lon, lat, depar, rank
  character(len=200) :: filename, filename_out, line_of_chars
  logical :: file_exists
  integer :: count_obs
  real :: lonmin, lonmax, latmin, latmax, lon_prev, lat_prev
  integer :: chanid_out(n_max_chans), chancount(n_max_chans)
  real :: meandepar(n_max_chans), meanrank(n_max_chans)
  
  
  io_unit=444
  oi_unit=555

  lonmin=9.9e9
  lonmax=-9.9e9
  latmin=9.9e9
  latmax=-9.9e9
  count_obs=0

  lon_prev=9.9e9
  lat_prev=9.9e9

  chancount(:)=0
  chanid_out(:)=0
  meandepar(:)=0.0
  meanrank(:)=0.0

  i_chans_back(:)=0
  do i=1,i_num_chans
     i_chans_back(i_chans(i))=i
  enddo

  filename='clddet_ascii.dat'
  filename_out='clddet_filtered.dat'

  inquire (file=trim(filename),exist=file_exists)
  if (.not. file_exists) then
     write (*,'(/,A,/)') 'FATAL: Found no input ASCII file '//trim(filename)
     return
  endif

  open (io_unit,file=trim(filename),status='old')
  open (oi_unit,file=trim(filename_out),status='replace')
  do
     read (io_unit,'(a)',iostat=IOS) line_of_chars
     if (IOS<0) exit
     if (IOS>0) cycle
     if (index(line_of_chars,'@')>0) cycle
     read (line_of_chars,*,iostat=IOS) satid, lon, lat, chanid, depar, rank
     if (i_chans_back(chanid)==0 .or. IOS>0) cycle
     if (satid==4) cycle
     if (abs(lon-lon_prev)>.0e-2 .or. abs(lat-lat_prev)>1.0e-2) then
        count_obs=count_obs+1
        if (lon<lonmin) lonmin=lon
        if (lon>lonmax) lonmax=lon
        if (lat<latmin) latmin=lat
        if (lat>latmax) latmax=lat
     endif
     write (oi_unit,'(A)') trim(line_of_chars)
     lon_prev=lon
     lat_prev=lat
     chanid_out(chanid)=chanid
     chancount(chanid)=chancount(chanid)+1
     meandepar(chanid)=meandepar(chanid)+depar
     meanrank(chanid)=meanrank(chanid)+rank
  enddo
  close (io_unit)
  close (oi_unit)

  where (chancount(:)>0) meandepar(:)=meandepar(:)/chancount(:)
  where (chancount(:)>0) meanrank(:)=meanrank(:)/chancount(:)
  l_channels_filtered=.true.

end subroutine channel_filter

!-----------------------------------------------------------------------------
! Sort the data according to channel height assignments (rank_cld) and
! apply the boxcar filtering
subroutine heapsort_and_smooth( &
     n_max_chans, l_sorted,i_num_chans,i_window_width, &
     r_bt_thres, &
     count_obs, lonmin, lonmax, latmin, latmax, &
     chanid_in, meandepar, meanrank)
  implicit none
  integer :: n_max_chans
  logical :: l_sorted
  integer :: i_num_chans, i_window_width, count_obs
  real :: r_bt_thres, lonmin, lonmax, latmin, latmax
  integer :: chanid_in(n_max_chans), chancount(n_max_chans)
  real :: meandepar(n_max_chans), meanrank(n_max_chans)
  integer :: jobs, satid, chanid(i_num_chans), cldflag(i_num_chans)
  real :: lon, lat, depar(i_num_chans), rank(i_num_chans), smooth(i_num_chans)
  integer :: i, j, io_unit, oi_unit, IOS
  character(len=200) :: filename, filename_out
  logical :: file_exists


  io_unit=666
  oi_unit=777

  filename='clddet_filtered.dat'
  filename_out='clddet_sorted_smoothed.dat'

  inquire (file=trim(filename),exist=file_exists)
  if (.not. file_exists) then
     write (*,'(/,A,/)') 'FATAL: Found no input ASCII file '//trim(filename)
     return
  endif

  open (io_unit,file=trim(filename),status='old')
  open (oi_unit,file=trim(filename_out),status='replace')
  write (oi_unit,'(a,i8)')   'Number of observations: ', count_obs
  write (oi_unit,'(a,i8)')   'Number of channels:     ', i_num_chans
  write (oi_unit,'(2(a,f8.3))') &
       'Observation longitudes range from ', lonmin, ' to ', lonmax
  write (oi_unit,'(2(a,f8.3))') &
       'Observation latitudes range from ', latmin, ' to ', latmax

  write (oi_unit,'(a)') ''
  write (oi_unit,'(a)') 'BT Threshold information for plotting:'
  write (oi_unit,'(a,2(1x,f8.3))') 'bt ', 1.0, -r_bt_thres
  write (oi_unit,'(a,2(1x,f8.3))') 'bt ', 1.0*i_num_chans, -r_bt_thres
  write (oi_unit,'(a)') 'bt '
  write (oi_unit,'(a,2(1x,f8.3))') 'bt ', 1.0,  r_bt_thres
  write (oi_unit,'(a,2(1x,f8.3))') 'bt ', 1.0*i_num_chans,  r_bt_thres
  write (oi_unit,'(a)') ''

  jobs=0

! Process mean departure spectrum first
  j=0
  do i=1,n_max_chans
     if (chanid_in(i)<1) cycle
     j=j+1
     chanid(j)=chanid_in(i)
     depar(j)=meandepar(i)
     rank(j)=meanrank(i)
     cldflag(j)=0
  enddo
  call sorting(j,chanid,depar,rank,cldflag)
  call smoothing(j,depar,smooth,i_window_width)
  do i=1,j
     write (oi_unit, &
          '(1x,i3,2(1x,f7.4),2(1x,i4),2(1x,f14.8),1x,f14.8,1x,i5,1x,i1)') &
          0, 0.0, 0.0, chanid(i), i, &
          depar(i), rank(i), smooth(i), 0, cldflag(i)
  enddo

  outer: do
! Read input at one observed location
     inner: do i=1,i_num_chans
        read (io_unit,*,iostat=IOS) &
             satid, lon, lat, chanid(i), depar(i), rank(i), cldflag(i)
        if (IOS<0) exit outer
        if (IOS>0) then
           write (*,'(a)') 'FATAL: Error in reading input data in HEAPSORT'
           return
        endif
     enddo inner
     jobs=jobs+1
     ! Sort the data
     call sorting(i_num_chans,chanid,depar,rank,cldflag)
     call smoothing(i_num_chans,depar,smooth,i_window_width)
     ! Write output
     do i=1,i_num_chans
        write (oi_unit, &
! increased format for lon to include lon <= -10.0000
!             '(1x,i3,2(1x,f7.4),2(1x,i4),2(1x,f14.8),1x,f14.8,1x,i5,1x,i1)') &
             '(1x,i3,1x,f8.4,1x,f7.4,2(1x,i4),2(1x,f14.8),1x,f14.8,1x,i5,1x,i1)') &
             satid, lon, lat, chanid(i), i, &
             depar(i), rank(i), smooth(i), jobs, cldflag(i)
     enddo
  enddo outer
  close (io_unit)
  close (oi_unit)

  l_sorted=.true.
  
end subroutine heapsort_and_smooth


subroutine sorting(n,chanid,depar,rank,cldflag)
  implicit none
  integer :: n, chanid(n), cldflag(n)
  real :: depar(n), rank(n)
  integer :: i, j, idummy
  real :: rdummy

  
  do i=1,n-1
     do j=i+1,n
        if (rank(i)>rank(j)) then
           idummy=chanid(i)
           chanid(i)=chanid(j)
           chanid(j)=idummy

           idummy=cldflag(i)
           cldflag(i)=cldflag(j)
           cldflag(j)=idummy

           rdummy=depar(i)
           depar(i)=depar(j)
           depar(j)=rdummy

           rdummy=rank(i)
           rank(i)=rank(j)
           rank(j)=rdummy
        endif
     enddo
  enddo

end subroutine sorting

subroutine smoothing(i_num_chans,depar,smooth,i_window_width)
  implicit none
  integer :: i_num_chans, i_window_width
  real :: depar(i_num_chans), smooth(i_num_chans)
  integer :: i, j, i_num
  real :: rcum

  do i=1,i_num_chans
     rcum=0.0
     i_num=0
     do j=i-i_window_width/2,i+i_window_width/2
        if (j<1 .or. j>i_num_chans) cycle
        i_num=i_num+1
        rcum=rcum+depar(j)
     enddo
     smooth(i)=rcum/i_num
  enddo
end subroutine smoothing
