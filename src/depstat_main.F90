program depstat

  use parameters, only : &
    jpim, jprb, &
    nobgroups, ndomains, nlevels_small, nlevels_large, nstat, interval, uid

  use array_definitions, only : &
    expid, conid, &
    cdate_first, cdate_last, &
    groupid, &
    nlevels, &
    ntimeslots, idate_array, &
    statarray, hemispheric, global, &
    statistic, stat95l, stat95u, stat99l, stat99u, &
    stat95_bg, stat95_an, &
    irank

  implicit none

! Local variables

  integer :: iargc
  integer :: flag
  integer :: jtimeslot
  character(len=5) :: clarge
  character(len=1) :: crank

!------------------------------------------------------------------------------
! Preparations

  if (iargc()<4) stop &
       'Usage depstat_main experiment_id control_id first_time ' // &
       'last_time large rank'
  call getarg(1,expid)
  call getarg(2,conid)
  call getarg(3,cdate_first)
  call getarg(4,cdate_last)

  clarge='none'
  if (iargc()>4) call getarg(5,clarge)

  if (clarge=='large' .or. clarge=='LARGE') then
     nlevels=nlevels_large
  else
     nlevels=nlevels_small
  end if

  crank='0'
  if (iargc()>5) call getarg(6,crank)
  read (crank,*) irank

  open (11,file='uid.txt',status='old')
  read (11,*) uid
  close (11)

  call prepare(flag)

  select case (flag)
  case (1,2)
    write (*,'(a)') 'Problem with input dates'
  case (3)
    write (*,'(a)') 'Last date must not be before first date'
  case (4)
    write (*,'(a)') 'Some (experiment) input files are missing'
  case (5)
    write (*,'(a)') 'Some (control) input files are missing'
  case default
    continue
  end select

  open (11,file='number_of_days.txt',status='replace')
  if (flag==0) then
    write (11,'(i3)') ntimeslots*interval/24
  else
    write (11,'(i3)') -1
  endif
  close (11)

  if (flag/=0) stop

!------------------------------------------------------------------------------
! Retrieve statistics for each timeslot

  allocate (statarray(nobgroups,ntimeslots,4,ndomains+1,nlevels,nstat))
  allocate (hemispheric(nobgroups,4,ndomains,nlevels,nstat))
  allocate (global(nobgroups,4,nlevels,nstat))

  open (999,file='depstat.log',status='replace')
  do jtimeslot=1,ntimeslots

    if (mod(jtimeslot,10)==0) write (*,*) idate_array(jtimeslot)
    hemispheric(:,:,:,:,:)=0.0_jprb
    global(:,:,:,:)=0.0_jprb

    call fetchOneTime(jtimeslot)

    statarray(:,jtimeslot,:,1:ndomains,:,:)=hemispheric(:,:,:,:,:)
    statarray(:,jtimeslot,:,ndomains+1,:,:)=global(:,:,:,:)

  end do
  close (999)

!------------------------------------------------------------------------------
! Average over all timeslots and report

  allocate (statistic(nobgroups,4,ndomains+1,nlevels,nstat))
  allocate (stat95l(nobgroups,4,ndomains+1,nlevels,nstat))
  allocate (stat95u(nobgroups,4,ndomains+1,nlevels,nstat))
  allocate (stat99l(nobgroups,4,ndomains+1,nlevels,nstat))
  allocate (stat99u(nobgroups,4,ndomains+1,nlevels,nstat))
  allocate (stat95_bg(nobgroups,ndomains+1,nlevels))
  allocate (stat95_an(nobgroups,ndomains+1,nlevels))

  call average
  call report

  deallocate (hemispheric)
  deallocate (global)
  deallocate (statistic)
  deallocate (stat95l)
  deallocate (stat95u)
  deallocate (stat99l)
  deallocate (stat99u)
  deallocate (stat95_bg)
  deallocate (stat95_an)

!------------------------------------------------------------------------------
! Deallocate and close the program

  deallocate (statarray)

end program depstat
