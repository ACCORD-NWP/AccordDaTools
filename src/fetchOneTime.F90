subroutine fetchOneTime(jtimeslot)

  use parameters, only : &
      jpim, jprb, &
      nobgroups, ndomains, nsensors, nstat, nlevels_large, &
      uid
  use array_definitions, only : &
      expid, conid, &
      idate_array, iasi_peakpres, &
      hemispheric, global, nlevels

  implicit none

  integer(kind=jpim) :: jtimeslot

! Local variables
  character(len=10) :: cdate
  character(len=100) :: csensor, cdomain
  character(len=200) :: exp_file, con_file, cline

  integer(kind=jpim) :: jrun, jsensor, jgroup, jdomain, jlevel, jgroup_array(4), jposition
  integer(kind=jpim) :: lok, iend, ibeg
  integer(kind=jpim) :: offset
  integer(kind=jpim) :: channel
  integer(kind=jpim) :: Vcoordinate

  logical :: bg, an, bc, inrecord, inheader

  real(kind=jprb), dimension(nobgroups,4,ndomains,nlevels,nsensors,nstat) :: array
  real(kind=jprb), dimension(6) :: linedata
  integer(kind=jpim), dimension(3) :: idum
  real(kind=jprb) :: oldcount, oldmean, oldrms, cfactor
  real(kind=jprb) :: newcount, newmean, newrms, newsdev


!---------------------------------------------------------------------------------------
! Some preparations

  write (cdate,'(i10.10)') idate_array(jtimeslot)
  exp_file= 'depstat_tmp_'//expid//'/'// &
       expid//'_'//cdate//'.txt'
  con_file= 'depstat_tmp_'//conid//'/'// &
       conid//'_'//cdate//'.txt'


!---------------------------------------------------------------------------------------
! Read input data

  array(:,:,:,:,:,:)=0.0

  do jrun=1,2

     if (jrun==1) then
        open (11,file=trim(exp_file),status='old')
     else if (jrun==2) then
        open (11,file=trim(con_file),status='old')
     end if

     csensor=''
!     bg=.false.
     bg=.true.
     an=.false.
     bc=.false.
     inrecord=.false.
     inheader=.false.

     
     do

        read (11,'(a)',iostat=lok) cline
        if (lok<0) exit

        if (cline(1:8)=='---Start') then
           inheader=.true.
           cycle
        elseif (cline(1:6)=='---End') then
           inheader=.false.
           cycle
        endif

        if (inheader) cycle

        
! Which departures?
        if (cline(1:4)=='OmB:') then
           bg=.true.
           an=.false.
           bc=.false.
           cycle
        elseif (cline(1:4)=='OmA:') then
           bg=.false.
           an=.true.
           bc=.false.
           cycle
        elseif (cline(1:5)=='Bcor:') then
           bg=.false.
           an=.false.
           bc=.true.
           cycle
        end if

! Which instrument we are dealing with?
        if (cline(1:1)/=' ' .and. cline(1:4)/='Read') then
           iend=LEN_TRIM(cline)
           ibeg=1
           read (cline(ibeg:iend),'(a)') csensor
           call getSensorNumber(csensor,jsensor,jgroup_array,jposition)

           inrecord=.true.
           cycle
        endif

        jdomain=1  ! Only one domain for now ...

        if (len_trim(cline)==0 .and. inrecord) then
           inrecord=.false.
        endif
              
        if (.not. inrecord) cycle
        if ( (.not. bg) .and. (.not. an) .and. (.not. bc) ) cycle
        if ( jsensor<1 .or. jsensor>nsensors .or. &
             jgroup_array(1)<1 .or. jgroup_array(1)>nobgroups) then
           write (999,'(a)') trim(csensor)
           cycle
        end if

        do jgroup=1,4

          if (jgroup_array(jgroup)<1_jpim .or. jgroup_array(jgroup)>nobgroups) cycle
          if (jgroup_array(jgroup)==15) then
             linedata(1)=jposition
             read (cline,*,iostat=lok) idum(1:3), linedata(2:6)
          else
             read (cline,*,iostat=lok) idum(1:2), linedata(1:6)
          end if
          if (lok>0) cycle

          offset=0

          if (jgroup_array(jgroup)==20 .and. linedata(1)>2300.0 .and. linedata(1)<=5600.0) then
             offset=1   ! IASI WV BAND
          else if (jgroup_array(jgroup)==23 .and. linedata(1)>713.0 .and. linedata(1)<=1578.0) then
             offset=1   ! CRIS WV BAND
          end if

          if (jgroup_array(jgroup)>=20 .and. jgroup_array(jgroup)<=21) then      ! IASI: Level from peak pressure
             channel=nint(linedata(1))
             if (iasi_peakpres(channel)>1.0e-4) then
                jlevel=Vcoordinate(iasi_peakpres(channel),jgroup_array(jgroup))
             else
                jlevel=0
             endif

          else if (jgroup_array(jgroup)>=23 .and. jgroup_array(jgroup)<=24) then ! CRIS: Level from peak pressure
                                                     ! Note that the nearest IASI channel will be used!
             call cris_ch2wn(linedata(1))            ! Channel number to wavenumber
             call iasi_wn2ch(linedata(1),channel)    ! Wavenumber to IASI channel
             if (iasi_peakpres(channel)>1.0e-4) then
                jlevel=Vcoordinate(iasi_peakpres(channel),jgroup_array(jgroup))
             else
                jlevel=0
             endif

          else if (jgroup_array(jgroup)==22_jpim) then    ! IASI LW-band Wavenumber space
             linedata(1)=644.75+.25*linedata(1)      ! Channel index to wavenumber
             jlevel=Vcoordinate(linedata(1),jgroup_array(jgroup))
!             if (nlevels/=nlevels_large) jlevel=0_jpim

          else if (jgroup_array(jgroup)==25_jpim) then    ! CrIS LW-band Wavenumber space
             if (linedata(1)<713.5) then
                linedata(1)=649.375+.625*linedata(1)    ! Channel index to wavenumber
             elseif (linedata(1)<1578.5) then
                linedata(1)=1209.375+.625*(linedata(1)-713.0)
             else
                linedata(1)=2154.375+.625*(linedata(1)-1578.0)
             endif
             jlevel=Vcoordinate(linedata(1),jgroup_array(jgroup))
!             if (nlevels/=nlevels_large) jlevel=0_jpim

          else if (jgroup_array(jgroup)>=38_jpim .and. &  ! IASI individual
                   jgroup_array(jgroup)<=40_jpim) then
             linedata(1)=644.75+.25*linedata(1)      ! Channel index to wavenumber
             jlevel=Vcoordinate(linedata(1),jgroup_array(jgroup))
!             if (nlevels/=nlevels_large) jlevel=0_jpim

          else if (jgroup_array(jgroup)>=41_jpim .and. &  ! CrIS individual
                   jgroup_array(jgroup)<=42_jpim) then
             if (linedata(1)<713.5) then
                linedata(1)=649.375+.625*linedata(1)         ! Channel index to wavenumber
             else
                linedata(1)=1210.00+1.25*(linedata(1)-714.0) ! Channel index to wavenumber
             endif
             jlevel=Vcoordinate(linedata(1),jgroup_array(jgroup))
!             if (nlevels/=nlevels_large) jlevel=0_jpim

          else
             jlevel=Vcoordinate(linedata(1),jgroup_array(jgroup))

          end if

          if (jlevel<1 .or. jlevel>nlevels) cycle

          if (bg) then
             oldcount=array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,1)
             oldmean =array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,2)
             oldrms  =array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,3)
             newcount=linedata(2)
             newmean =linedata(3)
             newrms  =linedata(4)

             newmean= &
                  (oldcount*oldmean+newcount*newmean)/ &
                  (oldcount+newcount)
             newrms =&
                  sqrt( (oldcount*oldrms**2 + newcount*newrms**2)/ &
                  (oldcount+newcount) )
             if (newcount<2) then
                cfactor=1.0_jprb
             else
                cfactor = newcount*1.0_jprb/(newcount-1)
             endif

             newsdev = sqrt(cfactor*(newrms**2-newmean**2))
             newcount=oldcount+newcount

             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,1)= &
                  newcount
             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,2)= &
                  newmean
             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,3)= &
                  newrms
             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,4)= &
                  newsdev

          else if (an) then
             oldcount=array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,5)
             oldmean =array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,6)
             oldrms  =array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,7)
             newcount=linedata(2)
             newmean =linedata(3)
             newrms  =linedata(4)

             newmean= &
                  (oldcount*oldmean+newcount*newmean)/ &
                  (oldcount+newcount)
             newrms =&
                  sqrt( (oldcount*oldrms**2 + newcount*newrms**2)/ &
                  (oldcount+newcount) )
             if (newcount<2) then
                cfactor=1.0_jprb
             else
                cfactor = newcount*1.0_jprb/(newcount-1)
             endif
             newsdev = sqrt(cfactor*(newrms**2-newmean**2))
             newcount=oldcount+newcount

             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,5)= &
                  newcount
             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,6)= &
                  newmean
             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,7)= &
                  newrms
             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,8)= &
                  newsdev

          else if (bc) then
             oldcount=array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,9)
             oldmean =array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,10)
             oldrms  =array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,11)
             newcount=linedata(2)
             newmean =linedata(3)
             newrms  =linedata(4)

             newmean= &
                  (oldcount*oldmean+newcount*newmean)/ &
                  (oldcount+newcount)
             newrms =&
                  sqrt( (oldcount*oldrms**2 + newcount*newrms**2)/ &
                  (oldcount+newcount) )
             if (newcount<2) then
                cfactor=1.0_jprb
             else
                cfactor = newcount*1.0_jprb/(newcount-1)
             endif
             newsdev = sqrt(cfactor*(newrms**2-newmean**2))
             newcount=oldcount+newcount

             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,9)= &
                  newcount
             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,10)= &
                  newmean
             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,11)= &
                  newrms
             array(jgroup_array(jgroup)+offset,jrun,jdomain,jlevel,jsensor,12)= &
                  newsdev

          end if

       end do

     end do

  end do
  close (11)


!---------------------------------------------------------------------------------------
! Combine data from possibly many sensors

  hemispheric(:,:,:,:,:)=0.0

  do jgroup=1,nobgroups
     do jrun=1,2
        domainloop: do jdomain=1,ndomains
           do jlevel=1,nlevels

              do jsensor=1,nsensors

                 if (array(jgroup,jrun,jdomain,jlevel,jsensor,1)>0.5_jprb) then
                    hemispheric(jgroup,jrun,jdomain,jlevel,1)= &
                         hemispheric(jgroup,jrun,jdomain,jlevel,1) + &  ! OMB COUNT
                         array(jgroup,jrun,jdomain,jlevel,jsensor,1)
                    hemispheric(jgroup,jrun,jdomain,jlevel,2)= &
                         hemispheric(jgroup,jrun,jdomain,jlevel,2) + &  ! OMB MEAN
                         array(jgroup,jrun,jdomain,jlevel,jsensor,1)* &
                         array(jgroup,jrun,jdomain,jlevel,jsensor,2)
                    hemispheric(jgroup,jrun,jdomain,jlevel,3)= &
                         hemispheric(jgroup,jrun,jdomain,jlevel,3) + &  ! OMB RMS
                         array(jgroup,jrun,jdomain,jlevel,jsensor,1)* &
                         array(jgroup,jrun,jdomain,jlevel,jsensor,3)**2
                 end if

                 if (array(jgroup,jrun,jdomain,jlevel,jsensor,5)>0.5_jprb) then
                    hemispheric(jgroup,jrun,jdomain,jlevel,5)= &
                         hemispheric(jgroup,jrun,jdomain,jlevel,5) + &  ! OMA COUNT
                         array(jgroup,jrun,jdomain,jlevel,jsensor,5)
                    hemispheric(jgroup,jrun,jdomain,jlevel,6)= &
                         hemispheric(jgroup,jrun,jdomain,jlevel,6) + &  ! OMA MEAN
                         array(jgroup,jrun,jdomain,jlevel,jsensor,5)* &
                         array(jgroup,jrun,jdomain,jlevel,jsensor,6)
                    hemispheric(jgroup,jrun,jdomain,jlevel,7)= &
                         hemispheric(jgroup,jrun,jdomain,jlevel,7) + &  ! OMA RMS
                         array(jgroup,jrun,jdomain,jlevel,jsensor,5)*&
                         array(jgroup,jrun,jdomain,jlevel,jsensor,7)**2
                 end if

                 if (array(jgroup,jrun,jdomain,jlevel,jsensor,9)>0.5_jprb) then
                    hemispheric(jgroup,jrun,jdomain,jlevel,9)= &
                         hemispheric(jgroup,jrun,jdomain,jlevel,9) + &  ! BCOR COUNT
                         array(jgroup,jrun,jdomain,jlevel,jsensor,9)
                    hemispheric(jgroup,jrun,jdomain,jlevel,10)= &
                         hemispheric(jgroup,jrun,jdomain,jlevel,10) + &  ! BCOR MEAN
                         array(jgroup,jrun,jdomain,jlevel,jsensor,9)* &
                         array(jgroup,jrun,jdomain,jlevel,jsensor,10)
                    hemispheric(jgroup,jrun,jdomain,jlevel,11)= &
                         hemispheric(jgroup,jrun,jdomain,jlevel,11) + &  ! BCOR RMS
                         array(jgroup,jrun,jdomain,jlevel,jsensor,9)*&
                         array(jgroup,jrun,jdomain,jlevel,jsensor,11)**2
                 end if

              end do

              if (hemispheric(jgroup,jrun,jdomain,jlevel,1)>0.5_jprb) then
                 hemispheric(jgroup,jrun,jdomain,jlevel,2)= &
                      hemispheric(jgroup,jrun,jdomain,jlevel,2)/ &
                      hemispheric(jgroup,jrun,jdomain,jlevel,1)
                 hemispheric(jgroup,jrun,jdomain,jlevel,3)= &
                      sqrt(hemispheric(jgroup,jrun,jdomain,jlevel,3)/ &
                      hemispheric(jgroup,jrun,jdomain,jlevel,1))
                 if (hemispheric(jgroup,jrun,jdomain,jlevel,1)<1.5_jprb) then
                   cfactor=1.0_jprb
                 else
                   cfactor = hemispheric(jgroup,jrun,jdomain,jlevel,1) / &
                            (hemispheric(jgroup,jrun,jdomain,jlevel,1)-1)
                 endif
                 hemispheric(jgroup,jrun,jdomain,jlevel,4)= &
                      sqrt(cfactor* &
                     (hemispheric(jgroup,jrun,jdomain,jlevel,3)**2-&
                      hemispheric(jgroup,jrun,jdomain,jlevel,2)**2) )
              end if

              if (hemispheric(jgroup,jrun,jdomain,jlevel,5)>0.5_jprb) then
                 hemispheric(jgroup,jrun,jdomain,jlevel,6)= &
                      hemispheric(jgroup,jrun,jdomain,jlevel,6)/ &
                      hemispheric(jgroup,jrun,jdomain,jlevel,5)
                 hemispheric(jgroup,jrun,jdomain,jlevel,7)= &
                      sqrt(hemispheric(jgroup,jrun,jdomain,jlevel,7)/ &
                      hemispheric(jgroup,jrun,jdomain,jlevel,5))
                 if (hemispheric(jgroup,jrun,jdomain,jlevel,5)<1.5_jprb) then
                   cfactor=1.0_jprb
                 else
                   cfactor = hemispheric(jgroup,jrun,jdomain,jlevel,5) / &
                            (hemispheric(jgroup,jrun,jdomain,jlevel,5)-1)
                 endif
                 hemispheric(jgroup,jrun,jdomain,jlevel,8)= &
                      sqrt(cfactor* &
                     (hemispheric(jgroup,jrun,jdomain,jlevel,7)**2-&
                      hemispheric(jgroup,jrun,jdomain,jlevel,6)**2) )
              end if

              if (hemispheric(jgroup,jrun,jdomain,jlevel,9)>0.5_jprb) then
                 hemispheric(jgroup,jrun,jdomain,jlevel,10)= &
                      hemispheric(jgroup,jrun,jdomain,jlevel,10)/ &
                      hemispheric(jgroup,jrun,jdomain,jlevel,9)
                 hemispheric(jgroup,jrun,jdomain,jlevel,11)= &
                      sqrt(hemispheric(jgroup,jrun,jdomain,jlevel,11)/ &
                      hemispheric(jgroup,jrun,jdomain,jlevel,9))
                 if (hemispheric(jgroup,jrun,jdomain,jlevel,9)<1.5_jprb) then
                   cfactor=1.0_jprb
                 else
                   cfactor = hemispheric(jgroup,jrun,jdomain,jlevel,9) / &
                            (hemispheric(jgroup,jrun,jdomain,jlevel,9)-1)
                 endif
                 hemispheric(jgroup,jrun,jdomain,jlevel,12)= &
                      sqrt(cfactor* &
                     (hemispheric(jgroup,jrun,jdomain,jlevel,11)**2-&
                      hemispheric(jgroup,jrun,jdomain,jlevel,10)**2) )
              end if

           end do
        end do domainloop
     end do
  end do


!---------------------------------------------------------------------------------------
! Combine from many geographical domains

  global(:,:,:,:)=0.0
  do jgroup=1,nobgroups
     do jrun=1,2
        do jlevel=1,nlevels
           do jdomain=1,3     ! Derive global statistics from NH, TR and SH

              if (hemispheric(jgroup,jrun,jdomain,jlevel,1)>0.5) then
                 global(jgroup,jrun,jlevel,1)= &
                      global(jgroup,jrun,jlevel,1) + &
                      hemispheric(jgroup,jrun,jdomain,jlevel,1)
                 global(jgroup,jrun,jlevel,2)= &
                      global(jgroup,jrun,jlevel,2) + &
                      hemispheric(jgroup,jrun,jdomain,jlevel,1)* &
                      hemispheric(jgroup,jrun,jdomain,jlevel,2)
                 global(jgroup,jrun,jlevel,3)= &
                      global(jgroup,jrun,jlevel,3) + &
                      hemispheric(jgroup,jrun,jdomain,jlevel,1)* &
                      hemispheric(jgroup,jrun,jdomain,jlevel,3)**2
              end if

              if (hemispheric(jgroup,jrun,jdomain,jlevel,5)>0.5) then
                 global(jgroup,jrun,jlevel,5)= &
                      global(jgroup,jrun,jlevel,5) + &
                      hemispheric(jgroup,jrun,jdomain,jlevel,5)
                 global(jgroup,jrun,jlevel,6)= &
                      global(jgroup,jrun,jlevel,6) + &
                      hemispheric(jgroup,jrun,jdomain,jlevel,5)* &
                      hemispheric(jgroup,jrun,jdomain,jlevel,6)
                 global(jgroup,jrun,jlevel,7)= &
                      global(jgroup,jrun,jlevel,7) + &
                   hemispheric(jgroup,jrun,jdomain,jlevel,5)* &
                   hemispheric(jgroup,jrun,jdomain,jlevel,7)**2
              end if

              if (hemispheric(jgroup,jrun,jdomain,jlevel,9)>0.5) then
                 global(jgroup,jrun,jlevel,9)= &
                      global(jgroup,jrun,jlevel,9) + &
                      hemispheric(jgroup,jrun,jdomain,jlevel,9)
                 global(jgroup,jrun,jlevel,10)= &
                      global(jgroup,jrun,jlevel,10) + &
                      hemispheric(jgroup,jrun,jdomain,jlevel,9)* &
                      hemispheric(jgroup,jrun,jdomain,jlevel,10)
                 global(jgroup,jrun,jlevel,11)= &
                      global(jgroup,jrun,jlevel,11) + &
                   hemispheric(jgroup,jrun,jdomain,jlevel,9)* &
                   hemispheric(jgroup,jrun,jdomain,jlevel,11)**2
              end if

           end do

           if (global(jgroup,jrun,jlevel,1)>0.5) then
              global(jgroup,jrun,jlevel,2)= &
                   global(jgroup,jrun,jlevel,2)/global(jgroup,jrun,jlevel,1)
              global(jgroup,jrun,jlevel,3)= &
                   sqrt(global(jgroup,jrun,jlevel,3)/global(jgroup,jrun,jlevel,1))
              if (global(jgroup,jrun,jlevel,1)<1.5_jprb) then
                cfactor=1.0_jprb
              else
                cfactor = global(jgroup,jrun,jlevel,1) / &
                        ( global(jgroup,jrun,jlevel,1)-1)
              endif
              global(jgroup,jrun,jlevel,4)= &
                   sqrt(cfactor* &
                  (global(jgroup,jrun,jlevel,3)**2- &
                   global(jgroup,jrun,jlevel,2)**2) )
           end if

           if (global(jgroup,jrun,jlevel,5)>0.5) then
              global(jgroup,jrun,jlevel,6)= &
                   global(jgroup,jrun,jlevel,6)/global(jgroup,jrun,jlevel,5)
              global(jgroup,jrun,jlevel,7)= &
                   sqrt(global(jgroup,jrun,jlevel,7)/global(jgroup,jrun,jlevel,5))
              if (global(jgroup,jrun,jlevel,5)<1.5_jprb) then
                cfactor=1.0_jprb
              else
                cfactor = global(jgroup,jrun,jlevel,5) / &
                        ( global(jgroup,jrun,jlevel,5)-1)
              endif
              global(jgroup,jrun,jlevel,8)= &
                   sqrt(cfactor* &
                  (global(jgroup,jrun,jlevel,7)**2- &
                   global(jgroup,jrun,jlevel,6)**2) )
           end if

           if (global(jgroup,jrun,jlevel,9)>0.5) then
              global(jgroup,jrun,jlevel,10)= &
                   global(jgroup,jrun,jlevel,10)/global(jgroup,jrun,jlevel,9)
              global(jgroup,jrun,jlevel,11)= &
                   sqrt(global(jgroup,jrun,jlevel,11)/global(jgroup,jrun,jlevel,9))
              if (global(jgroup,jrun,jlevel,9)<1.5_jprb) then
                cfactor=1.0_jprb
              else
                cfactor = global(jgroup,jrun,jlevel,9) / &
                        ( global(jgroup,jrun,jlevel,9)-1)
              endif
              global(jgroup,jrun,jlevel,12)= &
                   sqrt(cfactor* &
                  (global(jgroup,jrun,jlevel,11)**2- &
                   global(jgroup,jrun,jlevel,10)**2) )
           end if

        end do
     end do
  end do

!---------------------------------------------------------------------------------------
! Differentiating
  hemispheric(:,3,:,:,:) = hemispheric(:,1,:,:,:)-hemispheric(:,2,:,:,:)
  where (abs(hemispheric(:,2,:,:,:))>1.0e-9) &
       hemispheric(:,4,:,:,:) = hemispheric(:,1,:,:,:)/hemispheric(:,2,:,:,:)
  global(:,3,:,:) = global(:,1,:,:)-global(:,2,:,:)
  where (abs(global(:,2,:,:))>1.0e-9) &
       global(:,4,:,:) = global(:,1,:,:)/global(:,2,:,:)


end subroutine fetchOneTime


subroutine cris_ch2wn(x)
  use parameters, only : jpim, jprb
  implicit none
  real(kind=jprb) :: x

  if (x<713.5) then
     x=650.0+.625*(x-1.0)
  else if (x<1578.5) then
     x=1210.0+0.625*(x-714.0)
  else
     x=2155.0+0.625*(x-1579.0)
  endif
end subroutine cris_ch2wn

subroutine iasi_wn2ch(x,i)
  use parameters, only : jpim, jprb
  implicit none
  integer(kind=jpim) :: i
  real(kind=jprb) :: x
  i=nint( (x-644.75)/.25)
end subroutine iasi_wn2ch
