subroutine report

  use parameters, only : &
      jpim, jprb, &
      nobgroups, ndomains, &
      nlevels_temp, nlevels_rttov, &
      l_chisq_conf_intervals

  use array_definitions, only : &
      expid, conid, nlevels, &
      statistic, stat95l, stat95u, stat99l, stat99u, &
      stat95_bg, stat95_an, &
      groupid, irank

  implicit none

  integer(kind=jpim) :: jgroup, jdomain, jlevel
  character(len=200) :: filename_base
  real(kind=jprb) :: vcoordinate_back, offset
  logical :: separation1, separation2

  do jgroup=1,nobgroups
!---------------------------------------------------------

! Skip those observation types that we don't currently have in MetCoOp
     if (jgroup==27) cycle ! Radar refl
     if (jgroup==28) cycle ! SEVIRI
     if (jgroup==29) cycle ! SATOBU
     if (jgroup==30) cycle ! SATOBV
     if (jgroup==31) cycle ! SATOBUV
     if (jgroup==32) cycle ! GEOAMVU
     if (jgroup==33) cycle ! GEOAMVV
     if (jgroup==34) cycle ! GEOAMVUV
     if (jgroup==35) cycle ! POLAMVU
     if (jgroup==36) cycle ! POLAMVV
     if (jgroup==37) cycle ! POLAMVUV
     if (jgroup==38) cycle ! M-A IASI

     
     offset=0.0
     separation1=.false.
     separation2=.false.

! Logarithmic offsets
     if (jgroup== 1)  offset=-1000000.04_jprb  ! TEMPU
     if (jgroup== 2)  offset=-1000000.04_jprb  ! TEMPV
     if (jgroup== 3)  offset=-1000000.04_jprb  ! TEMPUV
     if (jgroup== 4)  offset=-1000000.04_jprb  ! TEMPT
     if (jgroup==44)  offset=-1000000.04_jprb  ! TEMPZ
     if (jgroup==11)  offset=-1000000.04_jprb  ! TPAU
     if (jgroup==12)  offset=-1000000.04_jprb  ! TPAV
     if (jgroup==13)  offset=-1000000.04_jprb  ! TPAUV
     if (jgroup==14)  offset=-1000000.04_jprb  ! TAT
     if (jgroup==20)  offset=-1000000.04_jprb  ! IASILW
     if (jgroup==23)  offset=-1000000.04_jprb  ! CRISLW

! Linear offsets
     if (jgroup==5)  offset=-12._jprb  ! TEMPQ
     if (jgroup==6)  offset=-12._jprb  ! TEMPRH
     if (jgroup==7)  offset=-12._jprb  ! AIREPU
     if (jgroup==8)  offset=-12._jprb  ! AIREPV
     if (jgroup==9)  offset=-12._jprb  ! AIREPUV
     if (jgroup==10) offset=-12._jprb  ! AIREPT
     if (jgroup==15) offset=0.10_jprb  ! SURFACE
     if (jgroup==16) offset=0.12_jprb  ! AMSUA
     if (jgroup==17) offset=0.20_jprb  ! ATMS
     if (jgroup==18) offset=0.06_jprb  ! MHS
     if (jgroup==19) offset=0.12_jprb  ! MWHS2
     if (jgroup==21) offset=-12._jprb  ! IASIWV
     if (jgroup==24) offset=-12._jprb  ! CRISWV
     if (jgroup==26) offset=-12._jprb  ! RADAR RH
     if (jgroup==28) offset=0.12_jprb  ! SEVIRI
     if (jgroup==29) offset=-12._jprb  ! SATOBU
     if (jgroup==30) offset=-12._jprb  ! SATOBV
     if (jgroup==31) offset=-12._jprb  ! SATOBUV
     if (jgroup==43) offset=-12._jprb  ! RADAR WIND

! Departure statistics
     filename_base=expid//'_'//conid//'_'//trim(groupid(jgroup))//'_omb_norm'
     open (109,file=trim(filename_base),status='replace')

     filename_base=expid//'_'//conid//'_'//trim(groupid(jgroup))//'_oma_norm'
     open (119,file=trim(filename_base),status='replace')

     filename_base=expid//'_'//conid//'_'//trim(groupid(jgroup))//'_omb_count'
     open (129,file=trim(filename_base),status='replace')

     filename_base=expid//'_'//trim(groupid(jgroup))//'_omb_mean'
     open (139,file=trim(filename_base),status='replace')

     filename_base=expid//'_'//trim(groupid(jgroup))//'_oma_mean'
     open (149,file=trim(filename_base),status='replace')

     filename_base=conid//'_'//trim(groupid(jgroup))//'_omb_mean'
     open (159,file=trim(filename_base),status='replace')

     filename_base=conid//'_'//trim(groupid(jgroup))//'_oma_mean'
     open (169,file=trim(filename_base),status='replace')

     filename_base=expid//'_'//trim(groupid(jgroup))//'_omb_sdev'
     open (179,file=trim(filename_base),status='replace')

     filename_base=conid//'_'//trim(groupid(jgroup))//'_omb_sdev'
     open (189,file=trim(filename_base),status='replace')

     filename_base=expid//'_'//trim(groupid(jgroup))//'_oma_sdev'
     open (199,file=trim(filename_base),status='replace')

     filename_base=conid//'_'//trim(groupid(jgroup))//'_oma_sdev'
     open (209,file=trim(filename_base),status='replace')

     filename_base=expid//'_'//trim(groupid(jgroup))//'_bcor_mean'
     open (219,file=trim(filename_base),status='replace')

     filename_base=conid//'_'//trim(groupid(jgroup))//'_bcor_mean'
     open (229,file=trim(filename_base),status='replace')

     do jdomain=ndomains+1,ndomains+1
        do jlevel=1,nlevels

           if (statistic(jgroup,1,jdomain,jlevel,1)<1.0e-1) cycle
           if ( (jgroup==17 .and. jlevel==18) .or. &
                ((.not. separation1) .and. jgroup==17 .and. jlevel==19) ) then   ! Separate ATMS WV channels

              separation1=.true.
              write (100+jdomain,*) ''
              write (110+jdomain,*) ''
              write (120+jdomain,*) ''
              write (130+jdomain,*) ''
              write (140+jdomain,*) ''
              write (150+jdomain,*) ''
              write (160+jdomain,*) ''
              write (170+jdomain,*) ''
              write (180+jdomain,*) ''
              write (190+jdomain,*) ''
              write (200+jdomain,*) ''
              write (210+jdomain,*) ''
              write (220+jdomain,*) ''
           end if

           if (l_chisq_conf_intervals) then
              write (100+jdomain,'(4(1x,f18.10))') &
                   statistic(jgroup,4,jdomain,jlevel,4), &
                   Vcoordinate_back(jlevel,jgroup,irank,offset), &
                   stat99l(jgroup,4,jdomain,jlevel,4)-statistic(jgroup,4,jdomain,jlevel,4), &
                   stat99u(jgroup,4,jdomain,jlevel,4)-statistic(jgroup,4,jdomain,jlevel,4)
              write (110+jdomain,'(4(1x,f18.10))') &
                   statistic(jgroup,4,jdomain,jlevel,8), &
                   Vcoordinate_back(jlevel,jgroup,irank,offset), &
                   stat99l(jgroup,4,jdomain,jlevel,8)-statistic(jgroup,4,jdomain,jlevel,8), &
                   stat99u(jgroup,4,jdomain,jlevel,8)-statistic(jgroup,4,jdomain,jlevel,8)
           else
              write (100+jdomain,'(4(1x,f18.10))') &
                   statistic(jgroup,4,jdomain,jlevel,4), &
                   Vcoordinate_back(jlevel,jgroup,irank,offset), &
                   -stat95_bg(jgroup,jdomain,jlevel), &
                   stat95_bg(jgroup,jdomain,jlevel)
              write (110+jdomain,'(4(1x,f18.10))') &
                   statistic(jgroup,4,jdomain,jlevel,8), &
                   Vcoordinate_back(jlevel,jgroup,irank,offset), &
                   -stat95_an(jgroup,jdomain,jlevel), &
                   stat95_an(jgroup,jdomain,jlevel)
           end if
           
           write (120+jdomain,'(2(1x,f18.10,1x,f18.5))') &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                statistic(jgroup,1,jdomain,jlevel,1)
           write (130+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,1,jdomain,jlevel,2), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0
           write (140+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,1,jdomain,jlevel,6), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0
           write (150+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,2,jdomain,jlevel,2), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0
           write (160+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,2,jdomain,jlevel,6), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0
           write (170+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,1,jdomain,jlevel,4), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0
           write (180+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,2,jdomain,jlevel,4), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0
           write (190+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,1,jdomain,jlevel,8), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0
           write (200+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,2,jdomain,jlevel,8), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0
           write (210+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,1,jdomain,jlevel,10), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0
           write (220+jdomain,'(4(1x,f18.10))') &
                statistic(jgroup,2,jdomain,jlevel,10), &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                0.0, 0.0

        end do

        write (120+jdomain,'(a)') ''

        do jlevel=1,nlevels

           if (statistic(jgroup,1,jdomain,jlevel,1)<1.0e-1) cycle
           if ( (jgroup==17 .and. jlevel==18) .or. &
                ((.not. separation2) .and. jgroup==17 .and. jlevel==19) ) then ! Separate ATMS WV channels

              separation2=.true.
              write (100+jdomain,*) ''
              write (110+jdomain,*) ''
              write (120+jdomain,*) ''
              write (130+jdomain,*) ''
              write (140+jdomain,*) ''
              write (150+jdomain,*) ''
              write (160+jdomain,*) ''
              write (170+jdomain,*) ''
              write (180+jdomain,*) ''
              write (190+jdomain,*) ''
              write (200+jdomain,*) ''
              write (210+jdomain,*) ''
              write (220+jdomain,*) ''
           end if

           write (120+jdomain,'(2(1x,f18.10,1x,f18.5))') &
                Vcoordinate_back(jlevel,jgroup,irank,offset), &
                statistic(jgroup,2,jdomain,jlevel,1)
        end do

     end do

     close (109)
     close (119)
     close (129)
     close (139)
     close (149)
     close (159)
     close (169)
     close (179)
     close (189)
     close (199)
     close (209)
     close (219)
     close (229)

  end do

  filename_base=expid//'_'//conid//'_datacount_summary'
  open (99,file=trim(filename_base),status='replace')
  do jgroup=1,nobgroups
     do jdomain=ndomains+1,ndomains+1
        do jlevel=1,nlevels
           if ( statistic(jgroup,1,jdomain,jlevel,1)>1.0e-1 .and. &
                statistic(jgroup,2,jdomain,jlevel,1)>1.0e-1 ) then
              write (99,'(2(1x,i2),1x,i4,2(1x,i8),1x,f12.6,1x,a)') &
                   jgroup, jdomain, jlevel, &
                   nint(statistic(jgroup,1,jdomain,jlevel,1)), &
                   nint(statistic(jgroup,2,jdomain,jlevel,1)), &
                   statistic(jgroup,1,jdomain,jlevel,1) / &
                   statistic(jgroup,2,jdomain,jlevel,1), &
                   trim(groupid(jgroup))
           elseif (statistic(jgroup,1,jdomain,jlevel,1)>1.0e-1) then
              write (99,'(3(1x,i2),2(1x,i8),1x,a)') &
                   jgroup, jdomain, jlevel, &
                   nint(statistic(jgroup,1,jdomain,jlevel,1)), &
                   nint(statistic(jgroup,2,jdomain,jlevel,1)), &
                   trim(groupid(jgroup))
           elseif (statistic(jgroup,2,jdomain,jlevel,1)>1.0e-1) then
              write (99,'(3(1x,i2),2(1x,i8),1x,a)') &
                   jgroup, jdomain, jlevel, &
                   nint(statistic(jgroup,1,jdomain,jlevel,1)), &
                   nint(statistic(jgroup,2,jdomain,jlevel,1)), &
                   trim(groupid(jgroup))
           endif
        enddo
     enddo
  enddo
  close (99)

end subroutine report
