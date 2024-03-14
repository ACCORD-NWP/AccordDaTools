function vcoordinate(level,jgroup) result(vindex)

! Converting vertical coordinate from whatever is given into array
! index.

  use parameters, only : &
       jpim, jprb, &
       nlevels_temp, &
       nlevels_rttov, &
       iasi_chanback
  use array_definitions, only : &
       levels_temp, levels_rttov

  implicit none
  real(kind=jprb) :: level
  integer(kind=jpim) :: jgroup, jlevel, vindex

  vindex=0_jpim

  if ( (jgroup>= 1_jpim .and. jgroup<= 6_jpim) .or. & ! TEMP
       (jgroup==44_jpim) .or.                       & ! TEMPZ
       (jgroup>= 7_jpim .and. jgroup<=10_jpim) .or. & ! AIREP
       (jgroup>=11_jpim .and. jgroup<=14_jpim) .or. & ! TEMP+AIREP U+V
       (jgroup>=26_jpim .and. jgroup<=27_jpim) .or. & ! RADAR RH/REFL
       (jgroup==43_jpim) .or. &                       ! RADAR WIND
       (jgroup>=29_jpim .and. jgroup<=37) ) then      ! SATOB
     do jlevel=1,nlevels_temp
        if (nint(level)==nint(levels_temp(jlevel))) then
           vindex=jlevel
           exit
        end if
     end do
     
  else if (jgroup==15_jpim) then                 ! SURFACE
     vindex=nint(level)
     
  else if (jgroup==16_jpim) then                 ! AMSU-A
     vindex=nint(level)
     
  else if (jgroup==17_jpim) then                 ! ATMS
     vindex=nint(level)
     if (vindex==5 .or. vindex==17) vindex=0
     
  else if (jgroup==18_jpim) then                 ! MHS
     vindex=nint(level)
     
  else if (jgroup==19_jpim) then                 ! MWHS2
     vindex=nint(level)
     
  else if (jgroup==28_jpim) then                 ! SEVIRI
     vindex=nint(level)
     
  else if ( (jgroup>=20_jpim .and. jgroup<=21_jpim) .or. & ! IASI
            (jgroup>=23_jpim .and. jgroup<=24_jpim) ) then ! CRIS
     vindex=1
     do jlevel=2,nlevels_temp
        if (abs(level-levels_temp(jlevel)) < abs(level-levels_temp(vindex))) then
           vindex=jlevel
        end if
     end do
     
  else if ( (jgroup==22_jpim .or. jgroup==25_jpim) .or. &  ! IR Wavenumber space
            (jgroup>=38_jpim .and. jgroup<=40_jpim) .or. & ! IASI individuals
            (jgroup>=41_jpim .and. jgroup<=42_jpim) ) then ! CRIS individuals
     vindex=nint((level-644.75)/.25)
!*
     if (jgroup==22_jpim .or. (jgroup>=38_jpim .and. jgroup<=40_jpim)) then

        vindex=iasi_chanback(vindex)

!        select case (vindex)
!        case (38)
!           vindex=1
!        case (51)
!           vindex=2
!        case (63)
!           vindex=3
!        case (85)
!           vindex=4
!        case (104)
!           vindex=5
!        case (109)
!           vindex=6
!        case (167)
!           vindex=7
!        case (173)
!           vindex=8
!        case (180)
!           vindex=9
!        case (185)
!           vindex=10
!        case (193)
!           vindex=11
!        case (199)
!           vindex=12
!        case (205)
!           vindex=13
!        case (207)
!           vindex=14
!        case (212)
!           vindex=15
!        case (224)
!           vindex=16
!        case (230)
!           vindex=17
!        case (236)
!           vindex=18
!        case (239)
!           vindex=19
!        case (242)
!           vindex=20
!        case (243)
!           vindex=21
!        case (249)
!           vindex=22
!        case (296)
!           vindex=23
!        case (333)
!           vindex=24
!        case (337)
!           vindex=25
!        case (345)
!           vindex=26
!        case (352)
!           vindex=27
!        case (386)
!           vindex=28
!        case (389)
!           vindex=29
!        case (432)
!           vindex=30
!        case (2701)
!           vindex=31
!        case (2819)
!           vindex=32
!        case (2910)
!           vindex=33
!        case (2919)
!           vindex=34
!        case (2991)
!           vindex=35
!        case (2993)
!           vindex=36
!        case (3002)
!           vindex=37
!        case (3008)
!           vindex=38
!        case (3014)
!           vindex=39
!        case (3098)
!           vindex=40
!        case (3207)
!           vindex=41
!        case (3228)
!           vindex=42
!        case (3281)
!           vindex=43
!        case (3309)
!           vindex=44
!        case (3322)
!           vindex=45
!        case (3438)
!           vindex=46
!        case (3442)
!           vindex=47
!        case (3484)
!           vindex=48
!        case (3491)
!           vindex=49
!        case (3499)
!           vindex=50
!        case (3506)
!           vindex=51
!        case (3575)
!           vindex=52
!        case (3582)
!           vindex=53
!        case (3658)
!           vindex=54
!        case (4032)
!           vindex=55         
!        case default
!           vindex=0
!        end select
        
     elseif (jgroup==25_jpim .or. (jgroup>=41_jpim .and. jgroup<=42_jpim)) then
        if (level<1100.0) then
           vindex=nint((level-649.375)/.625)
        elseif (level<1800.0) then
           vindex=nint((level-1209.375)/.625)+713
        else
           vindex=nint((level-2154.375)/.625)+1578
        endif
        select case (vindex)
        case (27)
           vindex=1
        case (59)
           vindex=2
        case (67)
           vindex=3
        case (69)
           vindex=4
        case (79)
           vindex=5
        case (80)
           vindex=6
        case (81)
           vindex=7
        case (82)
           vindex=8
        case (83)
           vindex=9
        case (84)
           vindex=10
        case (85)
           vindex=11
        case (86)
           vindex=12
        case (87)
           vindex=13
        case (88)
           vindex=14
        case (91)
           vindex=15
        case (92)
           vindex=16
        case (93)
           vindex=17
        case (94)
           vindex=18
        case (95)
           vindex=19
        case (96)
           vindex=20
        case (97)
           vindex=21
        case (98)
           vindex=22
        case (99)
           vindex=23
        case (100)
           vindex=24
        case (103)
           vindex=25
        case (104)
           vindex=26
        case (105)
           vindex=27
        case (106)
           vindex=28
        case (107)
           vindex=29
        case (108)
           vindex=30
        case (109)
           vindex=31
        case (110)
           vindex=32
        case (111)
           vindex=33
        case (115)
           vindex=34
        case (116)
           vindex=35
        case (117)
           vindex=36
        case (118)
           vindex=37
        case (119)
           vindex=38
        case (120)
           vindex=39
        case (121)
           vindex=40
        case (122)
           vindex=41
        case (123)
           vindex=42
        case (124)
           vindex=43
        case (125)
           vindex=44
        case (126)
           vindex=45
        case (132)
           vindex=46
        case (133)
           vindex=47
        case (134)
           vindex=48
        case (135)
           vindex=49
        case (136)
           vindex=50
        case (137)
           vindex=51
        case (138)
           vindex=52
        case (139)
           vindex=53
        case (140)
           vindex=54
        case (141)
           vindex=55
        case (142)
           vindex=56
        case (143)
           vindex=57
        case (144)
           vindex=58
        case (147)
           vindex=59
        case (161)
           vindex=60
        case (173)
           vindex=61
        case (177)
           vindex=62
        case (181)
           vindex=63
        case (185)
           vindex=64
        case (195)
           vindex=65
        case (199)
           vindex=66
        case (242)
           vindex=67
        case (275)
           vindex=68
        case (295)
           vindex=69
        case (342)
           vindex=70
        case (404)
           vindex=71
        case (439)
           vindex=72
        case (449)
           vindex=73
        case (464)
           vindex=74
        case (475)
           vindex=75
        case (490)
           vindex=76
        case (501)
           vindex=77
        case (898)
           vindex=78
        case (914)
           vindex=79
        case (972)
           vindex=80
        case (988)
           vindex=81
        case (1018)
           vindex=82
        case (1029)
           vindex=83
        case (1046)
           vindex=84
        case (1053)
           vindex=85
        case (1060)
           vindex=86
        case (1064)
           vindex=87
        case (1076)
           vindex=88
        case (1112)
           vindex=89
        case (1189)
           vindex=90
        case (1205)
           vindex=91
        case default
           vindex=0
        end select
     endif
!*
  end if
end function vcoordinate



function vcoordinate_back(jlevel,jgroup,irank,offset) result(level)

! Converting array index into the vertical coordinate used in visual
! plotting

  use parameters, only : &
       jpim, jprb, &
       nlevels_temp, &
       nlevels_rttov, &
       niasich_metcoop, &
       iasich_metcoop, &
       ncrisch_metcoop, &
       crisch_metcoop
  use array_definitions, only : &
       levels_temp, levels_rttov

  implicit none
  integer(kind=jpim) :: jlevel, jgroup, irank
  real(kind=jprb) :: level, offset, offs

  level=0.0_jprb

  if ( (jgroup>= 1_jpim .and. jgroup<= 6_jpim) .or. & ! TEMP
       (jgroup==44_jpim) .or.                       & ! TEMPZ
       (jgroup>= 7_jpim .and. jgroup<=10_jpim) .or. & ! AIREP
       (jgroup>=11_jpim .and. jgroup<=14_jpim) .or. & ! TEMP+AIREP U+V
       (jgroup>=26_jpim .and. jgroup<=27_jpim) .or. & ! RADAR RH/REFL
       (jgroup==43_jpim) .or. &                       ! RADAR WIND
       (jgroup>=29_jpim .and. jgroup<=37_jpim) ) then ! SATOB
     level=levels_temp(jlevel)

  else if (jgroup==15_jpim) then                      ! SURFACE
     level=jlevel*1.0_jprb

  else if (jgroup==16_jpim) then                      ! AMSU-A
     level=jlevel*1.0_jprb

  else if (jgroup==17_jpim) then                      ! ATMS
     level=jlevel*1.0_jprb
     if (jlevel==5 .or. jlevel==17) level=0.0

  else if (jgroup==18_jpim) then                      ! MHS
     level=jlevel*1.0_jprb

  else if (jgroup==19_jpim) then                      ! MWHS2
     level=jlevel*1.0_jprb

  else if ( (jgroup>=20_jpim .and. jgroup<=21_jpim) .or. & ! IASI
            (jgroup>=23_jpim .and. jgroup<=24_jpim) ) then ! CRIS
     level=levels_temp(jlevel)
     
  else if ( (jgroup>=22_jpim .and. jgroup<=25_jpim) .or. & ! IR wavenumber space
            (jgroup>=38_jpim .and. jgroup<=40_jpim) .or. & ! IASI individuals
            (jgroup>=41_jpim .and. jgroup<=42_jpim) ) then ! CRIS individuals
     level=644.75+.25*jlevel
!*
     if (jgroup==22_jpim) level=1.0*iasich_metcoop(jlevel)
     if (jgroup==25_jpim) level=1.0*crisch_metcoop(jlevel)
!*

  else if (jgroup==28_jpim) then                  ! SEVIRI
     level=jlevel*1.0_jprb
  end if

  if (abs(offset)<1000000.0_jprb) then  ! linear offset
     level=level+(irank-1)*offset
  else
     offs=(irank-1)*(abs(offset)-1000000.0_jprb)
     if (offset<0.0_jprb) offs=-1_jpim*offs
     level=exp(log(level)+offs)
  end if

end function vcoordinate_back
