! Converting input character string into sensor and observation group
! indices.
!
! Supported jobgroup indices
!
!  1 TEMP U
!  2 TEMP V
!  3 TEMP U/V
!  4 TEMP T
!  5 TEMP Q
!  6 TEMP RH
!  7 AIREP U
!  8 AIREP V
!  9 AIREP U/V
! 10 AIREP T
! 11 TEMP/AIREP U
! 12 TEMP/AIREP V
! 13 TEMP/AIREP U/V
! 14 TEMP/AIREP T
! 15 SURFACE
! 16 AMSU-A
! 17 ATMS
! 18 MHS
! 19 MWHS2
! 20 IASI LW
! 21 IASI WV
! 22 IASI LW Wavenumber space
! 23 CRIS LW
! 24 CRIS WV
! 25 CRIS LW Wavenumber space
! 26 Ground-based radar RH
! 27 Ground-based radar reflectivity
! 28 SEVIRI radiance
! 29 SATOB U
! 30 SATOB V
! 31 SATOB U/V
! 32 GEO AMV U
! 33 GEO AMV V
! 34 GEO AMV U/V
! 35 POL AMV U
! 36 POL AMV V
! 37 POL AMV U/V
! 38 Metop-A IASI
! 39 Metop-B IASI
! 40 Metop-C IASI
! 41 NPP CrIS
! 42 NOAA-20 CrIS
! 43 Ground-based radar radial wind
! 44 TEMP Z
!
!---------------------------------------------------------------------

subroutine getSensorNumber(csensor,jsensor,jobgroup,jposition)
  use parameters, only : jpim, satinst
  implicit none
  character(len=100) :: csensor
  integer(kind=jpim) :: jsensor, jobgroup(4), jposition

  jobgroup(:)=0_jpim
  jposition=0

  select case (trim(csensor))

!--- TEMP sounding
  case ('TEMP U')
     jsensor=1_jpim
     jobgroup(1)=1_jpim
     jobgroup(2)=3_jpim
     jobgroup(3)=11_jpim
     jobgroup(4)=13_jpim
  case ('TEMP V')
     jsensor=1_jpim
     jobgroup(1)=2_jpim
     jobgroup(2)=3_jpim
     jobgroup(3)=12_jpim
     jobgroup(4)=13_jpim
  case ('TEMP T')
     jsensor=1_jpim
     jobgroup(1)=4_jpim
     jobgroup(2)=14_jpim
  case ('TEMP Q')
     jsensor=1_jpim
     jobgroup(1)=5_jpim
  case ('TEMP RH')
     jsensor=1_jpim
     jobgroup(1)=6_jpim
  case ('TEMP Z')
     jsensor=1_jpim
     jobgroup(1)=44_jpim

!--- AIREP
  case ('AMDAR U')
     jsensor=2_jpim
     jobgroup(1)=7_jpim
     jobgroup(2)=9_jpim
     jobgroup(3)=11_jpim
     jobgroup(4)=13_jpim
  case ('AMDAR V')
     jsensor=2_jpim
     jobgroup(1)=8_jpim
     jobgroup(2)=9_jpim
     jobgroup(3)=12_jpim
     jobgroup(4)=13_jpim
  case ('AMDAR T')
     jsensor=2_jpim
     jobgroup(1)=10_jpim
     jobgroup(2)=14_jpim

!--- SURFACE
! In-situ Zs go to position 1
! In-situ U10m/V10m to position 2
! Atmospheric Path Delay to position 3
! In-situ RH2m to position 4
! In-situ Q2m to position 5
! In-situ T2m to position 6
! In-situ Ts to position 7
! Scatterometer winds to position 8
  case ('Zs')
     jsensor=1_jpim
     jobgroup(1)=15_jpim
     jposition=1
  case ('U10m','V10m')
     jsensor=1_jpim
     jobgroup(1)=15_jpim
     jposition=2
  case ('APD','Ground-based GNSS')
     jsensor=1_jpim
     jobgroup(1)=15_jpim
     jposition=3
 case ('RH2m')
    jsensor=1_jpim
    jobgroup(1)=15_jpim
    jposition=4
 case ('Q2m')
    jsensor=1_jpim
    jobgroup(1)=15_jpim
    jposition=5
 case ('T2m')
    jsensor=1_jpim
    jobgroup(1)=15_jpim
    jposition=6
 case ('Ts')
    jsensor=1_jpim
    jobgroup(1)=15_jpim
    jposition=7
 case ('Metop-A ASCAT u component', &
       'Metop-A ASCAT v component', &
       'Metop-B ASCAT u component', &
       'Metop-B ASCAT v component', &
       'Metop-C ASCAT u component', &
       'Metop-C ASCAT v component')
     jsensor=1_jpim
     jobgroup(1)=15_jpim
     jposition=8

!===== ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== 

!--- AMSU-A radiances
  case ('NOAA-15 AMSU-A')
     if (satinst%noaa15_amsua) then
        jsensor=1_jpim
        jobgroup(1)=16_jpim
     endif
  case ('NOAA-16 AMSU-A')
     if (satinst%noaa16_amsua) then
        jsensor=2_jpim
        jobgroup(1)=16_jpim
     endif
  case ('NOAA-17 AMSU-A')
     if (satinst%noaa17_amsua) then
        jsensor=3_jpim
        jobgroup(1)=16_jpim
     endif
  case ('NOAA-18 AMSU-A')
     if (satinst%noaa18_amsua) then
        jsensor=4_jpim
        jobgroup(1)=16_jpim
     endif
  case ('NOAA-19 AMSU-A')
     if (satinst%noaa19_amsua) then
        jsensor=5_jpim
        jobgroup(1)=16_jpim
     endif
  case ('Metop-A AMSU-A')
     if (satinst%metopa_amsua) then
        jsensor=6_jpim
        jobgroup(1)=16_jpim
     endif
  case ('Metop-B AMSU-A')
     if (satinst%metopb_amsua) then
        jsensor=7_jpim
        jobgroup(1)=16_jpim
     endif
  case ('Metop-C AMSU-A')
     if (satinst%metopc_amsua) then
        jsensor=8_jpim
        jobgroup(1)=16_jpim
     endif

!--- ATMS radiances
  case ('S-NPP ATMS')
     if (satinst%snpp_atms) then
        jsensor=1_jpim
        jobgroup(1)=17_jpim
     endif
  case ('NOAA-20 ATMS')
     if (satinst%noaa20_atms) then
        jsensor=2_jpim
        jobgroup(1)=17_jpim
     endif

!--- MHS radiances
  case ('NOAA-18 AMSU-B','NOAA-18 MHS')
     if (satinst%noaa18_amsub) then
        jsensor=1_jpim
        jobgroup(1)=18_jpim
     endif
  case ('NOAA-19 MHS')
     if (satinst%noaa19_mhs) then
        jsensor=2_jpim
        jobgroup(1)=18_jpim
     endif
  case ('Metop-A MHS')
     if (satinst%metopa_mhs) then
        jsensor=3_jpim
        jobgroup(1)=18_jpim
     endif
  case ('Metop-B MHS')
     if (satinst%metopb_mhs) then
        jsensor=4_jpim
        jobgroup(1)=18_jpim
     endif
  case ('Metop-C MHS')
     if (satinst%metopc_mhs) then
        jsensor=5_jpim
        jobgroup(1)=18_jpim
     endif

!--- MWHS2 radiances
  case ('FY-3C MWHS2 Tb')
     if (satinst%fy3c_mwhs2) then
        jsensor=1_jpim
        jobgroup(1)=19_jpim
     endif
  case ('FY-3D MWHS2')
     if (satinst%fy3d_mwhs2) then
        jsensor=2_jpim
        jobgroup(1)=19_jpim
     endif

!--- IASI radiances
! jobgroup(1)s 20-22 are reserved for IASI; these correspond to
! long-wave band, water-vapour band, wavenumber space, respectively.

  case ('Metop-A IASI')
     if (satinst%metopa_iasi) then
        jsensor=1_jpim
        jobgroup(1)=20_jpim
        jobgroup(2)=22_jpim
        jobgroup(3)=38_jpim
     endif
  case ('Metop-B IASI')
     if (satinst%metopb_iasi) then
        jsensor=2_jpim
        jobgroup(1)=20_jpim
        jobgroup(2)=22_jpim
        jobgroup(3)=39_jpim
     endif
  case ('Metop-C IASI')
     if (satinst%metopc_iasi) then
        jsensor=3_jpim
        jobgroup(1)=20_jpim
        jobgroup(2)=22_jpim
        jobgroup(3)=40_jpim
     endif

!--- CRIS radiances
! jobgroup(1)s 23-25 are reserved for CRIS; these correspond to
! long-wave band, water-vapour band, wavenumber space, respectively.

  case ('S-NPP CrIS')
     if (satinst%snpp_cris) then
        jsensor=1_jpim
        jobgroup(1)=23_jpim
        jobgroup(2)=25_jpim
        jobgroup(3)=41_jpim
     endif
  case ('NOAA-20 CrIS')
     if (satinst%noaa20_cris) then
        jsensor=2_jpim
        jobgroup(1)=23_jpim
        jobgroup(2)=25_jpim
        jobgroup(3)=42_jpim
     endif

!===== ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== ===== 

!--- Ground-based radar
 case ('Radar RH')
    jsensor=1_jpim
    jobgroup(1)=26_jpim
 case ('Radar reflectivity')
    jsensor=1_jpim
    jobgroup(1)=27_jpim
 case ('Radar Doppler wind')
    jsensor=1_jpim
    jobgroup(1)=43_jpim

!--- SEVIRI
 case ('Meteosat-11 SEVIRI')
    jsensor=1_jpim
    jobgroup(1)=28_jpim

!--- SATOB winds
  case ('Meteosat-11 AMV U component')
     jsensor=1_jpim
     jobgroup(1)=32_jpim
     jobgroup(2)=34_jpim
     jobgroup(3)=29_jpim
     jobgroup(4)=31_jpim
  case ('Meteosat-11 AMV V component')
     jsensor=1_jpim
     jobgroup(1)=33_jpim
     jobgroup(2)=34_jpim
     jobgroup(3)=30_jpim
     jobgroup(4)=31_jpim
  case ('Metop-BC AMV U component')
     jsensor=2_jpim
     jobgroup(1)=35_jpim
     jobgroup(2)=37_jpim
     jobgroup(3)=29_jpim
     jobgroup(4)=31_jpim
  case ('Metop-BC AMV V component')
     jsensor=2_jpim
     jobgroup(1)=36_jpim
     jobgroup(2)=37_jpim
     jobgroup(3)=30_jpim
     jobgroup(4)=31_jpim
  case ('Metop-C AMV U component')
     jsensor=3_jpim
     jobgroup(1)=35_jpim
     jobgroup(2)=37_jpim
     jobgroup(3)=29_jpim
     jobgroup(4)=31_jpim
  case ('Metop-C AMV V component')
     jsensor=3_jpim
     jobgroup(1)=36_jpim
     jobgroup(2)=37_jpim
     jobgroup(3)=30_jpim
     jobgroup(4)=31_jpim

 case default
     jsensor=0_jpim
  end select

end subroutine getSensorNumber
