module parameters
  implicit none

  integer, parameter :: jpim=selected_int_kind(9)
  integer, parameter :: jprb=selected_real_kind(13,300)

  integer(kind=jpim), parameter :: interval=3        ! Hours btw. successive times
!  integer(kind=jpim), parameter :: interval=24       ! Hours btw. successive times
  integer(kind=jpim), parameter :: nobgroups=44      ! Number of obs. groups
  integer(kind=jpim), parameter :: nsensors=10       ! Max. sensors in a group
  integer(kind=jpim), parameter :: ndomains=8        ! Number of geogr. domains
! integer(kind=jpim), parameter :: nlevels_small=259 ! Number of plotting levels
  integer(kind=jpim), parameter :: nlevels_small=174 ! Number of plotting levels
  integer(kind=jpim), parameter :: nlevels_large=174 ! Number of wavenumber-space plotting levels
  integer(kind=jpim), parameter :: nstat=12          ! Number of statistics 
  integer(kind=jpim), parameter :: maxtimes=732      ! Max. timeslots
  integer(kind=jpim), parameter :: nlevels_temp=30   ! Number of TEMP pressures
  integer(kind=jpim), parameter :: nlevels_rttov=43  ! Number of RTTOV pressures
  integer(kind=jpim), parameter :: niasich=8461      ! Number of IASI channels
! integer(kind=jpim), parameter :: niasich_metcoop=259
  integer(kind=jpim), parameter :: niasich_metcoop=174
  integer(kind=jpim), dimension(niasich_metcoop), parameter :: iasich_metcoop = (/ &
!        16,   38,   49,   51,   55,   57,   59,   61,   63,   66, &
!        70,   72,   74,   79,   81,   83,   85,   87,   89,  101, &
!       104,  106,  109,  111,  113,  116,  119,  122,  125,  128, &
!       131,  133,  135,  138,  141,  144,  146,  148,  151,  154, &
!       157,  159,  161,  163,  165,  167,  170,  173,  176,  178, &
!       179,  180,  183,  185,  187,  189,  191,  193,  195,  197, &
!       199,  201,  203,  205,  207,  210,  212,  214,  217,  219, &
!       222,  224,  226,  228,  230,  232,  234,  236,  239,  241, &
!       242,  243,  246,  249,  252,  254,  256,  258,  260,  262, &
!       265,  267,  269,  271,  272,  273,  275,  278,  280,  282, &
!       284,  286,  288,  290,  292,  294,  296,  299,  301,  303, &
!       306,  308,  310,  312,  314,  316,  318,  320,  323,  325, &
!       327,  329,  331,  333,  335,  337,  341,  345,  347,  350, &
!       352,  354,  356,  358,  360,  362,  364,  366,  369,  371, &
!       373,  375,  377,  379,  381,  383,  386,  389,  398,  401, &
!       404,  407,  410,  414,  416,  426,  428,  432,  434,  439, &
!       445,  457,  515,  546,  552,  559,  566,  571,  573,  646, &
!       662,  668,  756,  867,  921, 1027, 1133, 1191, 1194, 1271, &
!      1479, 1509, 1513, 1521, 1536, 1574, 1578, 1579, 1585, 1587, &
!      1626, 1639, 1643, 1652, 1658, 1671, 1805, 1884, 1946, 1991, &
!      2094, 2239, 2701, 2819, 2889, 2907, 2910, 2919, 2939, 2944, &
!      2958, 2991, 2993, 3002, 3008, 3014, 3029, 3049, 3093, 3098, &
!      3105, 3107, 3110, 3160, 3165, 3168, 3207, 3244, 3248, 3252, &
!      3228, 3281, 3309, 3312, 3322, 3411, 3438, 3442, 3446, 3448, &
!      3452, 3484, 3491, 3499, 3506, 3509, 3575, 3580, 3582, 3653, &
!      3658, 4032, 5379, 5381, 5383, 5399, 5401, 5403, 5480 /)
!
! Those that remain after removing higher stratospheric and
! ozone channels:       
!        38,   51,   57,   63,   85,  104,  109,  116,  122,  128, &
!       135,  141,  148,  154,  161,  167,  173,  180,  185,  187, &
!       193,  199,  205,  207,  212,  217,  219,  224,  226,  230, &
!       232,  236,  239,  241,  242,  243,  246,  249,  252,  254, &
!       256,  258,  260,  262,  265,  267,  269,  275,  278,  280, &
!       282,  284,  286,  288,  290,  292,  294,  296,  306,  308, &
!       310,  312,  314,  316,  318,  320,  323,  325,  327,  329, &
!       331,  333,  335,  337,  341,  345,  347,  350,  352,  354, &
!       356,  358,  360,  362,  364,  366,  369,  371,  373,  375, &
!       377,  379,  381,  383,  386,  389,  398,  401,  404,  407, &
!       410,  414,  416,  426,  428,  432,  434,  439,  445,  457, &
!       515,  546,  552,  559,  566,  571,  573,  646,  662,  668, &
!       756,  867,  921, 1027, 1133, 1191, 1194, 1271, 1805, 1884, &
!      1946, 1991, 2094, 2239, 2701, 2819, 2889, 2907, 2910, 2919, &
!      2939, 2944, 2958, 2991, 2993, 3002, 3008, 3014, 3029, 3049, &
!      3093, 3098, 3105, 3107, 3110, 3160, 3165, 3168, 3207, 3244, &
!      3248, 3252, 3228, 3281, 3309, 3312, 3322, 3411, 3438, 3442, &
!      3446, 3448, 3452, 3484, 3491, 3499, 3506, 3509, 3575, 3580, &
!      3582, 3653, 3658, 4032, 5379, 5381, 5383, 5399, 5401, 5403, &
!      5480 /)
!
! Those that remain after removing weak H2O, the excessive windows,
! or similar channels: + adding 337, 345 anyway for consistency
         38,   51,   57,   63,   85,  104,  109,  116,  122,  128, &
        135,  141,  148,  154,  161,  167,  173,  180,  185,  187, &
        193,  199,  205,  207,  212,  217,  219,  224,  226,  230, &
        232,  236,  239,        242,  243,  246,  249,  252,  254, &
        256,  258,  260,  262,  265,  267,  269,  275,  278,  280, &
        282,  284,  286,  288,  290,  292,  294,  296,  306,  308, &
        310,  312,  314,  316,  318,  320,  323,  325,  327,  329, &
        331,  333,  335,  337,  341,  345,  347,  350,  352,  354, &
        356,  358,  360,  362,  364,  366,  369,  371,  373,  375, &
        377,  379,  381,        386,  389,              404,  407, &
        410,  414,  416,  426,  428,  432,  434,        445,  457, &
        515,  546,  552,        566,  571,  573,  646,  662,  668, &
        756,  867,  921, 2701, 2819, 2889, 2907, 2910, 2919, 2939, &
       2944, 2958, 2991, 2993, 3002, 3008, 3014, 3029, 3049, 3093, &
       3098, 3105, 3107, 3110, 3160, 3165, 3168, 3207, 3244, 3248, &
       3252, 3228, 3281, 3309, 3312, 3322, 3411, 3438, 3442, 3446, &
       3448, 3452, 3484, 3491, 3499, 3506, 3509, 3575, 3580, 3582, &
       3653, 3658, 4032, 5379, 5381, 5383, 5399, 5401, 5403, 5480 /)
  integer(kind=jpim), dimension(niasich) :: iasi_chanback
  integer(kind=jpim), parameter :: ncrisch_metcoop=91
  integer(kind=jpim), dimension(ncrisch_metcoop), parameter :: crisch_metcoop = (/ &
         27,  59,  67,  69,  79,  80,  81,  82,  83,  84, &
         85,  86,  87,  88,  91,  92,  93,  94,  95,  96, &
         97,  98,  99, 100, 103, 104, 105, 106, 107, 108, &
        109, 110, 111, 115, 116, 117, 118, 119, 120, 121, &
        122, 123, 124, 125, 126, 132, 133, 134, 135, 136, &
        137, 138, 139, 140, 141, 142, 143, 144, 147, 161, &
        173, 177, 181, 185, 195, 199, 242, 275, 295, 342, &
        404, 439, 449, 464, 475, 490, 501, 898, 914, 972, &
        988,1018,1029,1046,1053,1060,1064,1076,1112,1189, &
       1205 /)

  logical :: l_uncorrected                           ! Show un-bias-corrected mean departures
  logical :: l_chisq_conf_intervals                  ! Confidence intervals based on chi-square distribution

  character(len=4) :: uid                            ! User ID

! Satellite instruments to be included in output statistics
  type type_satellite_instruments
     logical :: noaa15_amsua, noaa16_amsua, noaa17_amsua, &
                noaa18_amsua, noaa19_amsua, &
                metopa_amsua, metopb_amsua, metopc_amsua
     logical :: noaa18_amsub, noaa19_mhs, &
                metopa_mhs, metopb_mhs, metopc_mhs
     logical :: fy3c_mwhs2, fy3d_mwhs2
     logical :: snpp_atms, noaa20_atms
     logical :: metopa_iasi, metopb_iasi, metopc_iasi
     logical :: snpp_cris, noaa20_cris
  end type type_satellite_instruments
  type(type_satellite_instruments) :: satinst
  
end module parameters
