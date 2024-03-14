subroutine average

  use parameters, only : &
       jpim, jprb, nobgroups, ndomains, nstat, l_uncorrected
  use array_definitions, only : &
       nlevels, ntimeslots, statarray, statistic, &
       stat95l, stat95u, stat99l, stat99u, &
       stat95_bg, stat95_an

  implicit none
  integer(kind=jpim) :: jgroup, jrun, jdomain, jlevel, jstat, jtimeslot
  integer(kind=jpim) :: ifound, ndf, i, ntimes_bg, ntimes_an
  real(kind=jprb), dimension(nstat) :: old, upd, new
  real(kind=jprb), dimension(nobgroups,ntimeslots,ndomains+1,nlevels,nstat) :: &
       depstat_array
  real(kind=jprb) :: mean, sdev, chisq95, sigma, chisq99
  real(kind=jprb) :: ratio_mean_bg, ratio_sdev_bg
  real(kind=jprb) :: ratio_mean_an, ratio_sdev_an

  depstat_array(:,:,:,:,:)=0.0_jprb

  do jgroup=1,nobgroups
     do jrun=1,2
        do jdomain=1,ndomains+1
           do jlevel=1,nlevels

              statistic(jgroup,jrun,jdomain,jlevel,:)=0.0_jprb
              stat95l(jgroup,jrun,jdomain,jlevel,:)=0.0_jprb
              stat95u(jgroup,jrun,jdomain,jlevel,:)=0.0_jprb
              stat99l(jgroup,jrun,jdomain,jlevel,:)=0.0_jprb
              stat99u(jgroup,jrun,jdomain,jlevel,:)=0.0_jprb

              
!             if (jdomain<5 .or. jdomain==6 .or. jdomain==8) then
              
              do jtimeslot=1,ntimeslots

                 old=statistic(jgroup,jrun,jdomain,jlevel,:)
                 upd=statarray(jgroup,jtimeslot,jrun,jdomain,jlevel,:)
                 new(:)=0.0_jprb

                 do jstat=1,nstat

                    if (jstat==1 .or. jstat==5 .or. jstat==9) then      ! Count

                       new(jstat)=old(jstat)+upd(jstat)

                    else if (jstat==2 .or. jstat==6 .or. jstat==10) then ! Mean

                       if (new(jstat-1)<1.0e-9) then
                          new(jstat)=upd(jstat)
                       else
                          new(jstat)= &
                               (old(jstat-1)*old(jstat)+upd(jstat-1)*upd(jstat)) / &
                               (old(jstat-1)+upd(jstat-1))
                       end if

                       if (jrun==1) then
                          depstat_array(jgroup,jtimeslot,jdomain,jlevel,jstat) = &
                               upd(jstat)
                       else if (jrun==2) then
                          depstat_array(jgroup,jtimeslot,jdomain,jlevel,jstat) = &
                               depstat_array(jgroup,jtimeslot,jdomain,jlevel,jstat) &
                               - upd(jstat)
                       end if

                    else if (jstat==3 .or. jstat==7 .or. jstat==11) then ! RMS

                       if (new(jstat-2)<1.0e-9) then
                          new(jstat)=upd(jstat)
                       else
                          new(jstat)= sqrt ( &
                               (old(jstat-2)*old(jstat)**2+upd(jstat-2)*upd(jstat)**2) / &
                               (old(jstat-2)+upd(jstat-2)) )
                       end if

                    else if (jstat==4 .or. jstat==8 .or. jstat==12) then ! S.Dev.

                       if (new(jstat-3)<1.0e-9) then
                          new(jstat)=upd(jstat)
                       else
                          new(jstat)=sqrt( (new(jstat-3)/(new(jstat-3)-1.0_jprb)) * &
                               (new(jstat-1)**2-new(jstat-2)**2) )
                       end if

                       if (jrun==1) then
                          depstat_array(jgroup,jtimeslot,jdomain,jlevel,jstat) = &
                               upd(jstat)
                       else if (jrun==2) then
                          depstat_array(jgroup,jtimeslot,jdomain,jlevel,jstat) = &
                               depstat_array(jgroup,jtimeslot,jdomain,jlevel,jstat) / &
                               upd(jstat)
                       end if

                    else

                       cycle

                    endif

                 end do

                 statistic(jgroup,jrun,jdomain,jlevel,:) = new(:)

              end do

              do jstat=4,8,4
                 ndf=nint(statistic(jgroup,jrun,jdomain,jlevel,jstat-3)-1.0_jprb)
                 sigma=statistic(jgroup,jrun,jdomain,jlevel,jstat)

                 if (ndf<0_jpim) cycle
                 if (ndf==0_jpim) then
                    write (*,'(a)') 'Warning: Confidence intervals could not' // &
                         ' be computed!'
                    write (*,'(5(1x,i2))') jgroup,jrun,jdomain,jlevel,jstat
                    cycle
                 endif
                 stat95l(jgroup,jrun,jdomain,jlevel,jstat) = &
                      chisq95(ndf,.true.)*sigma
                 stat95u(jgroup,jrun,jdomain,jlevel,jstat) = &
                      chisq95(ndf,.false.)*sigma
                 stat99l(jgroup,jrun,jdomain,jlevel,jstat) = &
                      chisq99(ndf,.true.)*sigma
                 stat99u(jgroup,jrun,jdomain,jlevel,jstat) = &
                      chisq99(ndf,.false.)*sigma
              end do

           end do

        end do
     end do
  end do


! Add mean bias correction to mean departures

  if (l_uncorrected) then

     do jgroup=1,nobgroups
        do jrun=1,2
           do jdomain=1,ndomains+1
              do jlevel=1,nlevels
                 statistic(jgroup,jrun,jdomain,jlevel,2)= &
                      statistic(jgroup,jrun,jdomain,jlevel,2) + &
                      statistic(jgroup,jrun,jdomain,jlevel,10)
                 statistic(jgroup,jrun,jdomain,jlevel,6)= &
                      statistic(jgroup,jrun,jdomain,jlevel,6) + &
                      statistic(jgroup,jrun,jdomain,jlevel,10)
              enddo
           enddo
        enddo
     enddo

  end if

! Differentiate
  statistic(:,3,:,:,:)=statistic(:,1,:,:,:)-statistic(:,2,:,:,:)

! Normalize by control
  where (abs(statistic(:,2,:,:,:))>1.0e-9) &
    statistic(:,4,:,:,:)=statistic(:,1,:,:,:)/statistic(:,2,:,:,:)
  where (abs(statistic(:,2,:,:,:))>1.0e-9) &
    stat95l(:,4,:,:,:)=stat95l(:,1,:,:,:)/statistic(:,2,:,:,:)
  where (abs(statistic(:,2,:,:,:))>1.0e-9) &
    stat95u(:,4,:,:,:)=stat95u(:,1,:,:,:)/statistic(:,2,:,:,:)
  where (abs(statistic(:,2,:,:,:))>1.0e-9) &
    stat99l(:,4,:,:,:)=stat99l(:,1,:,:,:)/statistic(:,2,:,:,:)
  where (abs(statistic(:,2,:,:,:))>1.0e-9) &
    stat99u(:,4,:,:,:)=stat99u(:,1,:,:,:)/statistic(:,2,:,:,:)


!-------------

  do jgroup=1,nobgroups
     do jdomain=1,ndomains+1
        do jlevel=1,nlevels

           ntimes_bg=0
           ratio_mean_bg=0.0
           do jtimeslot=1,ntimeslots
              if (abs(statarray(jgroup,jtimeslot,2,jdomain,jlevel,4))> &
                   1.0e-6) then
                 ntimes_bg=ntimes_bg+1
                 ratio_mean_bg = &
                      ratio_mean_bg + &
                      statarray(jgroup,jtimeslot,1,jdomain,jlevel,4) / &
                      statarray(jgroup,jtimeslot,2,jdomain,jlevel,4)
              end if
           end do
           ratio_mean_bg = &
                ratio_mean_bg/ntimes_bg

           ratio_sdev_bg=0.0
           do jtimeslot=1,ntimeslots
              if (abs(statarray(jgroup,jtimeslot,2,jdomain,jlevel,4))> &
                   1.0e-6) then
                 ratio_sdev_bg = &
                      ratio_sdev_bg + &
                      ((statarray(jgroup,jtimeslot,1,jdomain,jlevel,4) / &
                      statarray(jgroup,jtimeslot,2,jdomain,jlevel,4)) - &
                      ratio_mean_bg)**2
              end if
           end do
           if (ntimes_bg>1) then
              ratio_sdev_bg = sqrt ( ratio_sdev_bg / (ntimes_bg-1) )
              stat95_bg(jgroup,jdomain,jlevel) = &
                   1.96*ratio_sdev_bg/sqrt(1.0*ntimes_bg)   ! 95% indeed
!                  2.575*ratio_sdev_bg/sqrt(1.0*ntimes_bg)   ! 99%
!                  1.645*ratio_sdev_bg/sqrt(1.0*ntimes_bg)   ! 90%
           else
              ratio_sdev_bg = 0.0
              stat95_bg(jgroup,jdomain,jlevel) = 0.0
           end if

!-------------
! 95% confidence intervals from daily variability in sdev ratios

           ntimes_an=0
           ratio_mean_an=0.0
           do jtimeslot=1,ntimeslots
              if (abs(statarray(jgroup,jtimeslot,2,jdomain,jlevel,8))> &
                   1.0e-6) then
                 ntimes_an=ntimes_an+1
                 ratio_mean_an = &
                      ratio_mean_an + &
                      statarray(jgroup,jtimeslot,1,jdomain,jlevel,8) / &
                      statarray(jgroup,jtimeslot,2,jdomain,jlevel,8)
              end if
           end do
           ratio_mean_an = &
                ratio_mean_an/ntimes_an

           ratio_sdev_an=0.0
           do jtimeslot=1,ntimeslots
              if (abs(statarray(jgroup,jtimeslot,2,jdomain,jlevel,8))> &
                   1.0e-6) then
                 ratio_sdev_an = &
                      ratio_sdev_an + &
                      ((statarray(jgroup,jtimeslot,1,jdomain,jlevel,8) / &
                      statarray(jgroup,jtimeslot,2,jdomain,jlevel,8)) - &
                      ratio_mean_an)**2
              end if
           end do
           if (ntimes_an>1) then
              ratio_sdev_an = sqrt ( ratio_sdev_an / (ntimes_an-1) )
              stat95_an(jgroup,jdomain,jlevel) = &
                   1.96*ratio_sdev_an/sqrt(1.0*ntimes_an)   ! 95% indeed
!                  2.575*ratio_sdev_an/sqrt(1.0*ntimes_an)   ! 99%
!                  1.645*ratio_sdev_an/sqrt(1.0*ntimes_an)   ! 90%
           else
              ratio_sdev_an = 0.0
              stat95_an(jgroup,jdomain,jlevel) = 0.0
           end if

        end do
     end do
  end do
!-------------

end subroutine average


function chisq95(ndf,lower) result(value)
  use parameters, only : jpim, jprb
  implicit none
  integer(kind=jpim) :: ndf, i, j, k
  logical :: lower
  real(kind=jprb) :: value, chisq
  integer, parameter :: nlarges=57
  real(kind=jprb), dimension(249) :: tab_lower, tab_upper
  real(kind=jprb), dimension(nlarges) :: tab_large_lower, tab_large_upper
  integer(kind=jprb), dimension(nlarges) :: larges

  tab_lower(:)=0.0_jprb
  tab_upper(:)=0.0_jprb

  tab_lower(1:249) = (/ &
   5.024,   7.378,   9.348,  11.143,  12.833,  14.449,  16.013,  17.535,  19.023,  20.483, &
  21.920,  23.337,  24.736,  26.119,  27.488,  28.845,  30.191,  31.526,  32.852,  34.170, &
  35.479,  36.781,  38.076,  39.364,  40.646,  41.923,  43.195,  44.461,  45.722,  46.979, &
  48.232,  49.480,  50.725,  51.966,  53.203,  54.437,  55.668,  56.896,  58.120,  59.342, &
  60.561,  61.777,  62.990,  64.201,  65.410,  66.617,  67.821,  69.023,  70.222,  71.420, &
  72.616,  73.810,  75.002,  76.192,  77.380,  78.567,  79.752,  80.936,  82.117,  83.298, &
  84.476,  85.654,  86.830,  88.004,  89.177,  90.349,  91.519,  92.689,  93.856,  95.023, &
  96.189,  97.353,  98.516,  99.678, 100.839, 101.999, 103.158, 104.316, 105.473, 106.629, &
 107.783, 108.937, 110.090, 111.242, 112.393, 113.544, 114.693, 115.841, 116.989, 118.136, &
 119.282, 120.427, 121.571, 122.715, 123.858, 125.000, 126.141, 127.282, 128.422, 129.561, &
 130.700, 131.838, 132.975, 134.111, 135.247, 136.382, 137.517, 138.651, 139.784, 140.917, &
 142.049, 143.180, 144.311, 145.441, 146.571, 147.700, 148.829, 149.957, 151.084, 152.211, &
 153.338, 154.464, 155.589, 156.714, 157.839, 158.962, 160.086, 161.209, 162.331, 163.453, &
 164.575, 165.696, 166.816, 167.936, 169.056, 170.175, 171.294, 172.412, 173.530, 174.648, &
 175.765, 176.882, 177.998, 179.114, 180.229, 181.344, 182.459, 183.573, 184.687, 185.800, &
 186.914, 188.026, 189.139, 190.251, 191.362, 192.474, 193.584, 194.695, 195.805, 196.915, &
 198.025, 199.134, 200.243, 201.351, 202.459, 203.567, 204.675, 205.782, 206.889, 207.995, &
 209.102, 210.208, 211.313, 212.419, 213.524, 214.628, 215.733, 216.837, 217.941, 219.044, &
 220.148, 221.251, 222.353, 223.456, 224.558, 225.660, 226.761, 227.863, 228.964, 230.064, &
 231.165, 232.265, 233.365, 234.465, 235.564, 236.664, 237.763, 238.861, 239.960, 241.058, &
 242.156, 243.254, 244.351, 245.448, 246.545, 247.642, 248.739, 249.835, 250.931, 252.027, &
 253.122, 254.218, 255.313, 256.408, 257.503, 258.597, 259.691, 260.785, 261.879, 262.973, &
 264.066, 265.159, 266.252, 267.345, 268.438, 269.530, 270.622, 271.714, 272.806, 273.898, &
 274.989, 276.080, 277.171, 278.262, 279.352, 280.443, 281.533, 282.623, 283.713, 284.802, &
 285.892, 286.981, 288.070, 289.159, 290.248, 291.336, 292.425, 293.513, 294.601 /)

  tab_upper(1:249) = (/ &
 9.82e-4, 5.06e-2,   0.216,   0.484,   0.831,   1.237,   1.690,   2.180,   2.700,   3.247, &
   3.816,   4.404,   5.009,   5.629,   6.262,   6.908,   7.564,   8.231,   8.907,   9.591, &
  10.283,  10.982,  11.689,  12.401,  13.120,  13.844,  14.573,  15.308,  16.047,  16.791, &
  17.539,  18.291,  19.047,  19.806,  20.569,  21.336,  22.106,  22.878,  23.654,  24.433, &
  25.215,  25.999,  26.785,  27.575,  28.366,  29.160,  29.956,  30.755,  31.555,  32.357, &
  33.162,  33.968,  34.776,  35.586,  36.398,  37.212,  38.027,  38.844,  39.662,  40.482, &
  41.303,  42.126,  42.950,  43.776,  44.603,  45.431,  46.261,  47.092,  47.924,  48.758, &
  49.592,  50.428,  51.265,  52.103,  52.942,  53.782,  54.623,  55.466,  56.309,  57.153, &
  57.998,  58.845,  59.692,  60.540,  61.389,  62.239,  63.089,  63.941,  64.793,  65.647, &
  66.501,  67.356,  68.211,  69.068,  69.925,  70.783,  71.642,  72.501,  73.361,  74.222, &
  75.083,  75.946,  76.809,  77.672,  78.536,  79.401,  80.267,  81.133,  82.000,  82.867, &
  83.735,  84.604,  85.473,  86.342,  87.213,  88.084,  88.955,  89.827,  90.700,  91.573, &
  92.446,  93.320,  94.195,  95.070,  95.946,  96.822,  97.698,  98.576,  99.453, 100.331, &
 101.210, 102.089, 102.968, 103.848, 104.729, 105.609, 106.491, 107.372, 108.254, 109.137, &
 110.020, 110.903, 111.787, 112.671, 113.556, 114.441, 115.326, 116.212, 117.098, 117.985, &
 118.871, 119.759, 120.646, 121.534, 122.423, 123.312, 124.201, 125.090, 125.980, 126.870, &
 127.761, 128.651, 129.543, 130.434, 131.326, 132.218, 133.111, 134.003, 134.897, 135.790, &
 136.684, 137.578, 138.472, 139.367, 140.262, 141.157, 142.053, 142.949, 143.845, 144.741, &
 145.638, 146.535, 147.432, 148.330, 149.228, 150.126, 151.024, 151.923, 152.822, 153.721, &
 154.621, 155.521, 156.421, 157.321, 158.221, 159.122, 160.023, 160.925, 161.826, 162.728, &
 163.630, 164.532, 165.435, 166.338, 167.241, 168.144, 169.047, 169.951, 170.855, 171.759, &
 172.664, 173.568, 174.473, 175.378, 176.283, 177.189, 178.095, 179.001, 179.907, 180.813, &
 181.720, 182.627, 183.534, 184.441, 185.348, 186.256, 187.164, 188.072, 188.980, 189.889, &
 190.797, 191.706, 192.615, 193.524, 194.434, 195.343, 196.253, 197.163, 198.073, 198.984, &
 199.894, 200.805, 201.716, 202.627, 203.539, 204.450, 205.362, 206.274, 207.186 /)

  larges(1:nlarges)=(/ &
     250, 300, 350, 400, 450, &
     500, 550, 600, 650, 700, &
     750, 800, 850, 900, 950, &
     1000, 2000, 3000, 4000, 5000, &
     6000, 7000, 8000, 9000, &
     10000, 20000, 30000, 40000, 50000, &
     60000, 70000, 80000, 90000, &
     100000, 200000, 300000, 400000, 500000, &
     600000, 700000, 800000, 900000, &
     1000000, 2000000, 3000000, 4000000, 5000000, &
     6000000, 7000000, 8000000, 9000000, &
     10000000, &
     20000000, &
     30000000, &
     50000000, &
     70000000, &
     100000000 /)

  tab_large_lower(1:nlarges) = (/ &
    295.689, 349.874, 403.723, 457.305, 510.670, &
    563.852, 616.878, 669.769, 722.542, 775.211, &
    827.785, 880.275, 932.689, 985.032, 1037.311, &
    1089.5309, 2125.8422, 3153.7034, 4177.1910, 5197.8837, &
    6216.5912, 7233.7944, 8249.8067, 9264.8458, &
    10279.0701, 20393.8835, 30481.9824, 40556.2531, 50621.6868, &
    60680.8433, 70735.2433, 80785.8776, 90833.4343, &
    100878.4146, 201241.4819, 301520.0729, 401754.9359, 501961.8542, &
    602148.9226, 702320.9496, 802481.0684, 902631.4553, &
    1002773.6946, 2003921.8083, 3004802.7849, 4005545.4821, 5006199.8102, &
    6006791.3697, 7007335.3602, 8007841.6923, 9008317.2553, &
    10008767.0554, &
    20012397.6540, &
    30015183.4720, &
    50019601.3171, &
    70023191.8682, &
    100027718.8242 /)

  tab_large_upper(1:nlarges) = (/ &
    208.098, 253.912, 300.064, 346.482, 393.118, &
    439.936, 486.910, 534.019, 581.245, 628.577, &
    676.003, 723.513, 771.099, 818.756, 866.477, &
    914.257, 1877.9460, 2850.0849, 3826.5973, 4805.9047, &
    5787.1972, 6769.9939, 7753.9816, 8738.9425, &
    9724.7183, 19609.9048, 29521.8057, 39447.5349, 49382.1011, &
    59322.9444, 69268.5443, 79217.9099, 89170.3530, &
    99125.3726, 198762.3040, 298483.7116, 398248.8474, 498041.9275, &
    597854.8590, 697682.8313, 797522.7097, 897372.3225, &
    997230.0804, 1996081.9616, 2995200.9586, 3994458.2769, 4993803.9461, &
    5993212.4685, 6992668.4047, 7992161.9403, 8991686.4904, &
    9991236.8703, &
    19987605.9946, &
    29984819.0959, &
    49980407.7777, &
    69976806.3161, &
    99972267.3101 /)

  k=min(ndf,larges(nlarges))

  if (lower) then

    if (k<=1) then
      chisq=tab_lower(1)
      value=sqrt(1.0_jprb*k/chisq)
    else if (k<=249_jpim) then
      chisq=tab_lower(k)
      value=sqrt(1.0_jprb*k/chisq)
    else
      i=1_jpim
      j=2_jpim
      do while (larges(j)<k)
        i=j
        j=j+1_jpim
      end do
      chisq=tab_large_lower(i)+ &
        ((k-larges(i))*1.0_jprb/(larges(j)-larges(i))) * &
        (tab_large_lower(j)-tab_large_lower(i))
      value=sqrt(1.0_jprb*k/chisq)
    endif

  else

    if (k<=1) then
      chisq=tab_upper(1)
      value=sqrt(1.0_jprb*k/chisq)
    else if (k<=249_jpim) then
      chisq=tab_upper(k)
      value=sqrt(1.0_jprb*k/chisq)
    else
      i=1_jpim
      j=2_jpim
      do while (larges(j)<k)
        i=j
        j=j+1_jpim
      end do
      chisq=tab_large_upper(i)+ &
        ((k-larges(i))*1.0_jprb/(larges(j)-larges(i))) * &
        (tab_large_upper(j)-tab_large_upper(i))
      value=sqrt(1.0_jprb*k/chisq)
    endif

  endif

end function chisq95


function chisq99(ndf,lower) result(value)
  use parameters, only : jpim, jprb
  implicit none
  integer(kind=jpim) :: ndf, i, j, k
  logical :: lower
  real(kind=jprb) :: value, chisq
  integer, parameter :: nlarges=57
  real(kind=jprb), dimension(249) :: tab_lower, tab_upper
  real(kind=jprb), dimension(nlarges) :: tab_large_lower, tab_large_upper
  integer(kind=jprb), dimension(nlarges) :: larges

  tab_lower(:)=0.0_jprb
  tab_upper(:)=0.0_jprb

  tab_lower(1:249) = (/ &	
   7.879,  10.597,  12.838,  14.860,  16.750,  18.548,  20.278,  21.955,  23.589,  25.188, &
  26.757,  28.300,  29.819,  31.319,  32.801,  34.267,  35.718,  37.156,  38.582,  39.997, &
  41.401,  42.796,  44.181,  45.559,  46.928,  48.290,  49.645,  50.993,  52.336,  53.672, &
  55.003,  56.328,  57.648,  58.964,  60.275,  61.581,  62.883,  64.181,  65.476,  66.766, &
  68.053,  69.336,  70.616,  71.893,  73.166,  74.437,  75.704,  76.969,  78.231,  79.490, &
  80.747,  82.001,  83.253,  84.502,  85.749,  86.994,  88.236,  89.477,  90.715,  91.952, &
  93.186,  94.419,  95.649,  96.878,  98.105,  99.330, 100.554, 101.776, 102.996, 104.215, &
 105.432, 106.648, 107.862, 109.074, 110.286, 111.495, 112.704, 113.911, 115.117, 116.321, &
 117.524, 118.726, 119.927, 121.126, 122.325, 123.522, 124.718, 125.913, 127.106, 128.299, &
 129.491, 130.681, 131.871, 133.059, 134.247, 135.433, 136.619, 137.803, 138.987, 140.169, &
 141.351, 142.532, 143.712, 144.891, 146.070, 147.247, 148.424, 149.599, 150.774, 151.948, &
 153.122, 154.294, 155.466, 156.637, 157.808, 158.977, 160.146, 161.314, 162.481, 163.648, &
 164.814, 165.980, 167.144, 168.308, 169.471, 170.634, 171.796, 172.957, 174.118, 175.278, &
 176.438, 177.597, 178.755, 179.913, 181.070, 182.226, 183.382, 184.538, 185.693, 186.847, &
 188.001, 189.154, 190.306, 191.458, 192.610, 193.761, 194.912, 196.062, 197.211, 198.360, &
 199.509, 200.657, 201.804, 202.951, 204.098, 205.244, 206.390, 207.535, 208.680, 209.824, &
 210.968, 212.111, 213.254, 214.396, 215.539, 216.680, 217.821, 218.962, 220.102, 221.242, &
 222.382, 223.521, 224.660, 225.798, 226.936, 228.074, 229.211, 230.347, 231.484, 232.620, &
 233.755, 234.891, 236.026, 237.160, 238.294, 239.428, 240.561, 241.694, 242.827, 243.959, &
 245.091, 246.223, 247.354, 248.485, 249.616, 250.746, 251.876, 253.006, 254.135, 255.264, &
 256.393, 257.521, 258.649, 259.777, 260.904, 262.031, 263.158, 264.285, 265.411, 266.537, &
 267.662, 268.788, 269.912, 271.037, 272.162, 273.286, 274.409, 275.533, 276.656, 277.779, &
 278.902, 280.024, 281.146, 282.268, 283.390, 284.511, 285.632, 286.753, 287.874, 288.994, &
 290.114, 291.234, 292.353, 293.472, 294.591, 295.710, 296.828, 297.947, 299.065, 300.182, &
 301.300, 302.417, 303.534, 304.651, 305.767, 306.883, 307.999, 309.115, 310.231 /)

  tab_upper(1:249) = (/ &
 3.93e-5,  0.0100,  0.0717,   0.207,   0.412,   0.676,   0.989,   1.344,   1.735,   2.156, &
   2.603,   3.074,   3.565,   4.075,   4.601,   5.142,   5.697,   6.265,   6.844,   7.434, &
   8.034,   8.643,   9.260,   9.886,  10.520,  11.160,  11.808,  12.461,  13.121,  13.787, &
  14.458,  15.134,  15.815,  16.501,  17.192,  17.887,  18.586,  19.289,  19.996,  20.707, &
  21.421,  22.138,  22.859,  23.584,  24.311,  25.041,  25.775,  26.511,  27.249,  27.991, &
  28.735,  29.481,  30.230,  30.981,  31.735,  32.490,  33.248,  34.008,  34.770,  35.534, &
  36.301,  37.068,  37.838,  38.610,  39.383,  40.158,  40.935,  41.713,  42.494,  43.275, &
  44.058,  44.843,  45.629,  46.417,  47.206,  47.997,  48.788,  49.582,  50.376,  51.172, &
  51.969,  52.767,  53.567,  54.368,  55.170,  55.973,  56.777,  57.582,  58.389,  59.196, &
  60.005,  60.815,  61.625,  62.437,  63.250,  64.063,  64.878,  65.694,  66.510,  67.328, &
  68.146,  68.965,  69.785,  70.606,  71.428,  72.251,  73.075,  73.899,  74.724,  75.550, &
  76.377,  77.204,  78.033,  78.862,  79.692,  80.522,  81.353,  82.185,  83.018,  83.852, &
  84.686,  85.520,  86.356,  87.192,  88.029,  88.866,  89.704,  90.543,  91.382,  92.222, &
  93.063,  93.904,  94.746,  95.588,  96.431,  97.275,  98.119,  98.964,  99.809, 100.655, &
 101.501, 102.348, 103.196, 104.044, 104.892, 105.741, 106.591, 107.441, 108.291, 109.142, &
 109.994, 110.846, 111.698, 112.551, 113.405, 114.259, 115.113, 115.968, 116.823, 117.679, &
 118.536, 119.392, 120.249, 121.107, 121.965, 122.823, 123.682, 124.541, 125.401, 126.261, &
 127.122, 127.983, 128.844, 129.706, 130.568, 131.430, 132.293, 133.157, 134.020, 134.884, &
 135.749, 136.614, 137.479, 138.344, 139.210, 140.077, 140.943, 141.810, 142.678, 143.545, &
 144.413, 145.282, 146.150, 147.020, 147.889, 148.759, 149.629, 150.499, 151.370, 152.241, &
 153.112, 153.984, 154.856, 155.728, 156.601, 157.474, 158.347, 159.221, 160.095, 160.969, &
 161.843, 162.718, 163.593, 164.469, 165.344, 166.220, 167.096, 167.973, 168.850, 169.727, &
 170.604, 171.482, 172.360, 173.238, 174.116, 174.995, 175.874, 176.753, 177.633, 178.512, &
 179.392, 180.273, 181.153, 182.034, 182.915, 183.796, 184.678, 185.560, 186.442, 187.324, &
 188.207, 189.090, 189.973, 190.856, 191.739, 192.623, 193.507, 194.391, 195.276 /)

  larges(1:nlarges)=(/ &
     250, 300, 350, 400, 450, &
     500, 550, 600, 650, 700, &
     750, 800, 850, 900, 950, &
     1000, 2000, 3000, 4000, 5000, &
     6000, 7000, 8000, 9000, &
     10000, 20000, 30000, 40000, 50000, &
     60000, 70000, 80000, 90000, &
     100000, 200000, 300000, 400000, 500000, &
     600000, 700000, 800000, 900000, &
     1000000, 2000000, 3000000, 4000000, 5000000, &
     6000000, 7000000, 8000000, 9000000, &
     10000000, &
     20000000, &
     30000000, &
     50000000, &
     70000000, &
     100000000 /)

  tab_large_lower(1:nlarges) = (/ &
       311.346, 366.844, 421.900, 476.606, 531.026, &
       585.207, 639.183, 692.982, 746.625, 800.131, &
       853.514, 906.786, 959.957, 1013.036, 1066.031, &
       1118.9480, 2166.6642, 3203.2777, 4234.1443, 5261.3382, &
       6285.9233, 7308.5316, 8329.5749, 9349.3392, &
       10368.0327, 20518.9213, 30634.7021, 40732.3097, 50818.3036, &
       60896.0481, 70967.5415, 81034.0859, 91096.5857, &
       101155.6995, 201632.8485, 301998.9772, 402307.6374, 502579.5722, &
       602825.4198, 703051.4997, 803261.9296, 903459.5694, &
       1003646.5017, 2005155.3597, 3006313.1397, 4007289.1895, 5008149.1029, &
       6008926.5230, 7009641.4291, 8010306.8440, 9010931.8184, &
       10011522.9336, &
       20016294.1511, &
       30019955.1471, &
       50025760.6849, &
       70030479.1915, &
       100036428.1427 /)

  tab_large_upper(1:nlarges) = (/ &
       196.161, 240.663, 285.608, 330.903, 376.483, &
       422.303, 468.328, 514.529, 560.885, 607.380, &
       653.997, 700.725, 747.554, 794.475, 841.480, &
       888.5635, 1840.8480, 2804.2347, 3773.3683, 4746.1744, &
       5721.5893,6698.9809, 7677.9376, 8658.1733, &
       9639.4797, 19488.5907, 29372.8095, 39275.2014, 49189.2068, &
       59111.4618, 69039.9680, 78973.4231, 88910.9227, &
       98851.8084, 198374.6539, 298008.5201, 397699.8547, 497427.9135, &
       597182.0653, 696955.9829, 796745.5412, 896547.9001, &
       996360.9571, 1994852.0786, 2993694.1891, 3992718.2024, 4991858.2826, &
       5991081.1855, 6990365.9876, 7989700.0410, 8989075.5260, &
       9988485.1384, &
       19983712.8081, &
       29980047.4566, &
       49974268.2330, &
       69969505.5873, &
       99963507.6223 /)


  k=min(ndf,larges(nlarges))

  if (lower) then

    if (k<=1) then
      chisq=tab_lower(1)
      value=sqrt(1.0_jprb*k/chisq)
    else if (k<=249_jpim) then
      chisq=tab_lower(k)
      value=sqrt(1.0_jprb*k/chisq)
    else
      i=1_jpim
      j=2_jpim
      do while (larges(j)<k)
        i=j
        j=j+1_jpim
      end do
      chisq=tab_large_lower(i)+ &
        ((k-larges(i))*1.0_jprb/(larges(j)-larges(i))) * &
        (tab_large_lower(j)-tab_large_lower(i))
      value=sqrt(1.0_jprb*k/chisq)
    endif

  else

    if (k<=1) then
      chisq=tab_upper(1)
      value=sqrt(1.0_jprb*k/chisq)
    else if (k<=249_jpim) then
      chisq=tab_upper(k)
      value=sqrt(1.0_jprb*k/chisq)
    else
      i=1_jpim
      j=2_jpim
      do while (larges(j)<k)
        i=j
        j=j+1_jpim
      end do
      chisq=tab_large_upper(i)+ &
        ((k-larges(i))*1.0_jprb/(larges(j)-larges(i))) * &
        (tab_large_upper(j)-tab_large_upper(i))
      value=sqrt(1.0_jprb*k/chisq)
    endif

  endif

end function chisq99
