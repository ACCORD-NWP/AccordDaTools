module array_definitions

  use parameters, only : &
    jpim, jprb, maxtimes, nobgroups, &
    nlevels_temp, nlevels_rttov, niasich
  implicit none

  character(len=4) :: expid, conid
  character(len=10) :: cdate_first, cdate_last
  character(len=10), dimension(nobgroups) :: groupid

  integer(kind=jpim) :: nlevels
  integer(kind=jpim) :: ntimeslots
  integer(kind=jpim), dimension(maxtimes) :: idate_array
  integer(kind=jpim) :: irank

  real(kind=jprb), dimension(nlevels_temp) :: levels_temp
  real(kind=jprb), dimension(nlevels_rttov) :: levels_rttov
  real(kind=jprb), dimension(niasich) :: iasi_peakpres

  real(kind=jprb), dimension(:,:,:,:,:,:), allocatable :: statarray
  real(kind=jprb), dimension(:,:,:,:,:), allocatable :: statistic
  real(kind=jprb), dimension(:,:,:,:,:), allocatable :: stat95l
  real(kind=jprb), dimension(:,:,:,:,:), allocatable :: stat95u
  real(kind=jprb), dimension(:,:,:,:,:), allocatable :: stat99l
  real(kind=jprb), dimension(:,:,:,:,:), allocatable :: stat99u
  real(kind=jprb), dimension(:,:,:,:,:), allocatable :: hemispheric
  real(kind=jprb), dimension(:,:,:,:), allocatable :: global
  real(kind=jprb), dimension(:,:,:), allocatable :: stat95_bg
  real(kind=jprb), dimension(:,:,:), allocatable :: stat95_an

end module array_definitions
