program cv_header_list
!  
  implicit none
!
!   Geometry definition
!
  real(kind=8)             :: gsize_in
  integer(kind=4)          :: nlev_nl
  integer(kind=4)          :: jpnlev_nl
  parameter (jpnlev_nl=200)
  real(kind=8)             :: ahalf_nl(jpnlev_nl+1),bhalf_nl(jpnlev_nl+1)
  integer(kind=4)          :: printlev
  namelist/namjbconv/ gsize_in,nlev_nl,ahalf_nl,bhalf_nl,printlev

  integer(kind=4)          :: nlon_in,nlat_in
  real(kind=8)             :: lon0_in,lat0_in
  real(kind=8)             :: lon1_in,lat1_in,lon2_in,lat2_in
  real(kind=8)             :: afull_nl(jpnlev_nl),bfull_nl(jpnlev_nl)
  real(kind=8)             :: pfull_nl(jpnlev_nl)
! 
!   Variables read from input file or written to output file
!
  character                :: clid*10,clcom*70
  integer(kind=4)          :: iorig,idate,itime,inbset
  integer(kind=4)          :: inbmat,iweight,itypmat,isetdist,ilendef
  integer(kind=4)          :: idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
  real(kind=8)             :: zlon1,zlat1,zlon2,zlat2,zlon0,zlat0
  integer(kind=4)          :: idgl,idlon,idgux,idlux,iismax,imsmax,iflevg,ichkwd 
  real(kind=8),allocatable :: zfcovtps_in(:,:,:),zfcovq_in(:,:,:),zfcovp_in(:,:,:),zfcovd_in(:,:,:)
  real(kind=8),allocatable :: zpdat_in(:)
  real(kind=8),allocatable :: sdiv_in(:,:,:), stps_in(:,:,:),shum_in(:,:,:), bfact_in(:)
  real(kind=8),allocatable :: bfact_iso(:)
!
!   File unit numbers
!
  integer(kind=4)          :: nuljb_in,nulout,nulnam
!
!   Other local variables
!
  integer(kind=4)          :: itrunc,nflevg
  integer(kind=4)          :: jn,jk,jj,jl
  integer(kind=4)          :: nsmax_jb_in,nmsmax_jb_in
  integer(kind=4),allocatable :: isorel2_in(:),isodor2_in(:)
  integer(kind=4)          ::   ispec2_in,iii 
  character                ::  fnspdens*15
  real(kind=8)             :: zsumcov,zsumvar1,zsumvar2,pi,zscale
!
!   Initialize some work variables
!
  nuljb_in=10
  nulout=06
  nulnam=05
  printlev=1
  pi=atan(1.)*4.

!   Open input covariance file
!
  open(nuljb_in,file='stabal96.cv',form='unformatted')
!
!   Read GSA file header
!
  read(nuljb_in) clid
  read(nuljb_in) clcom
  read(nuljb_in) iorig,idate,itime,inbset
  write(nulout,*) ''
!
!   Read  GSA set 0 header : model geometry definition (LAM case !)
!
  read(nuljb_in) inbmat,iweight,itypmat,isetdist,ilendef
  read(nuljb_in) idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
  read(nuljb_in)
  read(nuljb_in)
  read(nuljb_in) zlon1,zlat1,zlon2,zlat2,zlon0,zlat0,&
       &       idgl,idlon,idgux,idlux,iismax,imsmax,iflevg,ichkwd

  write(nulout,*) 'JPDAYS       :: ',iweight
  write(nulout,*) 'SW(lon-lat)  :: ',zlon1,zlat1
  write(nulout,*) 'NE(lon-lat)  :: ',zlon2,zlat2
  write(nulout,*) 'REF(lon-lat) :: ',zlon0,zlat0
  write(nulout,*) 'NDGL         :: ',idgl
  write(nulout,*) 'NDLON        :: ',idlon
  write(nulout,*) 'NDGUX        :: ',idgux
  write(nulout,*) 'NDLUX        :: ',idlux
  write(nulout,*) 'NSMAX        :: ',iismax
  write(nulout,*) 'NMSMAX       :: ',imsmax
  write(nulout,*) 'NFLEVG       :: ',iflevg
!
  stop
end program cv_header_list
