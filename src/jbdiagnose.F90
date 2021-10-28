!  
      implicit none
!
!   Geometry definition
!
      integer(kind=4)   :: nlon_in,nlat_in
      real(kind=8)      :: lon0_in,lat0_in,gsize_in
      real(kind=8)      :: lon1_in,lat1_in,lon2_in,lat2_in
      integer(kind=4)   :: jpnlev_nl
      parameter (jpnlev_nl=200)
      integer(kind=4)   ::  nlev_nl
      real(kind=8)      ::  ahalf_nl(jpnlev_nl+1),bhalf_nl(jpnlev_nl+1)
      real(kind=8)      ::  afull_nl(jpnlev_nl),bfull_nl(jpnlev_nl)
      real(kind=8)      ::  pfull_nl(jpnlev_nl)
      namelist/namjbconv/ gsize_in,nlev_nl,ahalf_nl,bhalf_nl
! 
!   Variables read from input file or written to output file
!
      character :: clid*10,clcom*70
      integer(kind=4)   :: iorig,idate,itime,inbset
      integer(kind=4)   :: inbmat,iweight,itypmat,isetdist,ilendef
      integer(kind=4)   :: idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      real(kind=8)      :: zlon1,zlat1,zlon2,zlat2,zlon0,zlat0
      integer(kind=4)   :: idgl,idlon,idgux,idlux,iismax,imsmax,&
           &               iflevg,ichkwd 
      real(kind=8),allocatable      :: zfcovtps_in(:,:,:),&
                   &       zfcovq_in(:,:,:),&
                   &       zfcovp_in(:,:,:),zfcovd_in(:,:,:)
      real(kind=8),allocatable      :: zpdat_in(:)
!
      real(kind=8), allocatable ::  sdiv_in(:,:,:), stps_in(:,:,:),&
                   &                shum_in(:,:,:), bfact_in(:)
!
      real(kind=8), allocatable ::   bfact_iso(:)
!
!   File unit numbers
!
      integer(kind=4)   :: nuljb_in,nulout,nulnam
!
!   Covariance for linearized geopotential
!
      real(kind=8),allocatable :: cov_ppp(:,:,:)
!
!   Variance vectors involved in balance operator calculation
!
      real(kind=8),allocatable :: var_vor(:,:)
      real(kind=8),allocatable :: var_ppp(:,:)
      real(kind=8),allocatable :: var_div_bal_ppp(:,:)
      real(KIND=8),allocatable :: var_div_u(:,:)
      real(KIND=8),allocatable :: var_div_total(:,:)
      real(kind=8),allocatable :: var_tps_bal_ppp(:,:)
      real(kind=8),allocatable :: var_tps_bal_div(:,:)
      real(kind=8),allocatable :: var_tps_u(:,:)
      real(kind=8),allocatable :: var_tps_total(:,:)
      real(kind=8),allocatable :: var_hum_bal_ppp(:,:)
      real(kind=8),allocatable :: var_hum_bal_div(:,:)
      real(kind=8),allocatable :: var_hum_bal_tps(:,:)
      real(kind=8),allocatable :: var_hum_u(:,:)
      real(kind=8),allocatable :: var_hum_total(:,:)
!
      real(kind=8),allocatable :: ave_var_vor(:)
      real(kind=8),allocatable :: ave_var_ppp(:)
      real(kind=8),allocatable :: ave_var_div_bal_ppp(:)
      real(KIND=8),allocatable :: ave_var_div_u(:)
      real(KIND=8),allocatable :: ave_var_div_total(:)
      real(kind=8),allocatable :: ave_var_tps_bal_ppp(:)
      real(kind=8),allocatable :: ave_var_tps_bal_div(:)
      real(kind=8),allocatable :: ave_var_tps_u(:)
      real(kind=8),allocatable :: ave_var_tps_total(:)
      real(kind=8),allocatable :: ave_var_hum_bal_ppp(:)
      real(kind=8),allocatable :: ave_var_hum_bal_div(:)
      real(kind=8),allocatable :: ave_var_hum_bal_tps(:)
      real(kind=8),allocatable :: ave_var_hum_u(:)
      real(kind=8),allocatable :: ave_var_hum_total(:)
!
!   Percentages of explained variances as functions of vertical level
!
      real(kind=8),allocatable :: expl_div_ppp(:)
      real(kind=8),allocatable :: expl_tps_ppp(:)
      real(kind=8),allocatable :: expl_tps_div(:)
      real(kind=8),allocatable :: expl_hum_ppp(:)
      real(kind=8),allocatable :: expl_hum_div(:)
      real(kind=8),allocatable :: expl_hum_tps(:)
!
!   Percentages of explained variances as functions of horizontal wave
!   number
!
      real(kind=8),allocatable :: expl_div_ppp_hor(:)
      real(kind=8),allocatable :: expl_tps_ppp_hor(:)
      real(kind=8),allocatable :: expl_tps_div_hor(:)
      real(kind=8),allocatable :: expl_hum_ppp_hor(:)
      real(kind=8),allocatable :: expl_hum_div_hor(:)
      real(kind=8),allocatable :: expl_hum_tps_hor(:)
!
!   Other local variables
!
      integer(kind=4)   :: itrunc,nflevg
      integer(kind=4)   :: jn,jk,jj,jl
      real(kind=8)      :: zdummy,zeps
      real(kind=8)      :: lx_in,ly_in,lxy_in
      integer(kind=4)   :: nsmax_jb_in,nmsmax_jb_in
      integer(kind=4),allocatable :: isorel2_in(:),isodor2_in(:)
      integer(kind=4)   ::   ispec2_in,iii 
      character ::  fnspdens*15
      real(kind=8)      :: zsumcov,zsumvar1,zsumvar2
!
!   Initialize some work variables
!
      zeps=0.0000001
      nuljb_in=10
      nulout=06
      nulnam=05
!
!   Read namelist
!
      read(nulnam,namjbconv)
!
      do jj=1,nlev_nl
         afull_nl(jj) = 0.5*(ahalf_nl(jj)+ahalf_nl(jj+1))
         bfull_nl(jj) = 0.5*(bhalf_nl(jj)+bhalf_nl(jj+1))
         pfull_nl(jj) = afull_nl(jj) + 100000.*bfull_nl(jj)
      end do

!   Open input covariance file
!
      open(nuljb_in,file='stabal96.cv',form='unformatted')
!
!   Read GSA file header
!
      read(nuljb_in) clid
      write(nulout,*) 'GSA ID=',clid
      read(nuljb_in) clcom
      write(nulout,*) 'Description : ',clcom
      read(nuljb_in) iorig,idate,itime,inbset
      write(nulout,*) ' Center=',iorig,' Date=',idate,' Time=',itime
!
!   Read  GSA set 0 header : model geometry definition (LAM case !)
!
      read(nuljb_in) inbmat,iweight,itypmat,isetdist,ilendef
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
                &     ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      read(nuljb_in) idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
                &     ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      read(nuljb_in) zlon1,zlat1,zlon2,zlat2,zlon0,zlat0,&
           &       idgl,idlon,idgux,idlux,iismax,imsmax,iflevg,&
           &       ichkwd
      write(nulout,*) ' zlon1=',zlon1,' zlat1=',zlat1,' zlon2=',zlon2
      write(nulout,*) ' zlat2=',zlat2,' zlon0=',zlon0,' zlat0=',zlat0
      write(nulout,*) ' idgl=',idgl,' idlon=',idlon
      write(nulout,*) ' idgux=',idgux,' idlux=',idlux
      write(nulout,*) ' iismax=', iismax,' imsmax=',imsmax
      write(nulout,*) ' iflevg=',iflevg,' ichkwd=',ichkwd
!
!  Check correspondence between number of levels in namelist
!  and number of levels in input statistics file
!
      if( iflevg.ne.nlev_nl ) then
         write(nulout,*)' Number of levels in namelist and input file disagree'
         stop
      end if
!
!  Set model geometry 
!
      nlon_in = idlon
      nlat_in = idgl
      write(6,*)'nlon_in,nlat_in=',nlon_in,nlat_in
      lx_in = float(nlon_in)*gsize_in
      ly_in = float(nlat_in)*gsize_in
      lxy_in = max(lx_in,ly_in)
      lon1_in = zlon1
      lat1_in = zlat1
      lon2_in = zlon2
      lat2_in = zlat2
      lon0_in = zlon0
      lat0_in = zlat0
      nsmax_jb_in=iismax
      nmsmax_jb_in=imsmax      
!
!   Set truncation of input covariance spectrum
!   and allocate space for the input covariances
!
      itrunc=iismax
      nflevg=iflevg
      allocate(zfcovtps_in(nflevg+1,nflevg+1,0:itrunc))
      allocate(zfcovq_in(nflevg,nflevg,0:itrunc))
      allocate(zfcovd_in(nflevg,nflevg,0:itrunc))
      allocate(zfcovp_in(nflevg,nflevg,0:itrunc))
      allocate(zpdat_in(nflevg+1))
!
!   Read  GSA set 1: reading vorticity covariance matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &                    ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,' itypdi2=',&
          &                    itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)zdummy
         read(nuljb_in)((zfcovp_in(jj,jk,jn),jj=1,nflevg),jk=1,nflevg),&
              &                 ichkwd
      end do
!
!   Read  GSA set 2: reading unbalanced divergence covariance matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
         &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
         &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)zdummy
         read(nuljb_in)((zfcovd_in(jj,jk,jn),jj=1,nflevg),jk=1,nflevg),&
            &            ichkwd
      end do
!
!   Read  GSA set 1: reading T,lnps covariance matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &               ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,' itypdi2=',&
           &                   itypdi2
      read(nuljb_in) (zpdat_in(jj),jj=1,idim1)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) zdummy
         read(nuljb_in)&
       &    ((zfcovtps_in(jj,jk,jn),jj=1,nflevg+1),jk=1,nflevg+1),ichkwd
      end do
!
!   Read  GSA set 1: reading unbalanced q covariance matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
            &           ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
            &           ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)zdummy
         read(nuljb_in)((zfcovq_in(jj,jk,jn),jj=1,nflevg),jk=1,nflevg),&
               &          ichkwd
      end do
!
!   Close input covariance file
!
      close(nuljb_in)
!
! Write diagnostic spectral density files
!
!  Vorticity
!
      do jk=1,nflevg
         write(fnspdens,1000)jk
 1000    format('diag_spdensPP',i2.2)
         open(unit=50,file=fnspdens,form='formatted')
         do jn=1,nsmax_jb_in
            write(50,*)jn,&
              &  gsize_in*float(max(nlon_in,nlat_in))/1000./float(jn),&
              &  zfcovp_in(jk,jk,jn)
         end do
         close(unit=50)
      end do
!
!  Divergence
!
      do jk=1,nflevg
         write(fnspdens,1001)jk
 1001    format('diag_spdensDD',i2.2)
         open(unit=50,file=fnspdens,form='formatted')
         do jn=1,nsmax_jb_in
            write(50,*)jn,&
              &  gsize_in*float(max(nlon_in,nlat_in))/1000./float(jn),&
              &  zfcovd_in(jk,jk,jn)
         end do
         close(unit=50)
      end do
!
! Temperature
!
      do jk=1,nflevg
         write(fnspdens,1002)jk
 1002    format('diag_spdensTT',i2.2)
         open(unit=50,file=fnspdens,form='formatted')
         do jn=1,nsmax_jb_in
            write(50,*)jn,&
              &  gsize_in*float(max(nlon_in,nlat_in))/1000./float(jn),&
              &  zfcovtps_in(jk,jk,jn)
         end do
         close(unit=50)
      end do
!
! Surface pressure
!
      write(fnspdens,1003)
 1003 format('diag_spdensSUPS')
      open(unit=50,file=fnspdens,form='formatted')
      do jn=1,nsmax_jb_in
         write(50,*)jn,&
           &  gsize_in*float(max(nlon_in,nlat_in))/1000./float(jn),&
           &  zfcovtps_in(nflevg+1,nflevg+1,jn)
      end do
      close(unit=50)
!
! Humidity
!
      do jk=1,nflevg
         write(fnspdens,1004)jk
 1004    format('diag_spdensQQ',i2.2)
         open(unit=50,file=fnspdens,form='formatted')
         do jn=1,nsmax_jb_in
            write(50,*)jn,&
              &  gsize_in*float(max(nlon_in,nlat_in))/1000./float(jn),&
              &  zfcovq_in(jk,jk,jn)
         end do
         close(unit=50)
      end do
!
! Write diagnostic vertical correlation files
!
!  Vorticity
!
      do jk=1,nflevg
         write(fnspdens,1010)jk
 1010    format('diag_vercorPP',i2.2)
         open(unit=50,file=fnspdens,form='formatted')
         do jj=1,nflevg
            zsumcov = 0.
            zsumvar1 = 0.
            zsumvar2 = 0.
            do jn=1,nsmax_jb_in
               zsumcov = zsumcov + zfcovp_in(jk,jj,jn)
               zsumvar1 = zsumvar1 + zfcovp_in(jk,jk,jn)
               zsumvar2 = zsumvar2 + zfcovp_in(jj,jj,jn)
            end do
            write(50,*)jj,&
              &  pfull_nl(jj),&
              &  zsumcov/sqrt(zsumvar1*zsumvar2)
         end do
         close(unit=50)
      end do
!
!  Divergence
!
      do jk=1,nflevg
         write(fnspdens,1011)jk
 1011    format('diag_vercorDD',i2.2)
         open(unit=50,file=fnspdens,form='formatted')
         do jj=1,nflevg
            zsumcov = 0.
            zsumvar1 = 0.
            zsumvar2 = 0.
            do jn=1,nsmax_jb_in
               zsumcov = zsumcov + zfcovd_in(jk,jj,jn)
               zsumvar1 = zsumvar1 + zfcovd_in(jk,jk,jn)
               zsumvar2 = zsumvar2 + zfcovd_in(jj,jj,jn)
            end do
            write(50,*)jj,&
              &  pfull_nl(jj),&
              &  zsumcov/sqrt(zsumvar1*zsumvar2)
         end do
         close(unit=50)
      end do
!
!  Temperature
!
      do jk=1,nflevg
         write(fnspdens,1012)jk
 1012    format('diag_vercorTT',i2.2)
         open(unit=50,file=fnspdens,form='formatted')
         do jj=1,nflevg
            zsumcov = 0.
            zsumvar1 = 0.
            zsumvar2 = 0.
            do jn=1,nsmax_jb_in
               zsumcov = zsumcov + zfcovtps_in(jk,jj,jn)
               zsumvar1 = zsumvar1 + zfcovtps_in(jk,jk,jn)
               zsumvar2 = zsumvar2 + zfcovtps_in(jj,jj,jn)
            end do
            write(50,*)jj,&
              &  pfull_nl(jj),&
              &  zsumcov/sqrt(zsumvar1*zsumvar2)
         end do
         close(unit=50)
      end do
!
!  Humidity
!
      do jk=1,nflevg
         write(fnspdens,1013)jk
 1013    format('diag_vercorQQ',i2.2)
         open(unit=50,file=fnspdens,form='formatted')
         do jj=1,nflevg
            zsumcov = 0.
            zsumvar1 = 0.
            zsumvar2 = 0.
            do jn=1,nsmax_jb_in
               zsumcov = zsumcov + zfcovq_in(jk,jj,jn)
               zsumvar1 = zsumvar1 + zfcovq_in(jk,jk,jn)
               zsumvar2 = zsumvar2 + zfcovq_in(jj,jj,jn)
            end do
            write(50,*)jj,&
              &  pfull_nl(jj),&
              &  zsumcov/sqrt(zsumvar1*zsumvar2)
         end do
         close(unit=50)
      end do
!
!   Open input balancing file
!
      open(nuljb_in,file='stabal96.bal',form='unformatted')
!
!   Read GSA file header
!
      read(nuljb_in) clid
      write(nulout,*) 'GSA ID=',clid
      read(nuljb_in) clcom
      write(nulout,*) 'Description : ',clcom
      read(nuljb_in) iorig,idate,itime,inbset
      write(nulout,*) ' Center=',iorig,' Date=',idate,' Time=',itime
!
!   Read  GSA set 0 header : model geometry definition (LAM case !)
!
      read(nuljb_in) inbmat,iweight,itypmat,isetdist,ilendef
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &           ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      read(nuljb_in) idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &           ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      read(nuljb_in) zlon1,zlat1,zlon2,zlat2,zlon0,zlat0,&
           &       idgl,idlon,idgux,idlux,iismax,imsmax,iflevg,&
           &       ichkwd
      write(nulout,*) ' zlon1=',zlon1,' zlat1=',zlat1,' zlon2=',zlon2
      write(nulout,*) ' zlat2=',zlat2,' zlon0=',zlon0,' zlat0=',zlat0
      write(nulout,*) ' idgl=',idgl,' idlon=',idlon
      write(nulout,*) ' idgux=',idgux,' idlux=',idlux
      write(nulout,*) ' iismax=', iismax,' imsmax=',imsmax
      write(nulout,*) ' iflevg=',iflevg,' ichkwd=',ichkwd
!
!   Set truncation of input balancing operators
!   and allocate space for the input balancing operators
!
      itrunc=iismax
      nflevg=iflevg
      allocate(isorel2_in(0:nsmax_jb_in))
      allocate(isodor2_in(0:nmsmax_jb_in))
      isorel2_in(:)=0
      isodor2_in(:)=0
      call ellips(nsmax_jb_in,nmsmax_jb_in,isodor2_in,isorel2_in)
      ispec2_in=0
      do iii=0,nmsmax_jb_in
         ispec2_in=ispec2_in+4*(isodor2_in(iii)+1)
      end do
      write(nulout,*)' calculated ispec2_in=',ispec2_in
      deallocate(isorel2_in)
      deallocate(isodor2_in)
!      
      allocate(sdiv_in(nflevg,nflevg,0:itrunc))
      allocate(stps_in(nflevg+1,2*nflevg,0:itrunc))
      allocate(shum_in(nflevg,3*nflevg+1,0:itrunc))
!
!   BFACT are allocated and read here
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &           ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &           ' itypdi2=',itypdi2
!
      read(nuljb_in)
      read(nuljb_in)
      allocate(bfact_in(ispec2_in))
      read(nuljb_in)
      read(nuljb_in) (bfact_in(jj),jj=1,ispec2_in),ichkwd
!
!   Read  GSA set 2 : SDIV matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
           &          ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
           &          ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)
         read(nuljb_in) ((sdiv_in(jj,jk,jn),jk=1,nflevg),jj=1,nflevg),&
              &            ichkwd
      end do
!
!   Read  GSA set 3: STPS/Pb matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) 
         read(nuljb_in)((stps_in(jj,jk,jn),jk=1,nflevg),jj=1,nflevg+1),&
             &              ichkwd
      end do
!
!   Read  GSA set 4: STPS/divu matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) 
         read(nuljb_in)&
       &    ((stps_in(jj,nflevg+jk,jn),jk=1,nflevg),jj=1,nflevg+1),&
       &         ichkwd
      end do
!
!   Read  GSA set 5:   SHUM/Pb matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in)
         read(nuljb_in) ((shum_in(jj,jk,jn),jk=1,nflevg),jj=1,nflevg),&
             &               ichkwd
      end do
!
!   Read  GSA set 6:   SHUM/Divu matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) 
         read(nuljb_in) &
           &  ((shum_in(jj,nflevg+jk,jn),jk=1,nflevg),jj=1,nflevg),&
           &            ichkwd
      end do
!
!   Read  GSA set 7:   SHUM/Tpsu matrices
!
      read(nuljb_in)inbmat,iweight,itypmat,isetdist,ilendef
      read(nuljb_in)idim1,idim2,ipar1,ipar2,itypdi1,itypdi2
      write(nulout,*) ' inbmat=',inbmat,' iweight=',iweight,&
          &            ' itypmat=',itypmat
      write(nulout,*) ' isetdist=',isetdist,' ilendef=',ilendef
      write(nulout,*) ' idim1=',idim1,' idim2=',idim2,' ipar1=',ipar1
      write(nulout,*) ' ipar2=',ipar2,' itypdi1=',itypdi1,&
          &            ' itypdi2=',itypdi2
      read(nuljb_in)
      read(nuljb_in)
      do jn=0,itrunc
         read(nuljb_in) 
         read(nuljb_in) &
           &  ((shum_in(jj,2*nflevg+jk,jn),jk=1,nflevg+1),jj=1,nflevg),&
           &                  ichkwd
      end do
!
!   Close input balancing file
!
      close(nuljb_in)
!
      write(nulout,*)' bfactor_in'
      call bfact_print(nulout,nsmax_jb_in,nmsmax_jb_in,ispec2_in,bfact_in)
!
!   Calculate iso-tropic vorticity to balanced linearized geopotential 
!   operator
!
      allocate(bfact_iso(0:nsmax_jb_in))
!
      call calc_bfact_iso(nflevg,nsmax_jb_in,nmsmax_jb_in,ispec2_in,&
            &       1.5,bfact_in,bfact_iso)
!
!   Diagnostic balance operator files
!
!   Allocate space for covariance of lineraized geopotential
!
      allocate(cov_ppp(nflevg,nflevg,0:nsmax_jb_in))
!
!   Allocate space for different components of the variances
!   and initalize with zeroes
!
      allocate(var_vor(nflevg,0:nsmax_jb_in))
      allocate(var_ppp(nflevg,0:nsmax_jb_in))
      allocate(var_div_bal_ppp(nflevg,0:nsmax_jb_in))
      allocate(var_div_u(nflevg,0:nsmax_jb_in))
      allocate(var_div_total(nflevg,0:nsmax_jb_in))
      allocate(var_tps_bal_ppp(nflevg+1,0:nsmax_jb_in))
      allocate(var_tps_bal_div(nflevg+1,0:nsmax_jb_in))
      allocate(var_tps_u(nflevg+1,0:nsmax_jb_in))
      allocate(var_tps_total(nflevg+1,0:nsmax_jb_in))
      allocate(var_hum_bal_ppp(nflevg,0:nsmax_jb_in))
      allocate(var_hum_bal_div(nflevg,0:nsmax_jb_in))
      allocate(var_hum_bal_tps(nflevg,0:nsmax_jb_in))
      allocate(var_hum_u(nflevg,0:nsmax_jb_in))
      allocate(var_hum_total(nflevg,0:nsmax_jb_in))
!
      allocate(ave_var_vor(nflevg))
      allocate(ave_var_ppp(nflevg))
      allocate(ave_var_div_bal_ppp(nflevg))
      allocate(ave_var_div_u(nflevg))
      allocate(ave_var_div_total(nflevg))
      allocate(ave_var_tps_bal_ppp(nflevg+1))
      allocate(ave_var_tps_bal_div(nflevg+1))
      allocate(ave_var_tps_u(nflevg+1))
      allocate(ave_var_tps_total(nflevg+1))
      allocate(ave_var_hum_bal_ppp(nflevg))
      allocate(ave_var_hum_bal_div(nflevg))
      allocate(ave_var_hum_bal_tps(nflevg))
      allocate(ave_var_hum_u(nflevg))
      allocate(ave_var_hum_total(nflevg))
!
      var_vor(:,:)  = 0.
      var_ppp(:,:)  = 0.
      var_div_bal_ppp(:,:)  = 0.
      var_div_u(:,:)  = 0.
      var_div_total(:,:)  = 0.
      var_tps_bal_ppp(:,:)  = 0.
      var_tps_bal_div(:,:)  = 0.
      var_tps_u(:,:)  = 0.
      var_tps_total(:,:)  = 0.
      var_hum_bal_ppp(:,:)  = 0.
      var_hum_bal_div(:,:)  = 0.
      var_hum_bal_tps(:,:)  = 0.
      var_hum_u(:,:)  = 0.
      var_hum_total(:,:)  = 0.
!
!   Calculate covariance of linearized geopotential
!
      do jn=0,nsmax_jb_in
         do jk=1,nflevg
            do jl=1,nflevg
               cov_ppp(jl,jk,jn) = bfact_iso(jn)*&
          &                        zfcovp_in(jl,jk,jn)*&
          &                        bfact_iso(jn)
            end do
         end do
      end do
!
!   Calculate the different contributions to the variances
!
      do jn=1,nsmax_jb_in
         do jj=1,nflevg
            var_vor(jj,jn) = zfcovp_in(jj,jj,jn)
            var_ppp(jj,jn) = cov_ppp(jj,jj,jn)
            var_div_u(jj,jn) = zfcovd_in(jj,jj,jn)
            var_hum_u(jj,jn) = zfcovq_in(jj,jj,jn)
         end do
         do jj=1,nflevg+1
            var_tps_u(jj,jn) = zfcovtps_in(jj,jj,jn)
         end do
         do jj=1,nflevg
            do jk=1,nflevg
               do jl=1,nflevg
                  var_div_bal_ppp(jj,jn) = var_div_bal_ppp(jj,jn) +&
                     &                     sdiv_in(jj,jk,jn)*&
                     &                     sdiv_in(jj,jl,jn)*&
                     &                     cov_ppp(jk,jl,jn)
               end do
            end do
         end do
         do jj=1,nflevg
            var_div_total(jj,jn) = var_div_bal_ppp(jj,jn) +&
              &                    var_div_u(jj,jn)
         end do
         do jj=1,nflevg+1
            do jk=1,nflevg
               do jl=1,nflevg
                  var_tps_bal_ppp(jj,jn) = var_tps_bal_ppp(jj,jn) +&
                     &                     stps_in(jj,jk,jn)*&
                     &                     stps_in(jj,jl,jn)*&
                     &                     cov_ppp(jk,jl,jn)
                  var_tps_bal_div(jj,jn) = var_tps_bal_div(jj,jn) +&
                     &                     stps_in(jj,nflevg+jk,jn)*&
                     &                     stps_in(jj,nflevg+jl,jn)*&
                     &                     zfcovd_in(jk,jl,jn)
               end do
            end do
         end do
         do jj=1,nflevg+1
            var_tps_total(jj,jn) = var_tps_bal_ppp(jj,jn) +&
              &                    var_tps_bal_div(jj,jn) +&
              &                    var_tps_u(jj,jn)
         end do
         do jj=1,nflevg
            do jk=1,nflevg
               do jl=1,nflevg
                  var_hum_bal_ppp(jj,jn) = var_hum_bal_ppp(jj,jn) +&
                     &                     shum_in(jj,jk,jn)*&
                     &                     shum_in(jj,jl,jn)*&
                     &                     cov_ppp(jk,jl,jn)
                  var_hum_bal_div(jj,jn) = var_hum_bal_div(jj,jn) +&
                     &                     shum_in(jj,nflevg+jk,jn)*&
                     &                     shum_in(jj,nflevg+jl,jn)*&
                     &                     zfcovd_in(jk,jl,jn)
               end do
            end do
            do jk=1,nflevg+1
               do jl=1,nflevg+1
                  var_hum_bal_tps(jj,jn) = var_hum_bal_tps(jj,jn) +&
                     &                     shum_in(jj,2*nflevg+jk,jn)*&
                     &                     shum_in(jj,2*nflevg+jl,jn)*&
                     &                     zfcovtps_in(jk,jl,jn)
               end do
            end do
         end do
         do jj=1,nflevg
            var_hum_total(jj,jn) = var_hum_bal_ppp(jj,jn) +&
              &                    var_hum_bal_div(jj,jn) +&
              &                    var_hum_bal_tps(jj,jn) +&
              &                    var_hum_u(jj,jn)
         end do
      end do
!
!   Sum variances over wave-numbers
!
      do jj=1,nflevg
         ave_var_vor(jj)  = sum(var_vor(jj,1:nsmax_jb_in))
         ave_var_ppp(jj)  = sum(var_ppp(jj,1:nsmax_jb_in))
         ave_var_div_bal_ppp(jj)  = sum(var_div_bal_ppp(jj,1:nsmax_jb_in))
         ave_var_div_u(jj)  = sum(var_div_u(jj,1:nsmax_jb_in))
         ave_var_div_total(jj)  = sum(var_div_total(jj,1:nsmax_jb_in))
         ave_var_hum_bal_ppp(jj)  = sum(var_hum_bal_ppp(jj,1:nsmax_jb_in))
         ave_var_hum_bal_div(jj)  = sum(var_hum_bal_div(jj,1:nsmax_jb_in))
         ave_var_hum_bal_tps(jj)  = sum(var_hum_bal_tps(jj,1:nsmax_jb_in))
         ave_var_hum_u(jj)  = sum(var_hum_u(jj,1:nsmax_jb_in))
         ave_var_hum_total(jj)  = sum(var_hum_total(jj,1:nsmax_jb_in))
      end do
      do jj=1,nflevg+1
         ave_var_tps_bal_ppp(jj)  = sum(var_tps_bal_ppp(jj,1:nsmax_jb_in))
         ave_var_tps_bal_div(jj)  = sum(var_tps_bal_div(jj,1:nsmax_jb_in))
         ave_var_tps_u(jj)  = sum(var_tps_u(jj,1:nsmax_jb_in))
         ave_var_tps_total(jj)  = sum(var_tps_total(jj,1:nsmax_jb_in))
      end do
!
!   Allocate and calculate fractions of explained variance as
!   functions of vertical level
!
      allocate(expl_div_ppp(nflevg))
      allocate(expl_tps_ppp(nflevg+1))
      allocate(expl_tps_div(nflevg+1))
      allocate(expl_hum_ppp(nflevg))
      allocate(expl_hum_div(nflevg))
      allocate(expl_hum_tps(nflevg))
!
      do jj=1,nflevg
         expl_div_ppp(jj) = ave_var_div_bal_ppp(jj)/ave_var_div_total(jj)
         expl_hum_ppp(jj) = ave_var_hum_bal_ppp(jj)/ave_var_hum_total(jj)
         expl_hum_div(jj) = ave_var_hum_bal_div(jj)/ave_var_hum_total(jj)
         expl_hum_tps(jj) = ave_var_hum_bal_tps(jj)/ave_var_hum_total(jj)
      end do
      do jj=1,nflevg+1
         expl_tps_ppp(jj) = ave_var_tps_bal_ppp(jj)/ave_var_tps_total(jj)
         expl_tps_div(jj) = ave_var_tps_bal_div(jj)/ave_var_tps_total(jj)
      end do
!
!   Allocate and calculate fractions of explained variance as
!   functions of vertical level
!
      allocate(expl_div_ppp_hor(0:nsmax_jb_in))
      allocate(expl_tps_ppp_hor(0:nsmax_jb_in))
      allocate(expl_tps_div_hor(0:nsmax_jb_in))
      allocate(expl_hum_ppp_hor(0:nsmax_jb_in))
      allocate(expl_hum_div_hor(0:nsmax_jb_in))
      allocate(expl_hum_tps_hor(0:nsmax_jb_in))
!
      do jn=0,nsmax_jb_in
         expl_div_ppp_hor(jn) = sum(var_div_bal_ppp(1:nflevg,jn))/&
                          &     sum(var_div_total(1:nflevg,jn))
         expl_tps_ppp_hor(jn) = sum(var_tps_bal_ppp(1:nflevg,jn))/&
                          &     sum(var_tps_total(1:nflevg,jn))
         expl_tps_div_hor(jn) = sum(var_tps_bal_div(1:nflevg,jn))/&
                          &     sum(var_tps_total(1:nflevg,jn))
         expl_hum_ppp_hor(jn) = sum(var_hum_bal_ppp(1:nflevg,jn))/&
                          &     sum(var_hum_total(1:nflevg,jn))
         expl_hum_div_hor(jn) = sum(var_hum_bal_div(1:nflevg,jn))/&
                          &     sum(var_hum_total(1:nflevg,jn))
         expl_hum_tps_hor(jn) = sum(var_hum_bal_tps(1:nflevg,jn))/&
                          &     sum(var_hum_total(1:nflevg,jn))
      end do
!
!   Diagnostic output files
!
!   Control variable standard deviations
!
      open(unit=50,file='diag_stand_devs',form='formatted')
      do jj=1,nflevg
         write(50,4444) jj,&
                  &     pfull_nl(jj),&
                  &     sqrt(ave_var_vor(jj)),&
                  &     sqrt(ave_var_div_u(jj)),&
                  &     sqrt(ave_var_tps_u(jj)),&
                  &     sqrt(ave_var_hum_u(jj))
 4444    format(i4,f10.2,4e15.6)
      end do
      close(unit=50)
!
!   Divergence wave number dependence
!
      open(unit=50,file='diag_bal_wn_div',form='formatted')
      do jn=1,nsmax_jb_in
         write(50,5880)jn,&
              &  gsize_in*float(max(nlon_in,nlat_in))/1000./float(jn),&
              &   expl_div_ppp_hor(jn)
      end do
 5880 format(i3,f8.1,f8.4)
      close(unit=50)
!
!   Divergence level dependence
!
      open(unit=50,file='diag_baloperdiv',form='formatted')
      do jj=1,nflevg
         write(50,5900)jj,pfull_nl(jj),&
                     &   sqrt(ave_var_div_total(jj)),&
                     &   sqrt(ave_var_div_bal_ppp(jj)),&
                     &   sqrt(ave_var_div_u(jj)),&
                     &   expl_div_ppp(jj)
      end do
 5900 format(i3,f10.2,3f14.10,f8.4)
      close(unit=50)
!
!   Temperature and surface pressure wave number dependence
!
      open(unit=50,file='diag_bal_wn_tps',form='formatted')
      do jn=1,nsmax_jb_in
         write(50,5940)jn,&
              &  gsize_in*float(max(nlon_in,nlat_in))/1000./float(jn),&
              &   expl_tps_ppp_hor(jn),&
              &   expl_tps_div_hor(jn)
      end do
 5940 format(i3,f8.1,2f8.4)
      close(unit=50)
!
!   Temperature and surface pressure level dependence
!
      open(unit=50,file='diag_balopertps',form='formatted')
      do jj=1,nflevg
         write(50,5950)jj,pfull_nl(jj),&
                     &   sqrt(ave_var_tps_total(jj)),&
                     &   sqrt(ave_var_tps_bal_ppp(jj)),&
                     &   sqrt(ave_var_tps_bal_div(jj)),&
                     &   sqrt(ave_var_tps_u(jj)),&
                     &   expl_tps_ppp(jj),&
                     &   expl_tps_div(jj)
      end do
 5950 format(i3,f10.2,4f14.10,2f8.4)
      close(unit=50)
!
!   Humidity wave number dependence
!
      open(unit=50,file='diag_bal_wn_hum',form='formatted')
      do jn=1,nsmax_jb_in
         write(50,5960)jn,&
              &  gsize_in*float(max(nlon_in,nlat_in))/1000./float(jn),&
              &   expl_hum_ppp_hor(jn),&
              &   expl_hum_div_hor(jn),&
              &   expl_hum_tps_hor(jn)
      end do
 5960 format(i3,f8.1,3f8.4)
      close(unit=50)
!
!   Humidity level dependence
!
      open(unit=50,file='diag_baloperhum',form='formatted')
      do jj=1,nflevg
         write(50,6000)jj,pfull_nl(jj),&
                     &   sqrt(ave_var_hum_total(jj)),&
                     &   sqrt(ave_var_hum_bal_ppp(jj)),&
                     &   sqrt(ave_var_hum_bal_div(jj)),&
                     &   sqrt(ave_var_hum_bal_tps(jj)),&
                     &   sqrt(ave_var_hum_u(jj)),&
                     &   expl_hum_ppp(jj),&
                     &   expl_hum_div(jj),&
                     &   expl_hum_tps(jj)
      end do
 6000 format(i3,f10.2,5f14.10,3f8.4)
      close(unit=50)
!
!   De-allocate space for input covariances
!
      deallocate(zfcovp_in)
      deallocate(zfcovd_in)
      deallocate(zfcovtps_in)
      deallocate(zfcovq_in)
      deallocate(zpdat_in)
!
!   De-allocate space for input balance operators
!
      deallocate(sdiv_in)
      deallocate(stps_in)
      deallocate(shum_in)
      deallocate(bfact_in)
!
      stop
      end
      subroutine calc_bfact_iso(nflev,nsmax,nmsmax,nspec2,&
            &       ppdjkstar,fact1,fact1_iso)
!
      implicit none
!
! Input and output
!
      integer(kind=4)      :: nflev,nsmax,nmsmax,nspec2
      real(kind=8)         :: ppdjkstar
      real(kind=8)         :: fact1(nspec2)
      real(kind=8)         :: fact1_iso(0:nsmax)
!
! Work arrays
!
      integer(kind=4)      :: jkstar,jm,jn,in,inm
      real(kind=8)         :: zink(0:nsmax)
      integer(kind=4)      :: kntmp(0:nmsmax),kmtmp(0:nsmax)
      integer(kind=4)      :: ncpl4m(0:nmsmax),nesm0(0:nmsmax)
      real(kind=8)         :: zkstar,zmf
!
      call ellips(nsmax,nmsmax,kntmp,kmtmp)
!
      do jm=0,nmsmax
         ncpl4m(jm)=4*(kntmp(jm)+1)
      end do
!
      nesm0(0)=1
      do jm=1,nmsmax
         nesm0(jm)=nesm0(jm-1)+ncpl4m(jm-1)
      end do
!
      fact1_iso(:) = 0.
      zink(:) = 0.
!
      fact1_iso(0)=fact1(1)
!     
      do jkstar=1,nsmax
        do jm=0,nmsmax
          do jn=0,ncpl4m(jm)-1
!
            in=int(jn/4)
            inm=nesm0(jm)+jn
!
            if( (jm.ne.0).or.(in.ne.0) )then
!
              zkstar=float(nsmax)*sqrt( (float(in)/float(nsmax ))**2+ &
                 &                      (float(jm)/float(nmsmax))**2  )
!
              if( (zkstar.ge.(float(jkstar)-ppdjkstar)).and.&
                & (zkstar.le.(float(jkstar)+ppdjkstar))      ) then
!
                zink(jkstar)=zink(jkstar)+&
                &     float(2**(min(1,in))) * float(2**(min(1,jm)))&
                &     *(0.1)**(abs(float(jkstar)-zkstar))
                zmf=float(2**(min(1,in))) * float(2**(min(1,jm))) &
                &      *(0.1)**(abs(float(jkstar)-zkstar))
                fact1_iso(jkstar)=fact1_iso(jkstar)+zmf*fact1(inm)
!
              end if
            end if
          end do
        end do
!
        fact1_iso(jkstar)=fact1_iso(jkstar)/zink(jkstar)
!
      enddo
!
      return
      end
      subroutine bfact_print(nulout,nsmax,nmsmax,ispec2,bfact)
!
      implicit none
!
      integer(kind=4)      ::  nulout
      integer(kind=4)      ::  nsmax,nmsmax,ispec2
      real(kind=8)         ::  bfact(ispec2)
!
      integer(kind=4)      ::  kntmp(0:nmsmax),kmtmp(0:nsmax)
      integer(kind=4)      ::  ncpl4m(0:nmsmax),nesm0(0:nmsmax)
!
      integer(kind=4)      ::  jm,jn,in,inm
!
      real(kind=4)         ::   b_out(0:5,0:5)

      call ellips(nsmax,nmsmax,kntmp,kmtmp)
!
      do jm=0,nmsmax
        ncpl4m(jm)=4*(kntmp(jm)+1)
      enddo
!
      nesm0(0)=1
      do jm=1,nmsmax
        nesm0(jm)=nesm0(jm-1)+ncpl4m(jm-1)
      enddo
!
      b_out(:,:) = 0.
!
      do jm=0,nmsmax
         do jn=0,ncpl4m(jm)-1
            in=int(jn/4)
            inm=nesm0(jm)+jn
!
            if( jm.le.5 .and. in.le.5 ) then
               b_out(in,jm) = bfact(inm)
            end if
!
         enddo
      enddo
!
      do jm=0,5
         write(nulout,100)jm,(b_out(jm,jn),jn=0,5)
      end do
!
  100 format('jm=',i4,'bfact=',6f10.0)
      return
!
      end
      SUBROUTINE ELLIPS(KSMAX,KMSMAX,KNTMP,KMTMP)
      INTEGER(KIND=4) :: KSMAX, KMSMAX
      INTEGER(KIND=4) :: KNTMP(0:KMSMAX),KMTMP(0:KSMAX)
      INTEGER(KIND=4) :: JM, JN
      REAL(KIND=8) :: ZEPS, ZKN, ZKM, ZAUXIL
      integer, parameter :: jpdblr = 8

      ZEPS=1.E-10
      ZAUXIL=0.
      DO 110 JM=1,KMSMAX-1
      ZKN = REAL(KSMAX,JPDBLR)/REAL(KMSMAX,JPDBLR)*&
     & SQRT(MAX(ZAUXIL,REAL(KMSMAX**2-JM**2,JPDBLR)))
        KNTMP(JM)=INT(ZKN+ZEPS)
 110  CONTINUE

      IF( KMSMAX.EQ.0 )THEN
         KNTMP(0)=KSMAX
      ELSE
         KNTMP(0)=KSMAX
         KNTMP(KMSMAX)=0
      ENDIF

      DO 210 JN=1,KSMAX-1
       ZKM = REAL(KMSMAX,JPDBLR)/REAL(KSMAX,JPDBLR)*&
     & SQRT(MAX(ZAUXIL,REAL(KSMAX**2-JN**2,JPDBLR)))
        KMTMP(JN)=INT(ZKM+ZEPS)
 210  CONTINUE

      IF( KSMAX.EQ.0 )THEN
         KMTMP(0)=KMSMAX
      ELSE
         KMTMP(0)=KMSMAX
         KMTMP(KSMAX)=0
      ENDIF
      RETURN
      END      
