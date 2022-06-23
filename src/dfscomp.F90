  PROGRAM DFSCOMP

! =====================================================
! Compute Degrees of Freedom for Signal (DFS)
! =====================================================
!
!
! Method
! =====================================================
!
! Implemented formulation is:
!
! DFS = (obsvalue* - obsvalue ) * (final_obs_error^-2) * 
! [ (fg_depar - an_depar)* - (fg_depar - an_depar) ] 
!
! i.e. DFS = ( y* - y ) * R^-1 * (Hdx* - Hdx )
!
! where * indicates perturbed observations and
! analysis from perturbed observations.
!
!
! Practicals
! =====================================================
!
! The program reads two file, 1 and 2, from
! perturbed and unperturbed analysis respectively
! for the same observations dataset.
! Files are obtained by running COMPSTAT in the
! CCMA directory.
!
!
! Scientific Documentation
! =====================================================
!
! Chapnik et al., 2006, QJRMS 132 pp543--565
!
!
! Modifications
! =====================================================
!
! Andrea Storto *met.no*, 05.02.2008 : Original
!


IMPLICIT NONE


 CHARACTER(LEN=300) :: CFILE1,CFILE2
 LOGICAL            :: LEX
 INTEGER            :: INUM,IERR
 INTEGER            :: NUN1,NUN2,NUN3
 INTEGER, PARAMETER :: NUMV=27
 REAL               :: ZDFS(NUMV)
 INTEGER            :: NDFS(NUMV)
 INTEGER            :: ITOT,INOTC,ICHAN
 
 INTEGER            :: IOT1,IOS1,IOSE1,IOV1
 REAL               :: ZLAT1,ZLON1,ZOBS1,ZERR1,ZFGDEP1,ZANDEP1
 INTEGER            :: IOT2,IOS2,IOSE2,IOV2
 REAL               :: ZLAT2,ZLON2,ZOBS2,ZERR2,ZFGDEP2,ZANDEP2
 CHARACTER(LEN=10)  :: CHST1,CHST2
 REAL               :: ZCH1,ZCH2
 INTEGER            :: IIND
 INTEGER            :: II
 LOGICAL            :: LLVERBCS, Skip_Line
 

 NUN1=21
 NUN2=22
 NUN3=23
 NDFS(:)=0
 ZDFS(:)=0
 ITOT = 0 
 INOTC = 0
 LLVERBCS=.TRUE.

 CFILE1='file1'
 CFILE1='file2'
 CALL GETARG(1,CFILE1)
 CALL GETARG(2,CFILE2)

 if (LEN_TRIM(CFILE1) .EQ. 0 .OR. &
   & LEN_TRIM(CFILE2) .EQ. 0 ) CALL USAGE


 INQUIRE(FILE=CFILE1,EXIST=LEX)
 IF(.NOT.LEX) CALL USAGE
 INQUIRE(FILE=CFILE2,EXIST=LEX)
 IF(.NOT.LEX) CALL USAGE

 OPEN(NUN1,FILE=CFILE1)
 OPEN(NUN2,FILE=CFILE2)

 IERR=0

 READ(NUN1,*,IOSTAT=IERR)
 IF (IERR .NE. 0 ) THEN
   WRITE(*,*) 'IERR NUN1',IERR
 ENDIF
 READ(NUN2,*,IOSTAT=IERR)
 IF (IERR .NE. 0 ) THEN
   WRITE(*,*) 'IERR NUN2',IERR
 ENDIF
 Skip_Line=.FALSE.

 obs_cycle : DO WHILE (IERR .EQ. 0)

!
! The ODB request is:
!
! SELECT
! obstype,codetype,press,sensor,statid,varno,degrees(lat),degrees(lon),
! obsvalue,final_obs_error,fg_depar,an_depar
! FROM  hdr,desc,body,errstat WHERE (an_depar is not NULL)
!

  IF (.NOT.Skip_Line) THEN
    READ(NUN1,*,IOSTAT=IERR) IOT1,IOS1,ZCH1,IOSE1,CHST1,IOV1,&
                           & ZLAT1,ZLON1,ZOBS1,ZERR1,ZFGDEP1,ZANDEP1 
  ENDIF
  READ(NUN2,*,IOSTAT=IERR) IOT2,IOS2,ZCH2,IOSE2,CHST2,IOV2,&
                         & ZLAT2,ZLON2,ZOBS2,ZERR2,ZFGDEP2,ZANDEP2 

  IF (IERR .NE. 0 ) THEN
    IF (IERR .EQ. -1 ) THEN
      WRITE(6,*)'dfscomp: File reading complete'
    ELSE
      WRITE(6,*) 'iostat != 0 : ', IERR
      WRITE(6,*) IOT1,IOS1,ZCH1,IOSE1,CHST1,IOV1,&
                  & ZLAT1,ZLON1,ZOBS1,ZERR1,ZFGDEP1,ZANDEP1
      WRITE(6,*) IOT2,IOS2,ZCH2,IOSE2,CHST2,IOV2,&
                  & ZLAT2,ZLON2,ZOBS2,ZERR2,ZFGDEP2,ZANDEP2
    ENDIF
  ENDIF

  IF (ZLON1 .NE. ZLON2 .OR. ZLAT1 .NE. ZLAT2 .OR. &
    & IOT1 .NE. IOT2 .OR. IOS1 .NE. IOS2 .OR. IOV1 .NE. IOV2 &
    & .OR. ZERR1 .NE. ZERR2) THEN
 
    WRITE(*,*) 'ZLON ',ZLON1,ZLON2
    WRITE(*,*) 'ZLAT ',ZLAT1,ZLAT2
    WRITE(*,*) 'IOT   ',IOT1,IOT2
    WRITE(*,*) 'IOS   ',IOS1,IOS2
    WRITE(*,*) 'IOV   ',IOV1,IOV2
    WRITE(*,*) 'ZERR  ',ZERR1,ZERR2
    WRITE(*,*) 'INCONSISTENCY BETWEEN INPUT FILES, SKIP THIS OBS'
    Skip_Line=.TRUE.
    CYCLE
  ELSE 
    Skip_Line=.FALSE.
  ENDIF

  IIND=0

  ICHAN=NINT(ZCH1)

! INDEX : OB_VAR (varno)
!     1 : SYNOP_Z
!     2 : SYNOP_T2
!     3 : SYNOP_R2
!     4 : SYNOP_U10
!     5 : SYNOP_ZTD
!     6 : TEMP_U
!     7 : TEMP_T
!     8 : TEMP_Z
!     9 : TEMP_Q
!    10 : AIREP_T
!    11 : AIREP_U
!    12 : SATOB_U
!    13 : DRIBU_Z
!    14 : DRIBU_U
!    15 : PILOT_Z
!    16 : PILOT_U
!    17 : AMSUA
!    18 : MHS
!    19 : MWHS2
!    20 : ATMS
!    21 : IASI
!    22 : CRIS
!    23 : SEVIRI
!    24 : SCATT_U
!    25 : RADAR_Z
!    26 : RADAR_U
!    27 : TEMP_CLS


  IF (IOT1 .EQ. 1 ) THEN
      ! SYNOPP
      IF (IOV1.EQ.1) IIND=1
      ! SYNOPT
      IF (IOV1.EQ.39) IIND=2
      ! SYNOPQ
      IF (IOV1.EQ.58 .OR. IOV1.EQ.40) IIND=3
      ! SYNOPU
      IF (IOV1.EQ.41 .OR. IOV1.EQ.42) IIND=4
      ! ZTD
      IF (IOV1.EQ.128) IIND=5
  ENDIF

  IF (IOT1 .EQ. 5 ) THEN
      ! TEMPU
      IF (IOV1.EQ.3 .OR. IOV1.EQ.4 .OR. IOV1.EQ.41 .OR. IOV1.EQ.42) IIND=6
      ! TEMPT
      IF (IOV1.EQ.2 .OR. IOV1.EQ.39) IIND=7
      ! TEMPZ
      IF (IOV1.EQ.1) IIND=8
      ! TEMPQ
      IF (IOV1.EQ.58 .OR. IOV1.EQ.59 .OR. IOV1.EQ.40 .OR. &
        & IOV1.EQ.7) IIND=9
      ! CLOUDSAT
      IF (IOV1.EQ.29 .AND. CHST1(1:3) .EQ.'CLS' ) IIND=27
  ENDIF

  ! AIREP
  IF (IOT1 .EQ. 2) THEN
      ! AIREP-T
      IF (IOV1.EQ.2 .OR. IOV1.EQ.39) IIND=10
      ! AIREPU
      IF (IOV1.EQ.3 .OR. IOV1.EQ.4 .OR. IOV1.EQ.41 .OR. IOV1.EQ.42) IIND=11
  ENDIF

  ! SATOB
  IF (IOT1 .EQ. 3) IIND=12

  ! DRIBU
  IF (IOT1 .EQ. 4) THEN
      ! DRIBUZ
      IF (IOV1.EQ.1) IIND=13
      ! DRIBUU
      IF (IOV1.EQ.3 .OR. IOV1.EQ.4 .OR. IOV1.EQ.41 .OR. IOV1.EQ.42) IIND=14
  ENDIF

  ! PILOT
  IF (IOT1 .EQ. 6) THEN
      ! PILOTZ
      IF (IOV1.EQ.1) IIND=15
      ! PILOTU
      IF (IOV1.EQ.3 .OR. IOV1.EQ.4 .OR. IOV1.EQ.41 .OR. IOV1.EQ.42) IIND=16
  ENDIF

  IF (IOT1 .EQ. 7) THEN
      ! AMSU-A
      IF (IOSE1 .EQ. 3) IIND=17
      ! AMSU-B and MHS
      IF (IOSE1 .EQ. 4 .OR. IOSE1 .EQ. 15) IIND=18
      ! ATMS
      IF (IOSE1 .EQ. 19) IIND=19
      ! MWHS2
      IF (IOSE1 .EQ. 73) IIND=20
      ! IASI
      IF (IOSE1 .EQ. 16) IIND=21
      ! CRIS
      IF (IOSE1 .EQ. 27) IIND=22
      ! SEVIRI
      IF (IOSE1 .EQ. 29) IIND=23
  ENDIF

  ! SCATT
  IF (IOT1 .EQ. 9) IIND=24

  ! RADAR
  IF (IOT1 .EQ. 13) THEN
      ! RFL
      IF (IOV1.EQ.29) IIND=25
      ! DOW
      IF (IOV1.EQ.195) IIND=26
  ENDIF

  IF (IIND .EQ. 0 ) THEN
     INOTC = INOTC + 1
     ! This should not happen
     WRITE(6,*)  IOT1,IOV1,IOS1,IOSE1,ICHAN,CHST1
     CYCLE obs_cycle
  ENDIF

! Compute DFS 

  NDFS(IIND)=NDFS(IIND)+1
  ZDFS(IIND)=ZDFS(IIND)+ABS( (ZOBS1-ZOBS2)*(1./(ZERR1*ZERR1))* &
           & ((ZFGDEP1-ZANDEP1)-(ZFGDEP2-ZANDEP2)) )
  !IF (IOT1 .EQ. 7 .AND. IOSE1 .EQ. 16) THEN
  !   write(370,*) NDFS(IIND), ZOBS1-ZOBS2, ZFGDEP1-ZANDEP1,ZFGDEP2-ZANDEP2
  !   write(371,*) NDFS(IIND), ZOBS1-ZOBS2, ZFGDEP1-ZANDEP1,ZFGDEP2-ZANDEP2,ZDFS(IIND)
  !ENDIF
  !IF (IOT1 .EQ. 7 .AND. IOSE1 .EQ. 3) THEN
  !   write(374,*) NDFS(IIND), ZOBS1-ZOBS2, ZFGDEP1-ZANDEP1,ZFGDEP2-ZANDEP2
  !   write(375,*) NDFS(IIND), ZOBS1-ZOBS2, ZFGDEP1-ZANDEP1,ZFGDEP2-ZANDEP2,ZDFS(IIND)
  !ENDIF


 ENDDO obs_cycle

 CLOSE(NUN1)
 CLOSE(NUN2)

 ITOT=SUM(NDFS(:))

 WRITE(*,*) 'dfscomp: Observations used   :',ITOT 
 WRITE(*,*) 'dfscomp: Observations unused :',INOTC

 OPEN(NUN3,FILE='dfs.dat')

 DO II=1,NUMV
   WRITE(NUN3,'(I6,I6,F10.3)') II,NDFS(II),ZDFS(II)
 ENDDO

 CONTAINS

 SUBROUTINE USAGE

 WRITE(*,*) 
 WRITE(*,*) 'Usage: dfscomp.x file1 file2'
 WRITE(*,*) '       file1 : ODB query from perturbed CCMA '
 WRITE(*,*) '       file2 : ODB query from unperturbed CCMA '
 WRITE(*,*) 

 STOP

 END SUBROUTINE USAGE

 END PROGRAM DFSCOMP
