       SUBROUTINE READCOV(pcov,kdim,nsmax,kpar,cfile ,llverb,kret)
       IMPLICIT NONE 
!      read a set of nonsep matrices for the requested parameter
!      from a GSA file
!                                               F. Bouttier Oct 96
!      input : cfile = GSAfilename
!               kpar = GSA parameter selector (IPAR coding)
!      output : pcov = covariance matrices
!               kret = ok if 0, not found if 1

!      ROUTINE BASED ON 
!      THE MAIN PROGRAM stdav.F90  :  G. Boloni based on "fediacov" 2010/09/09
!                                  :  B. Strajnar                   2010/10/26 
!      USE AS PYTHON EXTENSION     :  I. Dehmous                    2023/10/03              
      

      INTEGER, PARAMETER             :: jplun=67
      INTEGER, PARAMETER             :: JPRB = SELECTED_REAL_KIND(13,300)
      INTEGER, PARAMETER             :: JPIM = SELECTED_INT_KIND(9)
      INTEGER(KIND=JPIM),INTENT(IN ) :: kdim,nsmax ,kpar
      INTEGER(KIND=JPIM),INTENT(OUT) :: kret
      INTEGER(KIND=JPIM)  :: idim1,idim2,ipar1,ipar2,icor1,icor2, icheck
      REAL(KIND=JPRB)   ,INTENT(OUT) :: pcov(kdim,kdim,0:nsmax)
      CHARACTER(LEN=*)               :: cfile
      CHARACTER(LEN=8)               :: clid
      CHARACTER(LEN=70)              :: clcom
      LOGICAL           ,INTENT(IN)  :: llverb
      LOGICAL                        :: llfound
      INTEGER(KIND=JPIM)  :: iorig,idate,itime,inbset
      INTEGER(KIND=JPIM)  :: jset , jj , jn , jk 
      INTEGER(KIND=JPIM)  :: inbmat,iweight,itypmat,isetdist,ilendef


! 1.OPEN FILE AND READ GSA HEADER
! THE FILE IS WRITTEN IN BIG ENDIAN MODE 
! This option is added also in compiler options anyway  

      OPEN(jplun,FILE=cfile,FORM='unformatted',STATUS='old',&
     & ACCESS='sequential' ,convert='BIG_ENDIAN')
    
      IF (llverb) THEN
      WRITE(*,*) 'Searching for parameter :',kpar, &
     &           'in GSA file: ',cfile,'...'
      ENDIF 

      READ(jplun) clid
      READ(jplun) clcom
!   This read statement below is not always valid!!!
!   In some versions of festat it is not written to the stabal file!!!
      READ(jplun) iorig,idate,itime,inbset
      IF (llverb) THEN 
        WRITE(*,*) 'ID     =',clid
        WRITE(*,*) 'COMMENT=',clcom
        WRITE(*,*) 'ORIGINE,DATE,TIME,INBSET',iorig,idate,itime,inbset
      ENDIF 

! 2. SEARCH THE SET 
      llfound=.false.
      DO jset=1,inbset
        READ(jplun) inbmat,iweight,itypmat,isetdist,ilendef
        !IF (llverb) THEN 
         WRITE(*,*) 'Scanning set No...',jset
        !ENDIF 
        IF (llverb) THEN 
          WRITE(*,*) '   ',inbmat,iweight,itypmat,isetdist,ilendef
        ENDIF 
        READ(jplun) idim1,idim2,ipar1,ipar2,icor1,icor2
        IF (llverb) THEN 
          WRITE(*,*) '   ',idim1,idim2,ipar1,ipar2,icor1,icor2
        ENDIF 
        IF ((ipar1==kpar).AND.(ipar2==kpar)) llfound=.true.

! skip coordinate records
        READ(jplun)
        READ(jplun)
        IF (llfound) THEN 
          IF (llverb ) THEN 
          WRITE(*,*) 'Found param  :', kpar ,' In the set No :', jset
          WRITE(*,*) 'Reading data ...'
          ENDIF  

! THE OLD ABORT ROUTINE CAUSES PYTHON CRASH 
          IF (inbmat/=nsmax+1)call abor1('Error - bad matrix number')
          IF (itypmat/=1     )call abor1('Error - not vert cov matrices')
          IF (isetdist/=2    )call abor1('Error - not distrib by wave n')
          IF (ilendef/=1     )call abor1('Error - bad def list length')
          IF (idim1/=kdim    )call abor1('Error - bad 1st dimension')
          IF (idim2/=kdim    )call abor1('Error - bad 2nd dimension')
          IF (icor1/=icor2   )call abor1('Error - coords differ')

          DO jn=0,nsmax
            READ(jplun)
            READ(jplun) ((pcov(jj,jk,jn),jk=1,kdim),jj=1,kdim),icheck
            IF (icheck/=3141592) call abor1('Error - wrong check word')
          ENDDO
          EXIT
        ELSE
! SKIP THE SET 
          DO jn=1,inbmat
            IF(ilendef>0) read(jplun)
            READ(jplun)
          ENDDO
        ENDIF
      ENDDO
      CLOSE(jplun)
      IF (llfound) THEN
        kret=0
      ELSE
        WRITE(*,*) 'WARNING : Could not find matrix set'
        pcov(:,:,:)=0.
        kret=1
      ENDIF 
      RETURN 
      END SUBROUTINE READCOV 

!-------------------------abor1 -------------------------
      SUBROUTINE  abor1(cdmess)
      character(len=*) :: cdmess
      WRITE(*,*)          cdmess
      !call exit(1)
      END 
! Finish 
