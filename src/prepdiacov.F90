PROGRAM PREPDIACOV

! program prepares Jb data [x,y] to be ploted using Gnuplot
!                                           M. Siroka, 09/1999

   IMPLICIT NONE

   INTEGER IARGC, NORDER
   INTEGER NMAX, NFIN, NFOUT,IDUM
   PARAMETER (NMAX=1000, NFIN=44, NFOUT=55)

   CHARACTER CFILE*300, CXNAM*25, CYNAM*25, CTITLE*40 
   INTEGER JX, JY, JNX, JNY, IXMIN, IYMIN, IXMAX, IYMAX
   REAL RXMIN, RXMAX

! input data are double precision

   DOUBLE PRECISION ZDAT(NMAX,NMAX)
   REAL ZOUT(NMAX,NMAX)
   REAL ZSCALE,rdum

! namelist definition
   
   NAMELIST /NAML/ ZSCALE, NORDER

! set defaults

   ZSCALE=1.0
   NORDER=1

! get input file name

   IF  (IARGC() .NE. 1) THEN
     PRINT*, 'Usage: prep_gnu inputfile'
     CALL EXIT(1)
   ENDIF

   CALL GETARG(1,CFILE)
   PRINT*," "
   PRINT*,'Input file name is: ',TRIM(CFILE)

! read namelist
   
   OPEN(10,file='namefile',status='OLD')
   PRINT*,'Namelist file opened'
   READ(10,NAML,END=2)
   
 2     CONTINUE

   print*,'scaling: ',ZSCALE,' ordering: ',NORDER

! open I/O files

   OPEN(NFIN,file=CFILE)
   OPEN(NFOUT,file='data.dat')
  
! read header (default 2 lines - see festat, fediacov, fediabal etc.)

    READ(NFIN,*) JNX, JNY, RXMIN, IYMIN, RXMAX, IYMAX 
    READ(NFIN,*) CXNAM, CYNAM, CTITLE
   
   IF (JNX > NMAX .OR. JNY > NMAX ) THEN
     PRINT*,"  "
     PRINT*,"ERROR: JNX > NMAX .OR. JNY > NMAX"
     PRINT*,"       source code of PREP_GNU must be modified"
     PRINT*,"  "
     CALL EXIT(1)
   ENDIF
   
   if (CFILE(1:8).eq.'corlogqu') then
   CTITLE='unbal specific humidity cors'
   endif

! write header (4 lines - titles for axes and plot on separate lines)

!      WRITE(NFOUT,*) JNX, JNY, IXMIN, IYMIN, IXMAX, IYMAX 
   WRITE(NFOUT,*) "#", JNX, JNY
   WRITE(NFOUT,'(2A)') "# ", TRIM(CXNAM)
   WRITE(NFOUT,'(2A)') "# ", TRIM(CYNAM)
   WRITE(NFOUT,'(2A)') "# ", TRIM(CTITLE)

! read data

!(libgsa: normal order of levels)         READ(NFIN,*) (ZDAT(JX,JY),JX=1,JNX)
!(libgsa_log: backward order on x axes)   READ(NFIN,*) (ZDAT(JX,JY),JX=JNX,1,-1)

   DO JY=1,JNY
     IF (NORDER .EQ. 1) THEN
       READ(NFIN,*) (ZDAT(JX,JY),JX=1,JNX)
     ELSEIF (NORDER .EQ. -1) THEN
       READ(NFIN,*) (ZDAT(JX,JY),JX=JNX,1,-1)
     ELSE
       PRINT*,'WRONG ORDER IDENTIFIER !'
       CALL EXIT(2) 
     ENDIF
   ENDDO

! write to output file 'data.dat', format for GNU

   DO JX=1,JNX
     DO JY=1,JNY
       ZOUT(JX,JY)=ZSCALE*ZDAT(JX,JY)
       WRITE(NFOUT,*) JX, JY, ZOUT(JX,JY)
     ENDDO
! insert empty line (grided datafile in gnuplot)
     WRITE(NFOUT,*)
   ENDDO
   
! finito

   CLOSE(NFIN)
   CLOSE(NFOUT)

   STOP
END PROGRAM PREPDIACOV


