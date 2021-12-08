#! /bin/ksh
##-------------------------------------------------------------------------
## Script for preparation of Jb data in [x,y] format for plotting
##           based on script from Jozo & Mariska, september 1999
##           A. Bucanek, December 2018
##-------------------------------------------------------------------------

# input arguments
if [ $# -ne 3 ] ; then
  echo "Usage: prep.ksh inputfile scale order " ; exit 1
fi

## set names and paths
filein=$1
ZSCALE=$2
NORDER=$3

## prepare program, which reads input file and puts it to proper format for Gnuplot
## output file is named data.dat
##  (unfortunatelly program is not yet fully universal, because not all 
##   possibilities of pictures were checked)

if [ ! -f prep_gnu.exe ] ; then

\cat << FIN > prep_gnu.F
       PROGRAM PREP_GNU  

c program prepares Jb data [x,y] to be ploted using Gnuplot
c                                           M. Siroka, 09/1999

       IMPLICIT NONE

       INTEGER IARGC, NORDER
       INTEGER NMAX, NFIN, NFOUT,IDUM
       PARAMETER (NMAX=1000, NFIN=44, NFOUT=55)

       CHARACTER CFILE*300, CXNAM*25, CYNAM*25, CTITLE*40 
       INTEGER JX, JY, JNX, JNY, IXMIN, IYMIN, IXMAX, IYMAX

c input data are double precision

       DOUBLE PRECISION ZDAT(NMAX,NMAX)
       REAL ZOUT(NMAX,NMAX)
       REAL ZSCALE,rdum

c namelist definition
       
       NAMELIST /NAML/ ZSCALE, NORDER

c set defaults

       ZSCALE=1.0
       NORDER=1

c get input file name

       IF  (IARGC() .NE. 1) THEN
         PRINT*, 'Usage: prep_gnu inputfile'
         CALL EXIT(1)
       ENDIF

       CALL GETARG(1,CFILE)
       PRINT*," "
       PRINT*,'Input file name is: ',TRIM(CFILE)

c read namelist
   
       OPEN(10,file='namefile',status='OLD')
       PRINT*,'Namelist file opened'
       READ(10,NAML,END=2)
   
 2     CONTINUE

       print*,'scaling: ',ZSCALE,' ordering: ',NORDER

c open I/O files

       OPEN(NFIN,file=CFILE)
       OPEN(NFOUT,file='data.dat')
      
c read header (default 2 lines - see festat, fediacov, fediabal etc.)

c       READ(NFIN,*) JNX, JNY, IXMIN, IYMIN, IXMAX, IYMAX 
       READ(NFIN,*) JNX, JNY 
       IF (JNX > NMAX .OR. JNY > NMAX ) THEN
         PRINT*,"  "
         PRINT*,"ERROR: JNX > NMAX .OR. JNY > NMAX"
         PRINT*,"       source code of PREP_GNU must be modified"
         PRINT*,"  "
         CALL EXIT(1)
       ENDIF
       if (CFILE(1:4).eq.'expl') then
       READ(NFIN,*) rdum,rdum
       endif
       if (CFILE(1:8).eq.'stdevlog') then
       READ(NFIN,*) rdum,rdum                       
       endif   
       if (CFILE(1:6).eq.'corlog') then
       READ(NFIN,*) rdum,rdum                                                                                                                                    
       endif
       if (CFILE(1:8).eq.'corlogqu') then
       READ(NFIN,*) CXNAM, CYNAM, CTITLE
c       READ(NFIN,*) CTITLE
       CTITLE='unbal specific humidity cors'
       else
       READ(NFIN,*) CXNAM, CYNAM, CTITLE
       endif

c write header (4 lines - titles for axes and plot on separate lines)

c      WRITE(NFOUT,*) JNX, JNY, IXMIN, IYMIN, IXMAX, IYMAX 
       WRITE(NFOUT,*) "#", JNX, JNY
       WRITE(NFOUT,'(2A)') "# ", TRIM(CXNAM)
       WRITE(NFOUT,'(2A)') "# ", TRIM(CYNAM)
       WRITE(NFOUT,'(2A)') "# ", TRIM(CTITLE)

c read data

c(libgsa: normal order of levels)         READ(NFIN,*) (ZDAT(JX,JY),JX=1,JNX)
c(libgsa_log: backward order on x axes)   READ(NFIN,*) (ZDAT(JX,JY),JX=JNX,1,-1)

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

c write to output file 'data.dat', format for GNU

       DO JX=1,JNX
         DO JY=1,JNY
           ZOUT(JX,JY)=ZSCALE*ZDAT(JX,JY)
           WRITE(NFOUT,*) JX, JY, ZOUT(JX,JY)
         ENDDO
c insert empty line (grided datafile in gnuplot)
         WRITE(NFOUT,*)
       ENDDO
   
c finito

       CLOSE(NFIN)
       CLOSE(NFOUT)

       STOP
       END
FIN
# compile
  gfortran -o prep_gnu.exe prep_gnu.F
  rm -fr prep_gnu.F
else
  touch prep_gnu.exe
fi

## namelist

## (you may need to adjust namelist below for ordering of your data (for yet
##  unknow reasons sometimes data are written from 1..nflev or 0..nsmax and
##  sometimes from nflev..1); and for scaling of data (GMT does not plot too
##  small values))

/bin/rm -f namefile
cat > namefile << EOFCAT
 &NAML
     NORDER=${NORDER},
     ZSCALE=${ZSCALE},
 &END
EOFCAT

## prepare data --> data.dat

./prep_gnu.exe $filein
if [ $? -ne 0 ] ; then 
  echo "PB in program prep_gnu" ; exit 180
fi
