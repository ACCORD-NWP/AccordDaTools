#!/usr/bin/env bash

# This script will write stats from Jb files in ASCII 
# suitable for plotting ...

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

PROGNAME=`basename $0`

# Define a usage function
usage() {

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - write Jb statistics

${bold}USAGE${normal}
        ${PROGNAME} -b <bal-file> -c <cv-file> -g <grid-spacing>
                    -l <num-levels> -d <diacov-binary> [-e <exp-name>]
                    [-p <print-level>] [ -h ]

${bold}DESCRIPTION${normal}
        Script to start new LBC processing suite

${bold}OPTIONS${normal}
        -b ${unline}bal-file${normal}
           bal file

        -c ${unline}cv-file${normal}
           cv file

        -g ${unline}grid-spacing${normal}
           Grid-spacing in metres
        
        -l ${unline}level-definition${normal}
           Level definition (A's and B's) 
        
        -d ${unline}diacov-binary${normal}
           PATH to DIACOV executabel compiled by makeup/GMKPACK.
        
        -e ${unline}exp-name${normal}
           Experiment name for Jb files. Statistic files will be written to a
           directory with name supplied

        -p ${unline}print-level${normal}
           Verbosity level (integer) in range 0 - 9.
           Default: 1

        -h Help! Print usage information.

USAGE
}

BALFILE=DUMMY
CVFILE=DUMMY
GRIDSIZE=DUMMY
LEVELNUM=DUMMY
BINPATH=DUMMY
EXPNAME=diacov_stat
PRINTLEV=1
#
# Where am I?
#
this_script_loc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
appdir=$(dirname ${this_script_loc})
bindir=${appdir}/bin
libdir=${appdir}/lib
exedir=${appdir}/libexec
levdir=${appdir}/share/levdef

if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

while getopts b:c:g:l:d:e:p:h option
do
  case $option in
    b)
       BALFILE=$OPTARG
       ;;
    c)
       CVFILE=$OPTARG
       ;;
    g)
       GRIDSIZE=$OPTARG
       ;;
    l)
       LEVELNUM=$OPTARG
       ;;

    d)
       BINPATH=$OPTARG
       ;;
    e)
       EXPNAME=$OPTARG
       ;;
    p)
       PRINTLEV=$OPTARG
       ;;
    h)
       usage
       exit 0
       ;;
    *)
       echo
       echo "Try '${PROGNAME} -h' for more information"
       ;;
  esac
done

if [ ${BALFILE} == "DUMMY" ]; then
  echo "Please define bal-file using -b"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ${CVFILE} == "DUMMY" ]; then
  echo "Please define cv-file using -c"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ${GRIDSIZE} == "DUMMY" ]; then
  echo "Please define grid-size using -g"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ${BINPATH} == "DUMMY" ]; then
  echo "Please location of DIACOV executabel using -p"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ${LEVELNUM} == "DUMMY" ]; then
  echo "Please define num-levels using -l"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ $PRINTLEV -gt 0 ]; then
  echo
  echo "BALFILE=${BALFILE}"
  echo "CVILE=${CVFILE}"
  echo "GRIDSIZE=${GRIDSIZE}"
  echo "LEVELNUM=${LEVELNUM}"
  echo
fi

#
# Construct fort.4 namelist
#

echo "&NAMDIACOV"                 > fort.4
echo "NFLEV=${LEVELNUM},"        >> fort.4
echo "NSMAX=24,"                 >> fort.4
echo "NDGL=50,"                  >> fort.4
echo "PPDELTAX=${GRIDSIZE},"     >> fort.4
echo "CJBTYPE='STABAL96',"       >> fort.4
echo "/"                         >> fort.4

#
# Create soft links for jbdiagnose.x
#
ln -s ${BALFILE} stabal.bal
ln -s ${CVFILE}  stabal.cv

if [ -d ${EXPNAME} ]; then
  echo "${EXPNAME} already exists. Please choose another experiemnt name"
else
  ${BINPATH}/DIACOV > diacov.log 2>&1
  mkdir $EXPNAME
  mv diacov.log $EXPNAME/
  mv conumdu.y  conumqu.y  conumtpsu.y  conumv.y ${EXPNAME}/
  mv corpsu.y ${EXPNAME}/
  mv lscdu.y lscqu.y lsctu.y lscvtot.y ${EXPNAME}/
  mv stdavdu.y stdavqu.y stdavtu.y stdavvtot.y stdevpsu.y ${EXPNAME}/
  mv varsppsu.y ${EXPNAME}/
  mv cordu.xy corpsu.xy corqu.xy cortu.xy corvtot.xy ${EXPNAME}/
  mv stdevdu.xy stdevlogdu.xy stdevlogqu.xy stdevlogtu.xy stdevlogvtot.xy ${EXPNAME}/
  mv stdevqu.xy stdevtu.xy stdevvtot.xy ${EXPNAME}/
  mv varspdu[0-9][0-9]*.y varspqu[0-9][0-9]*.y varsptu[0-9][0-9]*.y varspvtot[0-9][0-9]*.y ${EXPNAME}/
  mv corlogdu[0-9][0-9]*.xy corlogqu[0-9][0-9]*.xy corlogtu[0-9][0-9]*.xy corlogvtot[0-9][0-9]*.xy ${EXPNAME}/
  mv cordu[0-9][0-9]*.xy corqu[0-9][0-9]*.xy cortu[0-9][0-9]*.xy corvtot[0-9][0-9]*.xy ${EXPNAME}/
  echo "Statistics and log file moved to ${EXPNAME}/"
fi

rm -f stabal.cv
rm -f stabal.bal
rm -f fort.4

exit 0
