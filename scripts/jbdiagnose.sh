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
                    -l <level-definition> [-L] [ -h ]

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
        
        -L List available level definitions
        
        -h Help! Print usage information.

USAGE
}

BALFILE=DUMMY
CVILE=DUMMY
GRIDSIZE=DUMMY
LEVELDEF=DUMMY

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

while getopts b:c:g:l:Lh option
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
       LEVELDEF=$OPTARG
       ;;
    L)
       echo
       ls -1 ${levdir} | sed 's/.def//g'
       echo
       exit 0
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

if [ ${LEVELDEF} == "DUMMY" ]; then
  echo "Please define level-definition using -l"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

echo
echo "BALFILE=${BALFILE}"
echo "CVILE=${CVFILE}"
echo "GRIDSIZE=${GRIDSIZE}"
echo "LEVELDEF=${LEVELDEF}"
echo

#
# Construct namjbconv namelist
#
if [ ! -s ${levdir}/${LEVELDEF}.def ]; then
  echo "${levdir}/${LEVELDEF}.def not found"
  echo "Try $PROGNAME -L to list available level definitions"
  exit 1
fi

echo "&namjbconv" > jbconv.nam
echo "gsize_in  =  ${GRIDSIZE}," >> jbconv.nam
cat ${levdir}/${LEVELDEF}.def >> jbconv.nam
echo "/" >> jbconv.nam

#
# Create soft links for jbdiagnose.x
#
ln ${BALFILE} stabal96.bal
ln ${CVFILE}  stabal96.cv

${exedir}/jbdiagnose.x < jbconv.nam

rm -f stabal96.cv
rm -f stabal96.bal
rm -f jbconv.nam

exit 0
