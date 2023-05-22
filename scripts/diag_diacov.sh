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
        ${PROGNAME} - process DIACOV output for plotting

${bold}USAGE${normal}
        ${PROGNAME} -d <input-dir>
                    -o <output-dir>
                    [ -h ]

${bold}DESCRIPTION${normal}
        Process DIACOV output for plotting for plotting using the 
        plotdiacov tool

${bold}OPTIONS${normal}
        -i ${unline}input-dir${normal}
           Input directory containing files produced by DIACOV

        -o ${unline}cv-file${normal}
           Output directory for processed files

        -h Help! Print usage information.

USAGE
}

INPDIR=DUMMY
OUTDIR=DUMMY
PRINTLEV=0
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

while getopts i:o:h option
do
  case $option in
    i)
       INPDIR=$OPTARG
       ;;
    o)
       OUTDIR=$OPTARG
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

if [ ${INPDIR} == "DUMMY" ]; then
  echo "Please define input-dir using -i"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ${OUTDIR} == "DUMMY" ]; then
  echo "Please define ouput-dir using -o"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ $PRINTLEV -gt 0 ]; then
  echo
  echo "$0: INPDIR=${INPDIR}"
  echo "$0: OUTDIR=${OUTDIR}"
  echo
fi

mkdir -p ${OUTDIR}

rm -f namefile
for var in q_pb q_divu tps_pb tps_divu q_tpsu; do
  FILE="${INPDIR}/expl${var}.xy"
  if [[ -e "$FILE" ]]; then
    ln -sf ${INPDIR}/expl${var}.xy expl${var}.xy
    cat > namefile << EOFCAT
&NAML
  NORDER=1,
  ZSCALE=1.0,
&END
EOFCAT
    ${exedir}/prepdiacov.x expl${var}.xy > expl${var}.log 2>&1
    mv data.dat ${OUTDIR}/expl${var}.dat
    rm expl${var}.xy namefile
  fi
done
echo "Successful. Output in ${OUTDIR}"
exit 0
