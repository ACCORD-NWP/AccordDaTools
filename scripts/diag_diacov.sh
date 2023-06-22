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
        Process DIACOV output for plotting using the 
        plotdiacov tool

${bold}OPTIONS${normal}
        -i ${unline}input-dir${normal}
           Input directory containing files produced by DIACOV

        -o ${unline}output-dir${normal}
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
echo "Processing DIACOV output in ${INPDIR}."
echo "Output will be written to ${OUTDIR}"

rm -f namefile
#---------------------
prefix="expl"
for var in q_pb q_divu tps_pb tps_divu q_tpsu; do
  FILE="${INPDIR}/${prefix}${var}.xy"
  if [[ -e "$FILE" ]]; then
    ln -sf ${INPDIR}/${prefix}${var}.xy ${prefix}${var}.xy
    cat > namefile << EOFCAT
&NAML
  NORDER=1,
  ZSCALE=1.0,
&END
EOFCAT
    ${exedir}/prepdiacov.x ${prefix}${var}.xy > ${OUTDIR}/${prefix}${var}.log 2>&1
    mv data.dat ${OUTDIR}/${prefix}${var}.dat
    rm ${prefix}${var}.xy namefile
  fi
done
#---------------------
prefix="cor"
for var in tp d v t q qu du tu ps; do
  FILE="${INPDIR}/${prefix}${var}.xy"
  if [[ -e "$FILE" ]]
  then
    ln -sf ${INPDIR}/${prefix}${var}.xy ${prefix}${var}.xy
    cat > namefile << EOFCAT
&NAML
  NORDER=-1,
  ZSCALE=1.0,
&END
EOFCAT
    ${exedir}/prepdiacov.x ${prefix}${var}.xy > ${OUTDIR}/${prefix}${var}.log 2>&1
    mv data.dat ${OUTDIR}/${prefix}${var}.dat
    rm ${prefix}${var}.xy namefile
  fi
done
#---------------------
prefix="cov"
for var in dp ; do
  FILE="${INPDIR}/${prefix}${var}.xy"
  if [[ -e "$FILE" ]]
  then
    ln -sf ${INPDIR}/${prefix}${var}.xy ${prefix}${var}.xy
    cat > namefile << EOFCAT
&NAML
  NORDER=-1,
  ZSCALE=100000,
&END
EOFCAT
    ${exedir}/prepdiacov.x ${prefix}${var}.xy > ${OUTDIR}/${prefix}${var}.log 2>&1
    mv data.dat ${OUTDIR}/${prefix}${var}.dat
    rm ${prefix}${var}.xy namefile
  fi
done
#---------------------
prefix="cov"
for var in td ; do
  FILE="${INPDIR}/${prefix}${var}.xy"
  if [[ -e "$FILE" ]]
  then
    ln -sf ${INPDIR}/${prefix}${var}.xy ${prefix}${var}.xy
    cat > namefile << EOFCAT
&NAML
  NORDER=-1,
  ZSCALE=1000000,
&END
EOFCAT
    ${exedir}/prepdiacov.x ${prefix}${var}.xy > ${OUTDIR}/${prefix}${var}.log 2>&1
    mv data.dat ${OUTDIR}/${prefix}${var}.dat
    rm ${prefix}${var}.xy namefile
  fi
done
#---------------------
prefix="cov"
for var in qp qd qt ; do
  FILE="${INPDIR}/${prefix}${var}.xy"
  if [[ -e "$FILE" ]]
  then
    ln -sf ${INPDIR}/${prefix}${var}.xy ${prefix}${var}.xy
    cat > namefile << EOFCAT
&NAML
  NORDER=-1,
  ZSCALE=1000,
&END
EOFCAT
    ${exedir}/prepdiacov.x ${prefix}${var}.xy > ${OUTDIR}/${prefix}${var}.log 2>&1
    mv data.dat ${OUTDIR}/${prefix}${var}.dat
    rm ${prefix}${var}.xy namefile
  fi
done
#---------------------
prefix="stdev"
for var in v d du q qu tu logv logd logdu logq logqu logt logtu; do
  FILE="${INPDIR}/${prefix}${var}.xy"
  if [[ -e "$FILE" ]]
  then
    ln -sf ${INPDIR}/${prefix}${var}.xy ${prefix}${var}.xy
    cat > namefile << EOFCAT
&NAML
  NORDER=1,
  ZSCALE=1,
&END
EOFCAT
    ${exedir}/prepdiacov.x ${prefix}${var}.xy > ${OUTDIR}/${prefix}${var}.log 2>&1
    mv data.dat ${OUTDIR}/${prefix}${var}.dat
    rm ${prefix}${var}.xy namefile
  fi
done
#---------------------
prefix="stdev"
for var in vtot logvtot ; do
  FILE="${INPDIR}/${prefix}${var}.xy"
  if [[ -e "$FILE" ]]
  then
    ln -sf ${INPDIR}/${prefix}${var}.xy ${prefix}${var}.xy
    cat > namefile << EOFCAT
&NAML
  NORDER=-1,
  ZSCALE=1000000,
&END
EOFCAT
    ${exedir}/prepdiacov.x ${prefix}${var}.xy > ${OUTDIR}/${prefix}${var}.log 2>&1
    mv data.dat ${OUTDIR}/${prefix}${var}.dat
    rm ${prefix}${var}.xy namefile
  fi
done
#---------------------
prefix="cor"
for var in vtot v d du q qu t tu logvtot logv logd logdu logq logqu logt logtu; do
  NUMBERS=$(ls ${INPDIR}/${prefix}${var}??.xy | xargs -n 1 basename | tr '\n' ' ' | sed -e 's/[^0-9]/ /g' -e 's/^ *//g' -e 's/ *$//g' | tr -s ' ' | sed 's/ /\n/g')
  for lev in $NUMBERS; do
#  for lev in 05 11 17 23 29 35 41 47 53 59 65; do
    FILE="${INPDIR}/${prefix}${var}${lev}.xy"
    if [[ -e "$FILE" ]]
    then
      ln -sf ${INPDIR}/${prefix}${var}${lev}.xy ${prefix}${var}${lev}.xy
      cat > namefile << EOFCAT
&NAML
  NORDER=-1,
  ZSCALE=1000000,
&END
EOFCAT
      ${exedir}/prepdiacov.x ${prefix}${var}${lev}.xy > ${OUTDIR}/${prefix}${var}${lev}.log 2>&1
      mv data.dat ${OUTDIR}/${prefix}${var}${lev}.dat
      rm ${prefix}${var}${lev}.xy namefile
    fi
  done
done

echo "Successful. Output in ${OUTDIR}"
exit 0
