#!/usr/bin/env bash


PROGNAME=`basename $0`

# Define a usage function
usage() {

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - Infrared radiance cloud detection diagnostic tool

${bold}USAGE${normal}
        ${PROGNAME} -i <input-directory> -o <output-directory> -d <an-date> -s <sensor>
                    [ -w ] window width [ -t ] bt threshold [ -h ] help

${bold}DESCRIPTION${normal}
        This diagnostic tool is intended for use in evaluation of the
        performance of the infrared radiance cloud detection scheme in the
        context of HARMONIE-AROME data assimilation systems.

${bold}OPTIONS${normal}
        -i ${unline}input-directory${normal}
           input directory (usually experiment name)

        -o ${unline}output-directory${normal}
           output directory

        -d ${unline}an-date${normal}
           Date and time of the analysis 
           YYYYMMDDHH 

        -s ${unline}sensor${normal}
           Name of the infrared sounder of interest
           iasi/cris/airs

        -w ${unline}i_window_width${normal}
           Window width for cloud detection smoothing
           Default i_window_width = 10

        -t ${unline}r_bt_thres${normal}
           Brightness temperature threshold
           Default r_bt_thres = 0.50

        -h Help! Print usage information.

USAGE
}

#Defaults
INDIR=DUMMY
OUTDIR=DUMMY
DATE=DUMMY
SENSOR=DUMMY
I_WINDOW_WIDTH=10
R_BT_THRES=0.5

#
# Where am I?
#
this_script_loc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
appdir=$(dirname ${this_script_loc})
appdir=$(dirname ${this_script_loc})
bindir=${appdir}/bin
libdir=${appdir}/lib
exedir=${appdir}/libexec

if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

while getopts i:o:d:s:w:t:h option
do
  case $option in
    i)
       INDIR=$OPTARG
       echo $INDIR
       if [ -d $INDIR ]; then
         echo "Directory with data ${INDIR} "
       else
         echo "Directory ${INDIR} does not exist. Please choose another name"
         exit 1
       fi
       ;;
    o)
       OUTDIR=$OPTARG
       if [ -d $OUTDIR ]; then
         echo "Directory ${OUTDIR} already exists. Please choose another name"
         exit 1
       else
         mkdir -p ${OUTDIR}
       fi
       ;;
    d)
       DATE=$OPTARG
       echo "Date   " $DATE
       ;;
    s)
       SENSOR=$OPTARG
       ;;
    w)
       I_WINDOW_WIDTH=$OPTARG
       ;;
    t)
       R_BT_THRES=$OPTARG
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

andate=$DATE
instrument=$SENSOR
width=$I_WINDOW_WIDTH
thres=$R_BT_THRES

FETCH_INPUT=1
RUN_FORTRAN=1
CLEAN=1


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#---
# 1 Preparations that will always need to be done
#

# Identify working directories:

echo Working directories are set as follows:
workdir=${this_script_loc}
echo "1) Directory where the script is run from:"
echo $workdir

echo "2) Directory for potentially large output ASCII files:" 
echo $OUTDIR   

echo "3) Personal base directory that contains user's HARMONIE experiments:"
echo $INDIR

if [ ${instrument} = "iasi" ]; then
  capital="IASI"
elif [ ${instrument} = "cris" ]; then
  capital="CRIS"
elif [ ${instrument} = "airs" ]; then
  capital="AIRS"
else
  echo "FATAL: Unknown instrument: "${instrument}
fi

#---
# 2 Fetch input files: relevant source code (~/src/clddet_analyzer.F90),
#   HM_Date log file and odb file from ECMA (clddet_ascii.dat.*).
#

# Fetch the log file
hm_date=${INDIR}/HM_Date_${andate}.html

if [ -s ${hm_date} ]; then   # Source code in experiment work directory
  echo "${PROGNAME}: Found log file, ${INDIR}/HM_Date_${andate}.html"
  cp ${hm_date} ${OUTDIR}/HM_Date.html
else
  echo "FATAL: file " $hm_date " not found."
  exit 1
fi

# Fetch ascii file (clddet_ascii.dat)
outf=${INDIR}/clddet_ascii.dat.${andate}
if [ -s ${outf} ]; then   # Source code in experiment work directory
  echo "${PROGNAME}: Found input data, ${INDIR}/clddet_ascii.dat.${andate} "
  cp ${outf} ${OUTDIR}/clddet_ascii.dat
else
  echo "${PROGNAME}: FATAL: file " $outf " not found. Exiting ..."
  exit 1
fi

#---
# 2 Running the FORTRAN code
#
echo "${PROGNAME}: Running clddet_analyzer.x ..."

cd ${OUTDIR}
${exedir}/clddet_analyzer.x ${capital} ${width} ${thres}

cd ${this_script_loc}
ln -sf ${OUTDIR}/clddet_sorted_smoothed.dat .

echo "${PROGNAME}: clddet_analyzer.x complete"
echo "${PROGNAME}: Output available in clddet_sorted_smoothed.dat"

#---
# Cleaning
#
echo "${PROGNAME}: Cleaning ..."
cd ${OUTDIR}
rm -f clddet_analyzer.x
rm -f HM_Date.html
rm -f clddet_ascii.dat

exit 0
