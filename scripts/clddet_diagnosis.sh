#!/bin/bash

#set -xe
#set -e

#Original::

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
                    [ -h ]

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

        -h Help! Print usage information.

USAGE
}


if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

#Defaults
INDIR=DUMMY
OUTDIR=DUMMY
DATE=DUMMY
SENSOR=DUMMY

while getopts i:o:d:s:h option
do
  case $option in
    i)
       INDIR=$OPTARG
       if [ -d $INDIR ]; then
         echo "Directory ${INDIR} "
       else
         echo "Directory ${INDIR} does not exist. Please choose another name"
         exit 1
       fi
       ;;
    o)
       OUTDIR=$OPTARG
       if [ -d $OUTDIR ]; then
         #echo "Directory ${outdir} already exists. Please choose another name"
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

expid=$INDIR     
andate=$DATE
instrument=$SENSOR

FETCH_INPUT=1
RUN_FORTRAN=1
CLEAN=1


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

#---
# 1 Preparations that will always need to be done
#

echo
echo "Started at"
date
echo

# Identify working directories:

# 1) Directory where the script is run from
pwd > pwd.txt
read workdir < pwd.txt
rm -f pwd.txt

# 2) Directory for potentially large output ASCII files
outdir=${workdir}/$OUTDIR

# 3) Personal base directory that contains user's HARMONIE experiments:
indir=${workdir}/$INDIR

# 4) Directory with *.f90 or *.F90:
srcdir=${workdir}/src


echo
echo Working directories are set as follows:
echo $workdir  
echo $outdir   
echo $indir 
echo $scrdir 
echo

if [ ${instrument} = "iasi" ]; then
  nmlfile="IASI_CLDDET.NL"
  capital="IASI"
elif [ ${instrument} = "cris" ]; then
  nmlfile="CRIS_CLDDET.NL"
  capital="CRIS"
elif [ ${instrument} = "airs" ]; then
  nmlfile="AIRS_CLDDET.NL"
  capital="AIRS"
else
  echo "FATAL: Unknown instrument: "${instrument}
  FETCH_INPUT=0
  ODB_REQUEST=0
  RUN_FORTRAN=0
  CLEAN=0
fi


#---
# 2 Fetch input files: relevant source code, namelist file, HM_Date
#   log file, odb files (ECMA.*).
#

if [ ${FETCH_INPUT} -eq 1 ]; then

  # Cloud detection tuning parameter values will be read from file
  # cloud_detect_setup.F90. If this file exists in experiment's work
  # directory, use this version. Otherwise read it from the HARMONIE
  # reference code.

  echo
  echo Fetching input files ...
  echo

  exp_src_file=${srcdir}/cloud_detect_setup.F90
  if [ -s ${exp_src_file} ]; then   # Source code in experiment work directory
      echo Found ${exp_src_file}
      cp ${exp_src_file} ${outdir}/.
  else
      echo "FATAL: No cloud_detect_setup.F90 file found."
      exit
  fi

  # Namelist variables will be read, if possible, from file
  # IASI_CLDDET.NL (or CRIS_CLDDET.NL). Again, search first from the
  # experiment's work directory.

  exp_namelist_file=${srcdir}/${nmlfile}

  if [ -s ${exp_namelist_file} ]; then   # Namelist file in experiment directory
    echo Found ${exp_namelist_file}
    cp ${exp_namelist_file} ${outdir}/.
  else
    echo "Warning: Found no cloud detection namelist file "${nmlfile}
    exit
  fi


cd ${outdir}

  # Fetch the log file
  echo
  echo Fetch HM_Date.html from ...

  hm_date=${indir}/HM_Date_${andate}.html

  if [ -s ${hm_date} ]; then   # Source code in experiment work directory
      echo ... found HM_Date.html
      cp ${hm_date} ${outdir}/HM_Date.html
  else
      echo "FATAL: file " $hm_date " not found."
      exit
  fi

  # Fetch ascii file (clddet_ascii.dat)
  echo
  echo Fetch clddet_ascii.dat

  outf=${indir}/clddet_ascii.dat.${andate}

  if [ -s ${outf} ]; then   # Source code in experiment work directory
      echo ... found /clddet_ascii.dat
      cp ${outf} ${outdir}/clddet_ascii.dat
  else
      echo "FATAL: file " $outf " not found."
      exit
  fi

fi # if [ ${FETCH_INPUT} -eq 1 ]; ...

#---
# 2 Running the FORTRAN code
#

if [ ${RUN_FORTRAN} -eq 1 ]; then

  echo
  echo Running the FORTRAN code ...
  echo

  cd ${outdir}
  gfortran -o clddet_analyzer.x ${srcdir}/clddet_analyzer.f90
  ./clddet_analyzer.x ${capital}

  cd ${workdir}
  ln -sf ${outdir}/clddet_sorted_smoothed.dat .

fi # if [ ${RUN_FORTRAN} ]; then

echo
echo "Output available in clddet_sorted_smoothed.dat"
echo

#---
# Cleaning
#

if [ ${CLEAN} -eq 1 ]; then

  echo
  echo Cleaning ...
  echo

  # Cleaning
  cd ${outdir}
  rm -f clddet_analyzer.x
  rm -f HM_Date.html
  rm -f IASI_CLDDET.NL
  rm -f cloud_detect_setup.F90

  echo ... done.
  echo

fi # if [ ${CLEAN} -eq 1 ]; then


echo
echo "Finished at"
date
echo

