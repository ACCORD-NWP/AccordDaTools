#!/bin/bash

#set -xe
set -e

#1prepare_obstool.sh
#Once we have ascii files with odbsql we start with thi script to prepare the data.

PROGNAME=`basename $0`

# Define a usage function
usage() {

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - estimate observation errors

${bold}USAGE${normal}
        ${PROGNAME} -i <input-directory> -o <output-directory>
                    -s <start-dtg> -e <end-dtg>
                    [ -h ]

${bold}DESCRIPTION${normal}
        Prepare input data for ObsTool error covariance estimation

${bold}OPTIONS${normal}
        -i ${unline}input-directory${normal}
           input directory

        -o ${unline}output-directory${normal}
           output directory

        -s ${unline}start-dtg${normal}

        -e ${unline}end-dtg${normal}

        -c ${unline}forecast-cycle${normal}

        -n ${unline}expt-name${normal}
           Experiment name

        -h Help! Print usage information.

USAGE
} 

SDTG=DUMMY
EDTG=DUMMY
FINT=3
NEXP="DUMMY"

while getopts i:o:s:e:c:n:h option
do
  case $option in
    i)
       OTIN=$OPTARG
       ;;
    o)
       OTOUT=$OPTARG
#EW       if [ -d $OTOUT ]; then
#EW         echo "Directory ${OTOUT} already exists. Please choose another name"
#EW         exit 1
#EW       else
#EW         mkdir -p ${OTOUT}
#EW       fi
       ;;
    s)
       SDTG=$OPTARG
       ;;
    e)
       EDTG=$OPTARG
       ;;
    c)
       FINT=$OPTARG
       ;;
    n)
       NEXP=$OPTARG
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

#
# Where am I?
#
this_script_loc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
appdir=$(dirname ${this_script_loc})
bindir=${appdir}/bin
libdir=${appdir}/lib
exedir=${appdir}/libexec
levdir=${appdir}/share/levdef

#For gnss ztd, radar, radar_dow  and also for ascat: 
exe=preparing_conv.R

#==========================================================
# 		PREPARE STATISTICS
#==========================================================
CDTG=$SDTG
while [[ $CDTG -le $EDTG ]]; do
  echo
  echo "Processing CCMA data for ${CDTG} ..."
  if [ ! -s ${OTIN}/ccma_mon_conv_${CDTG} ]; then
    echo "  ${PROGNAME}: Input ${OTIN}/ccma_mon_conv_${CDTG} does not exist."
    echo "  ${PROGNAME}: Aborting ..."
    exit 1
  fi
  sed -e "s/XXYYMMDDNTXX/${CDTG}/" ${exedir}/$exe > ${CDTG}_${exe}

  # Start visualization in R
  echo "R CMD BATCH ${CDTG}_${exe}"
  R CMD BATCH ${CDTG}_${exe} && rm -f ${CDTG}_${exe}
  # increment by cycle h
  DSTR="${CDTG:0:4}-${CDTG:4:2}-${CDTG:6:2} ${CDTG:8:2}:00:00"
  CDTG=$(date -d "$DSTR $FINT hour" +%Y%m%d%H)
done

exit 0
