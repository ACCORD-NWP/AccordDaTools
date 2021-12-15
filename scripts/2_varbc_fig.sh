#!/bin/bash

#set -x
#set -xe

#Old::
# after runnning 1_extract_VARBC_cycle.ksh files
# VARBC_${sat}_${sensor}_${channel}_${YYYY}${MM}${DD}_${HH} files
# in $DIR_TMP are grouped with selected loops for sat, sensor, 
# channels and cycle (hh) which will be used as the input for the Rscript
# which will plot the time evolution of the coefficients

#Current::
# Scan directory for VarBC predictor data files (produced by xxxx) and 
# produce time-series plots


PROGNAME=`basename $0`

# Define a usage function
usage() {

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - plot VarBC predictor time-series

${bold}USAGE${normal}
        ${PROGNAME} -i <input-directory> -o <output-directory>
                    [ -h ]

${bold}DESCRIPTION${normal}
        Plot VarBC predictor information from VARBC.cycle files
        produced by IAAAH NWP System.

${bold}OPTIONS${normal}
        -i ${unline}input-directory${normal}
           input directory

        -o ${unline}output-file${normal}
           output file-name

        -S ${unline}list-sat${normal}
           colon separted list of satellites to process. For example,
           -S 3:4:5 to process Metop-A, Metop-B and Metop-C. See
           https://apps.ecmwf.int/odbgov/satelliteidentifier/ for more
           details.

        -s ${unline}list-sen${normal}
           colon separted list of sensors to process. For example,
           -s 3:15 to process AMSU-A and MHS. See 
           https://apps.ecmwf.int/odbgov/sensor/ for more details.

        -b ${unline}begin-dtg${normal}
           DTG (YYYYMMDDHH) to start time-series

        -e ${unline}end-dtg${normal}
           DTG (YYYYMMDDHH) to end time-series

        -x ${unline}exp-name${normal}
           Experiment name

        -c ${unline}assim-cycle${normal}
           Interval between cycles. [default: 24]

        -h Help! Print usage information.

USAGE
}

if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

#Defaults
VARBCINP=DUMMY
VARBCOUT=varbc_pred.png
VARBCOUT=DUMMY
SATLIST=DUMMY
SENLIST=DUMMY
DTGBEG=DUMMY
DTGBEG=DUMMY
FCINT=24
EXPNAME=EXP

while getopts i:o:S:s:b:e:c:x:h option
do
  case $option in
    i)
       VARBCINP=$OPTARG
       ;;
    o)
       VARBCOUT=$OPTARG
       ;;
    S)
       SATLIST=$OPTARG
       ;;
    s)
       SENLIST=$OPTARG
       ;;
    b)
       DTGBEG=$OPTARG
       ;;
    e)
       DTGEND=$OPTARG
       ;;
    c)
       FCINT=$OPTARG
       ;;
    x)
       EXPNAME=$OPTARG
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

if [ "${VARBCINP}" == "DUMMY" ]; then
  echo "Please specify input directory using '-i'."
  echo "Try '$0 -h' for more information"
  exit 1
fi

if [ ! -d "${VARBCINP}" ]; then
  echo "Directory ${VARBCINP} does not exist."
  echo "Please check"
  exit 1
fi

if [ "${SATLIST}" == "DUMMY" ]; then
  echo "Please specify satellite identifier using '-S'."
  echo "Try '$0 -h' for more information"
  exit 1
fi

if [ "${SENLIST}" == "DUMMY" ]; then
  echo "Please specify satellite sensor using '-s'."
  echo "Try '$0 -h' for more information"
  exit 1
fi

if [ "${DTGBEG}" == "DUMMY" ]; then
  echo "Please specify DTGBEG using '-b'."
  echo "Try '$0 -h' for more information"
  exit 1
fi

if [ "${DTGEND}" == "DUMMY" ]; then
  echo "Please specify DTGBEG using '-e'."
  echo "Try '$0 -h' for more information"
  exit 1
fi

# Derive npred from first input files

list_sat=$(echo $SATLIST | sed 's/:/ /g')
list_sensor=$(echo $SENLIST | sed 's/:/ /g')

for sat in $list_sat
do

  # list_hh depends on the satellite and the domain
  if [[ $sat == 70  &&  $FCINT == "03" ]] ; then 
  #   list_sensor="29" ; list_hh="ALL"
      list_hh="ALL"
  else
    case $sat in
      209) list_hh="09 21" ;; 
      223) list_hh="06 18" ;;
      3|4|5) list_hh="09 12 21" ;;
      70) list_hh="00 06 12 18" ;;
    esac
  fi
  for sensor in $list_sensor; do
    if [[ $sat == 209  &&  $sensor == 3 ]] ; then list_channel="6 7 8"; fi
    if [[ $sat == 223  &&  $sensor == 3 ]] ; then list_channel="6 9"; fi
    if [[ $sat == 223  &&  $sensor == 15 ]] ; then list_channel="4 5"; fi
    if [[ $sat == 3  &&  $sensor == 3 ]] ; then list_channel="6 7 8 9"; fi
    if [[ $sat == 3  &&  $sensor == 15 ]] ; then list_channel="3 4 5"; fi
    if [[ $sat == 4  &&  $sensor == 3 ]] ; then list_channel="6 9"; fi
    if [[ $sat == 4  &&  $sensor == 15 ]] ; then list_channel="3 4 5"; fi
    if [[ $sat == 5  &&  $sensor == 3 ]] ; then list_channel="6 7 8 9"; fi
    if [[ $sat == 5  &&  $sensor == 15 ]] ; then list_channel="3 4 5"; fi
    if [[ $sat == 70  &&  $sensor == 29 ]] ; then list_channel="2 3 4 6 7 8"; fi

    for channel in $list_channel; do
      for hh in $list_hh; do
        file_out=${VARBCOUT}/VARBC_${sat}_${sensor}_${channel}_${hh}
        if [ -s ${file_out} ] ; then 
          rm ${file_out} 
        fi
        #DTG=$DTGBEG
        YYYY=`echo $DTGBEG|cut -c1-4`
        MM=`echo $DTGBEG|cut -c5-6`
        DD=`echo $DTGBEG|cut -c7-8`
        HH=`echo $DTGBEG|cut -c9-10`
        if [ $FCINT == "03" ] ; then
          DTG=$DTGBEG
        else
          DTG=$YYYY$MM$DD$hh
        fi
        echo "Start " $DTG $sat $sensor $channel $hh 
        while [ $DTG -le $DTGEND ]; do
          YYYY=`echo $DTG|cut -c1-4`
          MM=`echo $DTG|cut -c5-6`
          DD=`echo $DTG|cut -c7-8`
          HH=`echo $DTG|cut -c9-10`
          if [ $FCINT == "03" ] ; then
            ff=${VARBCINP}/VARBC_${sat}_${sensor}_${channel}_${YYYY}${MM}${DD}_${HH}0000
          else
            ff=${VARBCINP}/VARBC_${sat}_${sensor}_${channel}_${YYYY}${MM}${DD}_${hh}0000
          fi
#Hard-codeed for now
#Should be able to scrape from first input file
          npred=5
          if [ -s  $ff ]  ; then
            cat $ff >> ${file_out}
          else
            # if no there is not data in $DTG, a dummy line is included to avoid problems with varbc_plot.Rsh
            # echo $YYYY $MM $DD $HH $key $ndata $npred $params $param0 $predcs 
            nobs=0
            if [ $npred -eq 1 ] ; then
              echo ${YYYY}${MM}${DD} ${HH}0000 $sat $sensor $channel $nobs $npred "NaN NaN 0" >> ${file_out}
            else
              echo ${YYYY}${MM}${DD} ${HH}0000 $sat $sensor $channel $nobs $npred "NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN 0 1 8 9 10" >> ${file_out}
            fi
          fi
          DTG=$(date -d "${YYYY}-${MM}-${DD} ${HH}:00:00 ${FCINT} hour" '+%Y%m%d%H')
        done # end do while DTG
        echo "Plotting " $EXPNAME $sat $sensor $channel $hh 
#        varbc_plot.Rsh $sat $sensor $channel $hh $EXP > /dev/null 2>&1
        varbc_plot $sat.Rsh $sensor $channel $hh EXPNAME ${VARBCOUT}
      done # for hh
    done # for channel
  done # for sensor
done # for sat

exit 0
