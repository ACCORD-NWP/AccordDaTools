#!/bin/bash

#set -xe
#set -e

#Original::
# Loop from DTGBEG to DTGEND where for each cycle (DTG) the VARBC.cycle file
# is splitted in several one-line files according to satellite, sensor, channel, 
# the so-called VARBC_${sat}_${sensor}_${channel}_${YYYY}${MM}${DD}_${HH} files
# stored in $DIR_TMP
# 
#Currenta::
# Scan directory for VARBC.cycle files and write all information
# to output directory

#TODO:
#    * add satellite_id option - do we want this? -n
#    * add satellite_sensor option - do we want this? -s
#    * add DTGBEG option - do we want this? -b
#    * add DTGEND option - do we want this? -e


#
# Scan input directory for VARBC.cycle files
#

PROGNAME=`basename $0`

# Define a usage function
usage() {

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - process VarBC predictor information

${bold}USAGE${normal}
        ${PROGNAME} -i <input-directory> -o <oupput-directory>
                    [ -h ]

${bold}DESCRIPTION${normal}
        Script to process VarBC predictor information from VARBC.cycle files
        produced by IAAAH NWP System.

${bold}OPTIONS${normal}
        -i ${unline}bal-file${normal}
           input directory

        -o ${unline}cv-file${normal}
           output directory

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
VARBCOUT=DUMMY

while getopts i:o:h option
do
  case $option in
    i)
       VARBCINP=$OPTARG
       ;;
    o)
       VARBCOUT=$OPTARG
       mkdir -p ${VARBCOUT}
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

echo "$0: Starting to loop over files ..."

find ${VARBCINP} -name "VARBC.cycle" | while read FILEIN; do

  YYYYMMDD=$(awk 'NR == 2 {print $2; exit}' ${FILEIN})
  HHMMSS_S=$(awk 'NR == 2 {print $3; exit}' ${FILEIN})
  HHMMSS=$(printf "%06d\n" ${HHMMSS_S})
  echo "$0: Processing ${FILEIN} ${YYYYMMDD} ${HHMMSS}"

  # $2 is the number of observations, so for $2 > 1 only satellite channels are selected 
  # so GNSS stations are excluded

  # Find line numbers where ndata > 1
  list_nl=`awk 'BEGIN {FS="="}; $1=="ndata" && $2> 1 {print NR}' ${FILEIN}` 

  for nl in ${list_nl}; do
    #DBG echo "    processing line number ${nl}"
    # ndata -> nl ; line where ndata>1
    ndata=`sed -n -e "${nl}"p ${FILEIN} |cut -c7-`

    # key -> nl-2 ; two lines up
    n1=`expr $nl - 2`; key=`sed -n -e "${n1}"p ${FILEIN} |cut -c5-`
    # npred -> nl+1 ; one line down
    n2=`expr $nl + 1`; npred=`sed -n -e "${n2}"p ${FILEIN} |cut -c7-`
    # predcs -> nl+2 ; two lines down
    n3=`expr $nl + 2`; predcs=`sed -n -e "${n3}"p ${FILEIN} |cut -c8-`
    # param0 -> nl+3 ; three lines down
    n4=`expr $nl + 3`; param0=`sed -n -e "${n4}"p ${FILEIN} |cut -c8-`
    # params -> nl+4 ; four lines down
    n5=`expr $nl + 4`; params=`sed -n -e "${n5}"p ${FILEIN} |cut -c8-`

    # name_key= $sat_$sensor_$channel
    name_key=`echo $key|awk '{print $1"_"$2"_"$3}'`
    sat=`echo $key|awk '{print $1}'`
    sensor=`echo $key|awk '{print $2}'`
    channel=`echo $key|awk '{print $3}'`

    # Only NOAA-18, 19 (209 and 223) and METOP-A,B,C (4,3,5) and METOPSAT-11 are selected
    file_out=VARBC_${name_key}_${YYYYMMDD}_${HHMMSS}
   
#    for SAT in $list_sat ; do
#      for SENSOR in $list_sensor ; do
#        if [[ $sat == $SAT  &&  $sensor == $SENSOR ]] ; then
#          echo $YYYY $MM $DD $HH $key $ndata $npred $params $param0 $predcs > $VARBCOUT/${file_out}
          echo $YYYYMMDD $HHMMSS $key $ndata $npred $params $param0 $predcs > $VARBCOUT/${file_out}
#        fi
#      done  # end of list_sensor
#    done  # end of list_sat
  done  # end of list_nl
done # end of scan

exit 0
