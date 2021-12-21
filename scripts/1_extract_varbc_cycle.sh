#!/bin/bash

#set -xe
#set -e

#Original::
# Loop from DTGBEG to DTGEND where for each cycle (DTG) the VARBC.cycle file
# is splitted in several one-line files according to satellite, sensor, channel, 
# the so-called VARBC_${sat}_${sensor}_${channel}_${YYYY}${MM}${DD}_${HH} files
# stored in $DIR_TMP
# 
#Current::
# Scan directory for VARBC.cycle files and write all information
# to output directory

#TODO:
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
        ${PROGNAME} -i <input-directory> -o <output-directory>
                    [ -h ]

${bold}DESCRIPTION${normal}
        Script to process VarBC predictor information from VARBC.cycle files
        produced by IAAAH NWP System.

${bold}OPTIONS${normal}
        -i ${unline}input-directory${normal}
           input directory

        -o ${unline}output-directory${normal}
           output directory

        -S ${unline}list-sat${normal}
           colon separted list of satellites to process. For example,
           -S 3:4:5 to process Metop-A, Metop-B and Metop-C. See
           https://apps.ecmwf.int/odbgov/satelliteidentifier/ for more
           details.

        -s ${unline}list-sen${normal}
           colon separted list of sensors to process. For example,
           -s 3:15 to process AMSU-A and MHS. See 
           https://apps.ecmwf.int/odbgov/sensor/ for more details.

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
SATLIST=ALL
SENLIST=ALL

while getopts i:o:S:s:h option
do
  case $option in
    i)
       VARBCINP=$OPTARG
       ;;
    o)
       VARBCOUT=$OPTARG
       if [ -d $VARBCOUT ]; then
         echo "Directory ${VARBCOUT} already exists. Please choose another name"
         exit 1
       else
         mkdir -p ${VARBCOUT}
       fi
       ;;
    S)
       SATLIST=$OPTARG
       ;;
    s)
       SENLIST=$OPTARG
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


echo "Starting to loop over files ..."

echo "Processing ..."
find ${VARBCINP} -name "VARBC.cycle" | sort | while read FILEIN; do

  YYYYMMDD=$(awk 'NR == 2 {print $2; exit}' ${FILEIN})
  HHMMSS_S=$(awk 'NR == 2 {print $3; exit}' ${FILEIN})
  HHMMSS=$(printf "%06d\n" ${HHMMSS_S})
  echo -e "Processing ${FILEIN} ${YYYYMMDD} ${HHMMSS}\e[1A"

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

    file_out=VARBC_${name_key}_${YYYYMMDD}_${HHMMSS}
    file_out=VARBC_${name_key}

    if [[ "$SATLIST" == "ALL" ]]; then
      echo ${YYYYMMDD}:${HHMMSS} $key $ndata $npred $params $param0 $predcs >> $VARBCOUT/${file_out}_${HHMMSS}
      echo ${YYYYMMDD}:${HHMMSS} $key $ndata $npred $params $param0 $predcs >> $VARBCOUT/${file_out}
    else
      list_sat=$(echo $SATLIST | sed 's/:/ /g')
      list_sensor=$(echo $SENLIST | sed 's/:/ /g')
      for SAT in $list_sat ; do
        for SENSOR in $list_sensor ; do
          if [[ $sat == $SAT  &&  $sensor == $SENSOR ]] ; then
            echo ${YYYYMMDD}:${HHMMSS} $key $ndata $npred $params $param0 $predcs >> $VARBCOUT/${file_out}_${HHMMSS}
            echo ${YYYYMMDD}:${HHMMSS} $key $ndata $npred $params $param0 $predcs >> $VARBCOUT/${file_out}
          fi
        done  # end of list_sensor
      done  # end of list_sat
    fi
  done  # end of list_nl
done # end of scan
echo
echo "Scan complete ..."
echo "Output available in ${VARBCOUT}"
exit 0
