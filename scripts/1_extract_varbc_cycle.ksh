#!/bin/ksh

#Eoin suggests having some more documentation here...

. $PWD/include.paths

# Loop from DTGBEG to DTGEND where for each cycle (DTG) the VARBC.cycle file
# is splitted in several one-line files according to satellite, sensor, channel, 
# the so-called VARBC_${sat}_${sensor}_${channel}_${YYYY}${MM}${DD}_${HH} files
# stored in $DIR_TMP
# 

#DTGBEG=2021060806
#DTGEND=2021063021

# METEOSAT11/SEVIRI
#list_sat="70"
#list_sensor="29"

# METOP-B/MHS
#list_sat="3"
#list_sensor="15"

DTG=$DTGBEG
FREQ=03 

while [ $DTG -le $DTGEND ]
do
echo $DTG

    YYYY=`echo $DTG|cut -c1-4`
    MM=`echo $DTG|cut -c5-6`
    DD=`echo $DTG|cut -c7-8`
    HH=`echo $DTG|cut -c9-10`

    file_in=${DIR_VARBC}/${YYYY}/${MM}/${DD}/${HH}/VARBC.cycle

      if [ -f ${file_in} ] ; then

        cp ${file_in} ${DIR_TMP}
        cd ${DIR_TMP}

        # $2 is the number of observations, so for $2 > 1 only satellite channels are selected 
        # so GNSS stations are excluded
	list_nl=`awk 'BEGIN {FS="="}; $1=="ndata" && $2> 1 {print NR}' VARBC.cycle` 

	for nl in $list_nl
	do

	# ndata -> nl ; line where ndata>1
	ndata=`sed -n -e "${nl}"p VARBC.cycle|cut -c7-`

	# key -> nl-2 ; two lines up
	n1=`expr $nl - 2`; key=`sed -n -e "${n1}"p VARBC.cycle|cut -c5-`

	# npred -> nl+1 ; one line down
	n2=`expr $nl + 1`; npred=`sed -n -e "${n2}"p VARBC.cycle|cut -c7-`

	# predcs -> nl+2 ; two lines down
	n3=`expr $nl + 2`; predcs=`sed -n -e "${n3}"p VARBC.cycle|cut -c8-`

        # param0 -> nl+3 ; three lines down
	n4=`expr $nl + 3`; param0=`sed -n -e "${n4}"p VARBC.cycle|cut -c8-`

        # params -> nl+4 ; four lines down
	n5=`expr $nl + 4`; params=`sed -n -e "${n5}"p VARBC.cycle|cut -c8-`

        # name_key= $sat_$sensor_$channel
          name_key=`echo $key|awk '{print $1"_"$2"_"$3}'`
          sat=`echo $key|awk '{print $1}'`
          sensor=`echo $key|awk '{print $2}'`
          channel=`echo $key|awk '{print $3}'`

        # Only NOAA-18, 19 (209 and 223) and METOP-A,B,C (4,3,5) and METOPSAT-11 are selected
	file_out=VARBC_${name_key}_${YYYY}${MM}${DD}_${HH}
   
        for SAT in $list_sat ; do
        for SENSOR in $list_sensor ; do

        if [[ $sat == $SAT  &&  $sensor == $SENSOR ]] ; then
	   echo $YYYY $MM $DD $HH $key $ndata $npred $params $param0 $predcs > $DIR_TMP/${file_out}
        fi

	done  # end of list_sensor
	done  # end of list_sat
	done  # end of list_nl

    rm VARBC.cycle

    else
        echo ;echo ${file_in} " not found";echo
    fi

DTG=`mandtg $DTG + $FREQ`

done # end of DTGs

