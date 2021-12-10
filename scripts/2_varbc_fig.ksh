#!/bin/ksh

. $PWD/include.paths

#DTGBEG=2021060806
#DTGEND=2021063021

# after runnning 1_extract_VARBC_cycle.ksh files
# VARBC_${sat}_${sensor}_${channel}_${YYYY}${MM}${DD}_${HH} files
# in $DIR_TMP are grouped with selected loops for sat, sensor, 
# channels and cycle (hh) which will be used as the input for the Rscript
# which will plot the time evolution of the coefficients

# SEVIRI: data in all cycles (from 00 to 21), 
# if FCINT = 03 plotting of ALL cycles (from 00 to 21)
# if FCINT != 03 plotting for HH in $list_hh

#list_sat="70"
FCINT=03

# ATOVS: data in cycles in list_hh, FCINT=24
# METOP-B

#list_sat="3" 
FCINT=24

# usually for SEVIRI and ATOVS VarBC is done with 5 predictors
# but less predictors are possible
npred=5


echo "############################################"
echo "Cycling " $FCINT 
echo "VARBC file with " $npred " coefficients"
echo "############################################"



cd $DIR_DAT

for sat  in $list_sat
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

for sensor in $list_sensor
do

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


for channel in $list_channel
do

for hh in $list_hh
do

file_out=${DIR_DAT}/VARBC_${sat}_${sensor}_${channel}_${hh}

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

while [ $DTG -le $DTGEND ]
do

    YYYY=`echo $DTG|cut -c1-4`
    MM=`echo $DTG|cut -c5-6`
    DD=`echo $DTG|cut -c7-8`
    HH=`echo $DTG|cut -c9-10`


    if [ $FCINT == "03" ] ; then
         ff=${DIR_TMP}/VARBC_${sat}_${sensor}_${channel}_${YYYY}${MM}${DD}_${HH}
    else
         ff=${DIR_TMP}/VARBC_${sat}_${sensor}_${channel}_${YYYY}${MM}${DD}_${hh}
    fi


       if [ -s  $ff ]  ; then
          cat  $ff >> ${file_out}
       else
          # if no there is not data in $DTG, a dummy line is included to avoid problems with varbc_plot.Rsh
          # echo $YYYY $MM $DD $HH $key $ndata $npred $params $param0 $predcs 
          nobs=0
          if [ $npred -eq 1 ] ; then
            echo $YYYY $MM $DD $HH $sat $sensor $channel $nobs $npred "NaN NaN 0" >> ${file_out}
          else
            echo $YYYY $MM $DD $HH $sat $sensor $channel $nobs $npred "NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN 0 1 8 9 10" >> ${file_out}
          fi
       fi


DTG=`mandtg $DTG + $FCINT`

done # end do while DTG


echo "Plotting " $EXP $sat $sensor $channel $hh 

${DIR_SCR}/varbc_plot.Rsh $sat $sensor $channel $hh $EXP > /dev/null 2>&1


done # for hh
done # for channel
done # for sensor
done # for sat
