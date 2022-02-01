#1prepare_obstool.sh
#Once we have ascii files with odbsql we start with thi script to prepare the data.
 

#!/bin/bash
module load R

# Definitions

#Charge .profile
#. ./profile_obstool.h

#or you can have data here:
#---------------------
# Experiment Setting
#---------------------
#lEXP="AIBr_fr"
lEXP="AIBr_fr2"
cEXP="oper"
#Be careful: also write exp name in Rscrits/preparing_gnss.R and monitoring_gnss.R
#---------------------
# Setting Date
#---------------------
sdate=2019021100
edate=2019022821
#sdate=2019031712
#edate=2019032606
cycle=3
#Observation
OBS=gnss_ztd

#........................

mandtg=/home/ms/es/mdy/bin/mandtg.pl

echo ${lEXP} 

#READ=".\/Files\/"
DIRW=/AccordDaTools/src

#For gnss ztd, radar, radar_dow  and also for ascat: 
exe=preparing_conv.R
#For other satellite data: 
#exe=preparing_sat.R

echo $sdate
echo $edate
echo $cycle
date=$sdate

#==========================================================
# 		PREPARE STATISTICS
#==========================================================
cd ${DIRW}

  while [[ $date -le $edate ]]
  do

   echo "preparing $date"


   for iEXP in ${lEXP}
   do 
    sed \
		-e "s/exp=\"\"/exp=\"${iEXP}\"/"     \
        -e "s/XXYYMMDDNTXX/${date}/"         \
        $exe > run1.${iEXP}.x

# Start visualization in R
		R CMD BATCH run1.${iEXP}.x
   done

   # increment by cycle h
   #date=`expr ${date} + ${cycle}`
   date=`$mandtg ${date} + ${cycle}`
  done

  echo 'Result at /../$OBS/'${lEXP}
  echo 'Output at: ~/../src/run1.${lEXP}.x.Rout'


# Cleaning
			rm -f run*.x  

