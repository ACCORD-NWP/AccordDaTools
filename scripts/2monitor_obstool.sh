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


echo ${lEXP} 

#READ=".\/Files\/"
DIRW=/AccordDaTools/src

#For gnss ztd, radar, radar_dow etc : 
exe=monitoring_conv.R
#For  ascat: 
exe=monitoring_ascat.R
#For other satellite data: 
#exe=monitoring_sat.R

echo $sdate
echo $edate
date=$sdate


#==========================================================
# 		PLOT STATISTICS
#==========================================================

cd ${DIRW}

		llEXP=`echo ${lEXP} | sed "s/ /\",\"/g"`
		ccEXP=`echo ${cEXP} | sed "s/ /\",\"/g"`
        echo $llEXP
        echo $ccEXP

		sed \
			-e "s/XXEXPXX/${llEXP}/"     \
			-e "s/XXCEXPXX/${ccEXP}/"     \
			-e "s/sdate=\"\"/sdate=\"${sdate}\"/"     \
			-e "s/edate=\"\"/edate=\"${edate}\"/"     \
			-e "s/cyc=\"\"/cyc=\"${cycle}\"/"  \
			 $exe > run2.x

# Start visualization in R
			R CMD BATCH run2.x
          echo 'Result at ~/../$OBS'
          echo 'Output at ~/../src/run2.x.Rout'

# Cleaning
			rm -f run*.x  

