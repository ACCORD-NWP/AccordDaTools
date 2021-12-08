#! /bin/ksh
# 
# Script for plotting most of B matrix diagnostics files produced by Festat and Fediacov
#
# Autors Magnus and Ulf 2021 based on  5-Jan-2019 A. Bucanek 
#
# User settings
#
BASEDIR=/nobackup/smhid18/users/sm_mlind/jbdata_all_oper/jbdiagconv_hirlam/fediacov/Bmatrix_cy43/clean/
DATADIR=/nobackup/smhid18/users/sm_mlind/jbdata_all_oper/jbdiagconv_hirlam/fediacov/Bmatrix_cy43/clean/data/
########################################################################################

SCRDIR=${BASEDIR}/scr
TMPDIR=${BASEDIR}/tmp
PLOTDIR=${BASEDIR}/plots

mkdir -p ${SCRDIR}
mkdir -p ${TMPDIR} 
mkdir -p ${PLOTDIR}

cp ${SCRDIR}/prep.ksh ${TMPDIR}/.
cp ${SCRDIR}/plotxy.py ${TMPDIR}/.
cd ${TMPDIR}

#--- function to plot *.xy files by gnuplot ---
runxy () {
  set -e
  
  files=$1                 # files to plot with full path
  scale=$2                 # scaling of data
  spacing=$3               # contour spacing
  order=$4                 # data ordering in the diagnostics (1|-1); 1 increasing order, -1 decreasing order;
  xdir=$5                  # x axis orientation -- optional
  ydir=$6                  # y axis orientation -- optional
  xdir=${xdir:=-1}
  ydir=${ydir:=-1} 
  
  for i in $files
  do
     echo "preparing $i for plotting"
     echo $PWD
     ./prep.ksh $i $scale $order
  
     python plotxy.py
     mv data.png ${PLOTDIR}/${files}.png
  
     # cleaning
#     \rm -f data.dat namefile
  done
}

#--- plot *.y files ---
#runy "07 15 23 31 39 47 55 63"

#--- plot *.xy files ---
# runxy files scale spacing order [x_ori] [y_ori] (order must be the same as in festat)
for var in q_pb q_divu tps_pb tps_divu q_tpsu 
do
FILE="${DATADIR}/expl${var}.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/expl${var}.xy expl${var}.xy
runxy expl${var}.xy 1 0.05 1 1 -1
rm expl${var}.xy
fi 
done

#---------------------
for var in tp d v t q du tu ps
do
FILE="${DATADIR}/expl${var}.xy"
if [[ -e "$FILE" ]]
then
runxy cor${var}.xy  1 0.1 -1
rm expl${var}.xy
fi
done

FILE="${DATADIR}/corqu.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/corqu.xy corqu.xy
runxy corqu.xy 1 0.5 -1
rm corqu.xy
fi

#- These covariances comes from festat
FILE="${DATADIR}/covdp.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/covdp.xy covdp.xy
runxy covdp.xy 100000 0.25 -1
rm covdp.xy
fi
FILE="${DATADIR}/covtd.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/covtd.xy covtd.xy
runxy covtd.xy 1000000 0.25 -1
rm covtd.xy 
fi
FILE="${DATADIR}/covqp.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/covqp.xy covqp.xy
runxy covqp.xy 1000 0.1 -1
rm covqp.xy
fi
FILE="${DATADIR}/covqd.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/covqd.xy covqd.xy
runxy covqd.xy 1000 0.1 -1
rm covqd.xy
fi
FILE="${DATADIR}/covqt.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/covqt.xy covqt.xy
runxy covqt.xy 1000 0.1 -1
rm covqt.xy
fi

#----------------------------------
#--- plot standard deviation profiles per wavenumer ---

for var in v d du q qu
do
FILE="${DATADIR}/stdev${var}.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/stdev${var}.xy stdev${var}.xy
runxy stdev${var}.xy 1 2.0 1 1
rm stdev${var}.xy
fi
done
FILE="${DATADIR}/stdevvtot.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/stdevvtot.xy stdevvtot.xy
runxy stdevvtot.xy 1000000 2.0 -1 1
rm stdevvtot.xy
fi
FILE="${DATADIR}/stdevtu.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/stdevtu.xy stdevtu.xy
runxy stdevtu.xy 1 0.05 1 1
rm stdevtu.xy
fi

#---------------------------------------------
for var in logv logd logdu logq logqu
do
FILE="${DATADIR}/stdev${var}.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/stdev${var}.xy stdev${var}.xy
runxy stdev${var}.xy 1 2.0 1 1
rm stdev${var}.xy
fi
done

for var in logt logtu
do
FILE="${DATADIR}/stdev${var}.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/stdev${var}.xy stdev${var}.xy
runxy stdev${var}.xy 1 0.05 1 1
rm stdev${var}.xy
fi
done

FILE="${DATADIR}/stdevlogvtot.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/stdevlogvtot.xy stdevlogvtot.xy
runxy stdevlogvtot.xy 1000000 2.0 1 1
rm stdevlogvtot.xy
fi

#--- plot cors per level (lot of files) ---
for var in vtot v d du q qu t tu logvtot logv logd logdu logq logqu logt logtu 
do
for lev in 05 11 17 23 29 35 41 47 53 59 65
do
FILE="${DATADIR}/cor${var}${lev}.xy"
if [[ -e "$FILE" ]]
then
ln -sf ${DATADIR}/cor${var}${lev}.xy cor${var}${lev}.xy
runxy cor${var}${lev}.xy 1 0.1 1 1
rm cor${var}${lev}.xy
fi
done
done

# clean up
rm data.dat namefile

