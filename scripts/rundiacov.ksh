#! /bin/ksh
# 
# Script for generating .xy (and .y) files from stabal.cvt and stabal.bal files by running DIACOV program
#
# Autor Magnus Lindskog 2021 based on  5-Jan-2019 A. Bucanek 
#
# User settings
#
BASEDIR=/nobackup/smhid18/users/sm_mlind/jbdata_all_oper/jbdiagconv_hirlam/fediacov/Bmatrix_cy43/clean/
DATADIR=/nobackup/smhid18/users/sm_mlind/jbdata_all_oper/jbdiagconv_hirlam/fediacov/Bmatrix_cy43/clean/data/
EXE=/nobackup/smhid18/users/sm_mlind/hm_home/cy43mc_modarl/bin/R64/DIACOV
CVFILE=${DATADIR}/METCOOP25D_65_2021100300-2021111006_412.cv
CVTFILE=${DATADIR}/METCOOP25D_65_2021100300-2021111006_412.cvt
NFLEV=65
NSMAX=539
NDGL=1080
PPDELTAX=2.5
# Get values above from festat log file for example (these values are for metcoopd)
########################################################################################

SCRDIR=${BASEDIR}/scr
TMPDIR=${BASEDIR}/tmp
PLOTDIR=${BASEDIR}/plots

mkdir -p ${SCRDIR}
mkdir -p ${TMPDIR} 
mkdir -p ${PLOTDIR}

cd ${TMPDIR}
# prepare namelist for diacov - full variables
cat > nam_diag1 <<EOF
&NAMDIACOV
  NFLEV=$NFLEV,
  NSMAX=$NSMAX,
  NDGL=$NDGL,
  PPDELTAX=$DELTAX,
  CJBTYPE='NONSEP93',
/
EOF

# prepare namelist for diacov - unbal variables
cat > nam_diag2 <<EOF
&NAMDIACOV
  NFLEV=$NFLEV,
  NSMAX=$NSMAX,
  NDGL=$NDGL,
  PPDELTAX=$DELTAX,
  CJBTYPE='STABAL96',
/
EOF


#Run DIACOV with stabal.cv
ln -sf $CVTFILE stabal.cvt
cp nam_diag1 fort.4
${EXE}

#Run DIACOV with stabal.cvt
ln -sf $CVFILE stabal.cv
cp nam_diag2 fort.4
${EXE}

# Take care of results
mv *.xy ${DATADIR}/.
mv *.y ${DATADIR}/.


# Clean up
rm nam_diag2 nam_diag1 fort.4 stabal.cvt stabal.cv

exit


