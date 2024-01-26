#!/bin/bash
#SBATCH --cpus-per-task=8
#SBATCH --error=festat.log
#SBATCH --job-name=FESTAT
#SBATCH --nodes=37
#SBATCH --output=festat.log
#SBATCH --qos=np
#SBATCH --ntasks-per-node=27
#SBATCH --time=02:00:00
#
#
export MPPEXEC="srun"
export NPROC=999
export NPROCX=1
export NPROCY=1
export OMP_MAX_ACTIVE_LEVELS=1
export OMP_NUM_THREADS=8
export OMP_STACKSIZE=256M
eojcmd=""
ulimit -S -s unlimited || ulimit -s
ulimit -S -m unlimited || ulimit -m
ulimit -S -d unlimited || ulimit -d
set -e 
##
EXP=SURF_CARRA2_CY46
ENV=${HOME}/hm_home/$EXP
# Build the namelist
cat > fort.4 << EOF
&NAMFESTAT
  NCASES=$NPROC,
  LSTABAL=.true.,
  LANAFBAL=.true., 
  LELAM=.TRUE., 
  CNAME='ICMSHHARM+0', 
  NFRGRP=9,
  NPRINTLEV=0,
  LOZONE=.FALSE.,
  LOZBAL=.FALSE.,
  LUVBAL=.FALSE.,
  OUTBAL='stab.bal'
  OUTCVT='stab.cvt',
  OUTCVU='stab.cv',
/
EOF
source ${ENV}/Env_system
srun FESTAT  || exit
#
trap - 0
exit
