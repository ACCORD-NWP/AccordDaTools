#!/bin/bash

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

usage() {

PROGNAME=`basename $0`

cat << EOFUSAGE

${bold}NAME${normal}
        ${PROGNAME} - Run FESTAT to ...

${bold}USAGE${normal}
        ${PROGNAME} -m <host-name> -x <nprocx> -y <nprocy>
                    [ -t <threads> ]
                    [ -b <binary-directory> ]
                    [ -i <input-directory> ] [ -o <output-directory> ]
                    [ -h ]

${bold}DESCRIPTION${normal}
        Script to run Forecast component of the AHNS. This script can create
        batch submission headers and run a sample forecast.


${bold}OPTIONS${normal}
        -m ${unline}host name${normal}
            The name of your platform used in logic contained in this script.
            [ECMWF|LOCAL]
        -x ${unline}nprocx${normal}
            Number of processors in x-direction for 2D domain decomposition
        
        -y ${unline}nprocy${normal}
            Number of processors in y-direction for 2D domain decomposition
        
        -t ${unline}threads${normal}
            Number of OpenMP threads [ default : 1 ]

        -b ${unline}binary-directory${normal}
            PATH to FESTAT binary

        -i ${unline}input-directory${normal}

        -o ${unline}output-directory${normal}

EOFUSAGE

}

set -e

NPROCX=-1
NPROCY=-1
THREADS=1
HOST=ECMWF                  # Host ECMWF|LOCAL
BINDIR=$HOME/install/bin    # Where to install libraries/executables
INPDIR=$HOME/festat_inputs  # Where to install libraries/executables
OUTDIR=$HOME/festat_output  # Where to install libraries/executables
UNIQTAG=$(mktemp -u XXXXXXXX)

USAGE=0

while getopts m:x:y:t:b:i:o:h option
do
  case $option in
    m)
       HOST=$OPTARG
       ;;
    x)
       NPROCX=$OPTARG
       ;;
    y)
       NPROCY=$OPTARG
       ;;
    t)
       THREADS=$OPTARG
       ;;
    b)
       BINDIR=$OPTARG
       ;;
    i)
       INPDIR=$OPTARG
       ;;
    o)
       OUTDIR=$OPTARG
       ;;
    h)
       USAGE=1
       ;;
    *)
       USAGE=1
       ;;
  esac
done

if [ ${USAGE} -eq 1 -o "$#" -eq 0 ]; then
  usage
  exit 1
fi

if [ ${NPROCX} -lt 1 -o ${NPROCY} -lt 1 ]; then
  echo "Invalid NPROC settings: ${NPROCX},  ${NPROCY}"
  exit 1
fi
#
# Calculate NPROC and NPROCXY
#
NPROCXY=`expr ${NPROCX} \* ${NPROCY}`
NPROC=${NPROCXY}

WRK=${OUTDIR}
echo " ... output-directory: ${WRK}"

WDIR=wrkFestat_${HOST}_${NPROCX}_${NPROCY}_${THREADS}
d_FESTAT=${WRK}/${WDIR}
echo " ... run-directory   : ${d_FESTAT}"
mkdir -p ${d_FESTAT}
cd ${d_FESTAT}

if [ ${HOST} == "ECMWF" ] ; then

  cat << EOFBATCH > batch.tmp
#SBATCH --cpus-per-task=8
#SBATCH --error=festat.log
#SBATCH --job-name=FESTAT
#SBATCH --nodes=37
#SBATCH --output=festat.log
#SBATCH --qos=np
#SBATCH --ntasks-per-node=27
#SBATCH --time=02:00:00
#

EXP=SURF_CARRA2_CY46
ENV=${HOME}/hm_home/$EXP
source ${ENV}/Env_system

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
EOFBATCH

  SUBMIT=sbatch

elif [ ${HOST} == "LOCAL" ] ; then
  cat << EOFBATCH > batch.tmp
#-- host specific settings added by $0
##
## INSERT batch directives here
##

ulimit -S -s unlimited || ulimit -s
ulimit -S -m unlimited || ulimit -m
ulimit -S -d unlimited || ulimit -d

#METIE
export DR_HOOK=1
export DR_HOOK_IGNORE_SIGNALS=8
export DR_HOOK_SILENT=1                # 0|1
export DR_HOOK_OPT=prof                # calls,cputime,walltime,times,heap,stack,rss
                                       # paging,memory,all,prof,cpuprof,hpmprof,trim,self
export DR_HOOK_SHOW_PROCESS_OPTIONS=0
export DR_HOOK_PROFILE=drhook.prof.%d

export MPPEXEC="mpirun -np ${NPROC}"
#-- END host specific settings added by $0

EOFBATCH

  SUBMIT=""

else

  echo "ERROR: host not supported ... exiting"
  exit 1

fi

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

echo " ... add system specific settings and required environment variables to a run script"
echo "#!/bin/bash"  > festat_${PARTITION}_${NPROCX}_${NPROCY}_${THREADS}.sh
cat batch.tmp      >> festat_${PARTITION}_${NPROCX}_${NPROCY}_${THREADS}.sh
echo "${MPPEXEC} ${BINDIR}/FESTAT || exit " >> festat_${PARTITION}_${NPROCX}_${NPROCY}_${THREADS}.sh
chmod 755 festat_${PARTITION}_${NPROCX}_${NPROCY}_${THREADS}.sh

echo " ... submit job with ... "
echo " ...                     ${SUBMIT} ./festat_${PARTITION}_${NPROCX}_${NPROCY}_${THREADS}.sh"
echo " "

${SUBMIT} ./festat_${PARTITION}_${NPROCX}_${NPROCY}_${THREADS}.sh
#
trap - 0
exit
