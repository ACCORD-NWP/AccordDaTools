#!/usr/bin/env bash

# This program produces the statistics for plotting

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

PROGNAME=`basename $0`

# Define a usage function
usage() {

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - generates departure statistics for plotting

${bold}USAGE${normal}
        ${PROGNAME} -c <control-name>
                    -t <test-name>
		    -f <first-dtg>
		    -l <last-dtg>
                    [ -h ]
${bold}DESCRIPTION${normal}
        This program produces the ASCII files
	containing departure statistics suitable
	for plotting.

${bold}OPTIONS${normal}

        -c ${unline}control-name${normal}
           The short 4-digit name for control run
           (e.g. cont)

        -t ${unline}test-name${normal}
           The short 4-digit name for test run
           (e.g. test)

        -f ${unline}first-dtg${normal}
           The first DTG <YYYYMMDDHH>

        -l ${unline}last-dtg${normal}
           The last DTG <YYYYMMDDHH>

        -h Help! Print usage information.

USAGE
}

CNAME=DUMMY
TNAME=DUMMY
FDTG=DUMMY
LDTG=DUMMY

this_script_loc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
appdir=$(dirname ${this_script_loc})
exedir=${appdir}/libexec
bindir=${appdir}/bin
libdir=${appdir}/lib

wdir=$( pwd )

if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

while getopts c:t:f:l:h option
do
  case $option in
    c)
      CNAME=$OPTARG
      ;;
    t)
      TNAME=$OPTARG
      ;;
    f)
      FDTG=$OPTARG
      ;;
    l)
      LDTG=$OPTARG
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

whoami > uid.txt

g="large"   # hyperspectral long-wave channels displayed also in channel index space
p="0"       # Treat AIRS and CrIS separately from each other

if [ "${g}" = "large" ]; then
  o=1
  lwwn=1
else
  o=0
  lwwn=0
fi


#Produce statistics

rank=1
${exedir}/depstat_main ${TNAME} ${CNAME} ${FDTG} ${LDTG} ${g} ${rank} ${p}


#Create directory for output

fname="number_of_days.txt"
if [ -s ${fname} ]; then
  read n < ${fname}
else
  n=0
fi
rm -f ${fname}

if [ ${n} -lt 0 ]; then
  exit
fi


indir=${wdir}/${TNAME}_${CNAME}_${n}days/data/
outdir=${wdir}/${TNAME}_${CNAME}_${n}days/figs/
mkdir -p $indir
mkdir -p $outdir

mv ${TNAME}_${CNAME}_*_om?_* $indir
mv ${TNAME}_*_om?_* $indir
mv ${CNAME}_*_om?_* $indir
mv ${TNAME}_*_bcor_* $indir
mv ${CNAME}_*_bcor_* $indir
mv ${TNAME}_${CNAME}_datacount_summary $indir

#--------------- Run python plot ------------------------------------

python3 plotdepstat.py ${CNAME} ${TNAME} ${indir} ${outdir}

exit
