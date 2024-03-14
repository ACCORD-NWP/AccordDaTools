#!/usr/bin/env bash

# This script will write ODB dara to ASCII format

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

PROGNAME=`basename $0`

# Define a usage function
usage() {

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - write ODB data to ASCII files

${bold}USAGE${normal}
        ${PROGNAME} -d <input-dir>
		    -c <control-name>
		    -t <test-name>
                    [ -h ]

${bold}DESCRIPTION${normal}
        Untar the preliminarily checked out CCMA ODB files 
	and write the information to ASCII format. The ODB
	files should be named as follows:
	<NAME>_<YYYYMMDDHH>_odb_ccma.tar
	where NAME is strictly 4-digit-long.

${bold}OPTIONS${normal}
        -d ${unline}input-dir${normal}
           Input directory containing CCMA ODB tar files
	   in the format <NAME>_<YYYYMMDDHH>_odb_ccma.tar

        -c ${unline}control-name${normal}
           The short 4-digit name for control run
	   experiment data (e.g. cont)

        -t ${unline}test-name${normal}
           The short 4-digit name for test run
	   experiment data (e.g. test)

        -h Help! Print usage information.

USAGE
}


INPDIR=DUMMY
CNAME=DUMMY
TNAME=DUMMY

this_script_loc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
appdir=$(dirname ${this_script_loc})
bindir=${appdir}/bin
libdir=${appdir}/lib
exedir=${appdir}/libexec

wdir=$( pwd )
tmpdir=${wdir}/temp

if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

while getopts d:c:t:h option
do
  case $option in
    d)
      INPDIR=$OPTARG
      ;;
    c)
      CNAME=$OPTARG
      ;;
    t)
      TNAME=$OPTARG
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

if [ ${INPDIR} == "DUMMY" ]; then
  echo "Please define input data directory using -d"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi
if [ ${CNAME} == "DUMMY" ]; then
  echo "Please define control exp 4-digit short name  using -c"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi
if [ ${TNAME} == "DUMMY" ]; then
  echo "Please define test exp 4-digit short name  using -t"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi


mkdir -p ${wdir}/depstat_tmp_${CNAME}
mkdir -p ${wdir}/depstat_tmp_${TNAME}
mkdir -p ${tmpdir}

cd $INPDIR
dtgs=$( ls ${CNAME}*.tar | cut -c 6-15 )
cd ${tmpdir}

for expname in $CNAME $TNAME
do
  for dtg in $dtgs
  do
    tar -xf ${INPDIR}/${expname}_${dtg}_odb_ccma.tar -C . 
    cd odb_ccma/CCMA
    j=0
    for i in conv radar nonconv
    do
      j=`expr ${j} + 1`
      rm -f ${tmpdir}/extract_${i}.dat

      if [ ${j} -eq 1 ] ; then
        odbsql -q "select reportype,-1,-2,obstype,varno,vertco_reference_1,lon,lat,datum_status,obsvalue,fg_depar,an_depar,biascorr_fg,time from hdr,body" > ${tmpdir}/extract_${i}.dat

      elif [ ${j} -eq 2 ] ; then
        odbsql -q "select -3,-1,-2,obstype,varno,vertco_reference_1,lon,lat,datum_status,obsvalue,fg_depar,an_depar,biascorr_fg,time from hdr,body" > ${tmpdir}/extract_${i}.dat
      
      else
	odbsql -q "select reportype,satellite_identifier,satellite_instrument,obstype,varno,vertco_reference_1,lon,lat,datum_status,obsvalue,fg_depar,an_depar,biascorr_fg,time from hdr,body,sat where satellite_instrument is not null" > ${tmpdir}/extract_${i}a.dat
      
        odbsql -q "select reportype,satellite_identifier,sensor,obstype,varno,vertco_reference_1,lon,lat,datum_status,obsvalue,fg_depar,an_depar,biascorr_fg,time from hdr,body,sat where satellite_instrument is null" > ${wdir}/temp/extract_${i}b.dat

        cat ${tmpdir}/extract_${i}a.dat ${tmpdir}/extract_${i}b.dat > ${tmpdir}/extract_${i}.dat
      fi

      ls -l ${tmpdir}/extract_${i}.dat

    done

    cd ${tmpdir}
    rm -f obstat.log obstat.out

    ${exedir}/obstat.x

    cat obstat.log obstat.out > ${expname}_${dtg}.txt
    mv ${expname}_${dtg}.txt ${wdir}/depstat_tmp_${expname}/.
  
  done
done

module unload odb_api
rm -R $tmpdir
exit
