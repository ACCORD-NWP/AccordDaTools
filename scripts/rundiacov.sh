#!/usr/bin/env bash

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

PROGNAME=`basename $0`

# Define a usage function
usage() {

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - generates files by running DIACOV program

${bold}USAGE${normal}
        ${PROGNAME} -i <data-dir>
		    -c <jb-file-name>
		    -g <grid-spacing>
		    -d <diacov-binary>
                    [ -h ]

${bold}DESCRIPTION${normal}
        Generates .xy (and .y) files from stabal.cvt and stabal.bal files (generated by FESTAT) by running DIACOV program

${bold}OPTIONS${normal}
	-i ${unline}data-dir${normal}
	   Input data directory (full path)

        -c ${unline}jb-file-name${normal}
	   Name of *.cv and *.cvt files (without extension)
		   
	-g ${unline}grid-spacing${normal}
           Grid-spacing in kilometers
		   
	-d ${unline}diacov-binary${normal}
           PATH to DIACOV binary.

        -h Help! Print usage information.

USAGE
}

DATADIR=DUMMY
JBFILE=DUMMY
GRIDSIZE=DUMMY
BINPATH=DUMMY

#wrkdir=/home/eela/EoinDiacovTest/ #need to be changed to be more flexible
this_script_loc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
appdir=$(dirname ${this_script_loc})
tmpdir=tmp
mkdir -p ${tmpdir}

if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

while getopts i:c:g:d:h option
do
  case $option in
    i)
       DATADIR=$OPTARG
       ;;
    c)
       JBFILE=$OPTARG
       ;;
    g)
       GRIDSIZE=$OPTARG
       ;;
    d)
       BINPATH=$OPTARG
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

if [ ${DATADIR} == "DUMMY" ]; then
  echo "Please define input data directory using -i"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ${JBFILE} == "DUMMY" ]; then
  echo "Please define jb-file-name using -c"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ${GRIDSIZE} == "DUMMY" ]; then
  echo "Please define grid-size using -g"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ${BINPATH} == "DUMMY" ]; then
  echo "Please define path to DIACOV using -d"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

cd $tmpdir

LEVELNUM=$(cv_header_list -i ../${DATADIR}/${JBFILE}.cv | grep "NFLEVG" | awk '{print $3}')
NSMAX=$(cv_header_list -i ../${DATADIR}/${JBFILE}.cv | grep "NSMAX" | awk '{print $3}')
NDGL=$(cv_header_list -i ../${DATADIR}/${JBFILE}.cv | grep "NDGL" | awk '{print $3}')

# prepare namelist for diacov - full variables
cat > nam_diag1 <<EOF
&NAMDIACOV
  NFLEV=${LEVELNUM},
  NSMAX=${NSMAX},
  NDGL=${NDGL},
  PPDELTAX=${GRIDSIZE},
  CJBTYPE='NONSEP93',
/
EOF

# prepare namelist for diacov - unbal variables
cat > nam_diag2 <<EOF
&NAMDIACOV
  NFLEV=${LEVELNUM},
  NSMAX=${NSMAX},
  NDGL=${NDGL},
  PPDELTAX=${GRIDSIZE},
  CJBTYPE='STABAL96',
/
EOF

#Run DIACOV with stabal.cv
ln -sf ../${DATADIR}/${JBFILE}.cvt stabal.cvt
cp nam_diag1 fort.4
${BINPATH}

#Run DIACOV with stabal.cvt
ln -sf ../${DATADIR}/${JBFILE}.cv stabal.cv
cp nam_diag2 fort.4
${BINPATH}

# Take care of results
mv *.xy ../${DATADIR}/.
mv *.y ../${DATADIR}/.


# Clean up
rm nam_diag2 nam_diag1 fort.4 stabal.cvt stabal.cv

echo "Successful. Output in ${DATADIR}"
exit 0
