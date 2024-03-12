#!/usr/bin/env bash

# This script will write stats from Jb files in ASCII 
# suitable for plotting ...

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

PROGNAME=`basename $0`

# Define a usage function
usage() {

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - list cv header 

${bold}USAGE${normal}
        ${PROGNAME} -i <cv-file>
                    [ -h ]

${bold}DESCRIPTION${normal}
        Script to start new LBC processing suite

${bold}OPTIONS${normal}

        -i ${unline}cv-file${normal}
           cv file

        -h Help! Print usage information.

USAGE
}

CVFILE=DUMMY
#
# Where am I?
#
this_script_loc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
appdir=$(dirname ${this_script_loc})
bindir=${appdir}/bin
libdir=${appdir}/lib
exedir=${appdir}/libexec
levdir=${appdir}/share/levdef

if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

while getopts i:h option
do
  case $option in
    i)
       CVFILE=$OPTARG
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

if [ ${CVFILE} == "DUMMY" ]; then
  echo "Please define cv-file using -i"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ! -s ${CVFILE} ]; then
  echo "cv-file, ${CVFILE}, not found"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

#
# Create soft links for cv_header_list.x
#
ln ${CVFILE} stabal96.cv

${exedir}/cv_header_list.x

exit 0
