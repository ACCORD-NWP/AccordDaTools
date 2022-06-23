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
        ${PROGNAME} - write Jb statistics

${bold}USAGE${normal}
        ${PROGNAME} -p <pert-file> -u <unpert-file>
                [-o <output-file>] [ -h ]

${bold}DESCRIPTION${normal}
        DFS computation

${bold}OPTIONS${normal}
        -p ${unline}pert-file${normal}
           perturbed data file

        -u ${unline}unpert-file${normal}
           unperturbed data file

        -o ${unline}dfs-output${normal}
           output file with DFS values

        -L List available level definitions
        
        -h Help! Print usage information.

USAGE
}

CNTFILE=DUMMY
UPRFILE=DUMMY
OUTFILE=DUMMY
PRINTLEV=1
#
# Where am I?
#
this_script_loc="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
appdir=$(dirname ${this_script_loc})
bindir=${appdir}/bin
libdir=${appdir}/lib
exedir=${appdir}/libexec

if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

while getopts p:u:o:h option
do
  case $option in
    p)
       CNTFILE=$OPTARG
       ;;
    u)
       UPRFILE=$OPTARG
       ;;
    o)
       OUTFILE=$OPTARG
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

if [ ${CNTFILE} == "DUMMY" ]; then
  echo "Please define bal-file using -b"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

if [ ${UPRFILE} == "DUMMY" ]; then
  echo "Please define cv-file using -c"
  echo "Try '${PROGNAME} -h' for more information"
  exit 1
fi

#
# Create soft links for dfscomp.x
#
ln -s ${CNTFILE} file1
ln -s ${UPRFILE} file2

${exedir}/dfscomp.x file1 file2

rm -f file1
rm -f file2

if [ ${OUTFILE} != "DUMMY" ]; then
  echo
  echo "DFS output in ${OUTFILE}"
  mv dfs.dat ${OUTFILE} 
else
  echo
  echo "DFS output in dfs.dat"  
fi
exit 0
