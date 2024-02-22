#!/bin/bash

#set -xe
#set -e

#Original::

#
# Usage: ./clddet_plotter.sh J
#   where J is the index of observation to be plotted.
#
#   Note that J=0 corresponds to mean (average) of all
#   observations.
#

PROGNAME=`basename $0`

# Define a usage function
usage() {

bold=$(tput bold)
normal=$(tput sgr0)
unline=$(tput smul)

cat << USAGE

${bold}NAME${normal}
        ${PROGNAME} - Infrared radiance cloud detection diagnostic tool

        ${PROGNAME} -j <index of profile> [ -h ]

${bold}DESCRIPTION${normal}
        Script to plot index profile from clddet_sorted_smoothed.dat

${bold}OPTIONS${normal}

        -j ${unline}index profile${normal}
           j=0 corresponds to mean (average) of all profiles

        -h Help! Print usage information.

USAGE
}


if [ ${#} -eq 0 ]; then
  echo "No command line arguments provided"
  echo "Try '${PROGNAME} -h' for more information"
fi

#Defaults
iobs=0

while getopts j:o:h option
do
  case $option in
    j)
       iobs=$OPTARG
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

echo "index profile ${iobs}"

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

infile="clddet_sorted_smoothed.dat"

# Identify working directories:

# 1) Directory where the script is run from
pwd > pwd.txt
read workdir < pwd.txt
rm -f pwd.txt

# 2) Directory with Python script:
srcdir=${workdir}/src


# Retrieve raw and smoothed O-B departures in vertically-ranked
# channel space:

grep ' '${iobs}' .$' ${infile} > temporary
cut -c 27-31 temporary > ranked_indices.dat
cut -c 32-46 temporary > raw_departures.dat
cut -c 62-76 temporary > smoothed_departures.dat

lon=`cut -c 5-21 temporary|sort -u | awk '{print $1}' `
lat=`cut -c 5-21 temporary|sort -u | awk '{print $2}' `
echo "Lon = " $lon " Lat = " $lat

# Retrieve the divider that separates clear and cloudy channels from
# each other in the vertically-ranked channel space:

nclear="$(grep ' 0$' temporary |wc -l)"
cat > cloud_x.dat <<EOF
${nclear}.5
${nclear}.5
EOF

cat > cloud_y.dat <<EOF
-200.0
200.0
EOF


# Retrieve threshold information (corresponds to R__BT_Threshold as
# set in cloud_detect_setup.F90; may be overridden in
# nam/harmonie_namelists.pm):

grep '^bt ' ${infile} |cut -c 4-100 > temporary
cut -c 1-10 temporary|head -2 > threshold_x.dat
cut -c 11-20 temporary|head -2 > threshold_lower.dat
cut -c 11-20 temporary|tail -2 > threshold_upper.dat

#module load python3
python3 ${srcdir}/clddet.py

rm -f temporary ranked_indices.dat raw_departures.dat smoothed_departures.dat
rm -f cloud_x.dat cloud_y.dat
rm -f threshold_x.dat threshold_upper.dat threshold_lower.dat



