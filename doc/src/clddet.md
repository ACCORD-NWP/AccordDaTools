# Infrared radiance cloud detection diagnostic tool

This is an adaption for ACCORD DA tools of the IR cloud detection diagnosis tool done by (c) Reima.Eresmaa@fmi.fi 
(25th Novemeber 2022) available on ECMWF in ~fi9/f90/clddet/clddet.tar

Adaptation was made by Joan Campins (jcampinsp@aemet.es on 20th May 2025)


## Interpretation

The cloud detection scheme makes use of the expectation that stratospheric- and upper-tropospheric-sensitive channels are 
more often clear of clouds than lower-tropospheric channels. The x-axis in the output figures consists of channels in 
vertically-ranked space such that the highest-peaking channels are at far left, and lowest-peaking channels at far right. 
If the scheme has identified some cloud contamination in the given sounding, the cloud-contaminated and cloud-free 
channels are distinguished from each other by the vertical dashed red line somewhere in the middle of the figure: to 
the right of this line, all channels are cloud-affected and will consequently not be used during the assimilation. 
Channels on the left-hand-side of the red vertical line are free from the cloud effect and can be assimilated.

Since the cloud detection relies heavily on O-B departure data, it can easily be interfered if bias corrections are not 
up to date for any reason. This will be most easily diagnosed on the stratospheric-peaking channels, that are most of 
the time not affected by cloud, and therefore should on average be very close to zero in (O-B). In a healthy system 
setup, the left-hand part of the smoothed O-B departure curve should be approximately horizontal. Considerable deviation 
from this pattern is likely indicative of a problem of some kind in the system setup.


## Purpose

This diagnostic tool is intended for use in evaluation of the performance of the infrared radiance cloud detection scheme 
in the context of HARMONIE-AROME data assimilation systems. As of February 2022, the most relevant infrared sounders 
include IASI and CrIS. The cloud detection scheme follows broad principles that are outlined in McNally and Watts 
(QJRMS, 2003).


## Content

The tool comprises two scripts:

- A shell script: **clddet_diagnosis.sh**

- A python script: **clddet_plotter.sh**

and another code files (in src):

- **clddet_analyzer.F90** 


## Usage

### Cloud detection diagnosis

The main production script is **clddet_diagnosis.sh**. In the heading of this script, the user will need to specify
4 mandatory and two optional arguments:

1. the input directory (*indir*), which usually is the experiment identification name
2. the output directory (*outdir*), where large files are stored
3. date and time of the analysis that they are diagnosing (*cycle*)
4. name of the infrared sounder that they are working with (*sensor*) 
5. window width for cloud detection smoothing (*i_window_width*). Default = 10
6. brightness temperature threshold (*r_bt_thres*). Default = 0.5 K

It is assumed that the relevant experiment (input directory) exists.

Once these settings are correctly updated, the tool is run by command

**./clddet_diagnosis.sh -i indir -o outdir -c cycle -s sensor**

Help can be obtained with **./clddet_diagnosis.sh -h**

The script needs two input files, called Fetch input files, that are necessary to run clddet_analyzer.x:

- HM_Date log file: **HM_Date_${cycle}.html**: stored in ${indir}, which is a copy of ~./archive/log/HM_Date_${cycle}.html

- ASCII file derived from odb files (ECMA.*): **clddet_ascii.dat_${cycle}**: stored in ${indir}

  - **clddet_ascii.dat_${cycle}** can be obtained from ECMA by means:
  
  - tar -xf odb_stuff.tar odbvar.tar
  - tar -xf odbvar.tar odbvar/${odbname} ; where  odbname="ECMA.${sensor}" (sensor= iasi, cris or airs)
  - cd ./odbvar/${odbname}
  - module load odb odb_api
  - odbsql -q "select satellite_identifier,degrees(lon),degrees(lat),vertco_reference_1,fg_depar,rank_cld,datum_event1.contam_cld_flag from body,sat,hdr,radiance_body" > clddet_ascii.dat_${cycle}


In usual circumstances, running the script will take several minutes. After the script run is completed, a symbolic 
link "clddet_sorted_smoothed.dat" should appear in the working directory. This link will point to a non-zero sized 
ASCII file placed in the output directory. The ASCII file contains data from all observations of the sounder of 
interest at the appropriate date and time, as far as these data are correctly stored in clddet_ascii.dat.


### Plotting

Graphical plotting of results can then be done with the command

**python3 ./clddet_plotter.py -j ARG**

where ARG is an integer number that represents a running index of an individual infrared satellite sounding. 
By default, ARG=0, which means that the output figure will by representative of average spectrum computed from 
the input data. The plotting script will save the produced figure in file clddet.png.


