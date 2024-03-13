# VarBC coefficient diagnostic tool


## 1. Purpose

This diagnostic tool is intended for use in the evaluation of the performance of the variational bias correction (VarBC) used for satellite observations in the context of HARMONIE-AROME data assimilation systems. In particular for evaluating the performance in time the VarBC coefficients used to derive the bias to be corrected (Dee, 2004).


## 2. Content

The tool comprises two shell scripts:

- **varbcdiagnosis.sh** for data extraction

- **plotvarbccoeff.sh** for VarBC coefficient time-series plotting (actually this script needs the python script **plotvarbccoeff.py** but in the future it is intented to use only the python one)


## 3. Usage

**varbcdiagnosis** extracts VarBC coefficients for certain satellite/instrument all all the channels with data and for 
a certain period of time. VarBC coefficients are extracted from VARBC.cycle files and stored as ASCII files.

Script **varbcdiagnosis** arguments:

        -i input-directory (where VARBC.cycle files are stored, usually in the form ./EXP/YYYY/MM/DD/HH
           EXPERIMENT, Year, Month, Day and Hour

        -o output-directory
           
        -S list-sat
           colon separted list of satellites to process. For example, -S 3:4:5 to process Metop-A, Metop-B and Metop-C. 
           See https://apps.ecmwf.int/odbgov/satelliteidentifier/ for more etails.

        -s list-sen
           colon separted list of sensors to process. For example, -s 3:15 to process AMSU-A and MHS. 
           See https://apps.ecmwf.int/odbgov/sensor/ for more details.

        -h Help! Print usage information.


**plotvarbccoeff** produces a plot for a selected satellite, sensor, channel and hour (optional, if not present all the assimilation cycles with observations are included in the ASCII file). 

Script **plotvarbccoef** arguments:

       -h, --help  show this help message and exit
       -i IPATH    Input file name
       -l LLOC     Legend location using matplotlib syntax
       -d LABELS   Optional experiment description
       -b          Batch mode, produce png only

For example for METOP-B (3), MHS (15), Channel 9 at 21 UTC or at all assimilations cycles with data, IPATH are  
output-directory/VARBC_3_15_9_210000 or output-directory/VARBC_3_15_9.


## 4. Interpretation

Time-series of VarBC coefficients can be used for monitoring the performance of variational bias correction, specially at the initial steps of the bias correction, when usually inictial bias corrected is zero (cold-start) and satellite observations are assimilated in passive. Coefficents usually stabilize in time (it depends in many factors), and when they are stable it is assumed that satellite observations can be active assimilated. However, it is recommended to monitor mand and standard deviation of (ob-fg), for instance by obsmon. 


