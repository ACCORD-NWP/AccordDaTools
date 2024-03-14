# VarBC coefficient diagnostic tool


## 1. Purpose

This diagnostic tool is intended for use in the evaluation of the performance of the variational bias correction (VarBC) used for satellite observations in the context of HARMONIE-AROME data assimilation systems. In particular for evaluating the performance in time of the VarBC coefficients.

There are tool two tools required:
- **varbcdiagnosis** for data extraction
- **plotvarbccoeff** for VarBC coefficient time-series plotting 

## 2. Tools

### VARBC.cycle data extraction

**varbcdiagnose** extracts VarBC coefficients for a certain satellite/instrument and all the channels with data and for a certain period of time. VarBC coefficients are extracted from VARBC.cycle files.

Help/usage:
```shell
$~> varbcdiagnose -h

        -i input-directory 
           where VARBC.cycle files are stored, usually in the form ./EXP/YYYY/MM/DD/HH (EXPERIMENT, Year, Month, Day and                Hour)

        -o output-directory
           
        -S list-sat
           colon separted list of satellites to process. For example, -S 3:4:5 to process Metop-A, Metop-B and Metop-C. 
           See https://apps.ecmwf.int/odbgov/satelliteidentifier/ for more etails.

        -s list-sen
           colon separted list of sensors to process. For example, -s 3:15 to process AMSU-A and MHS. 
           See https://apps.ecmwf.int/odbgov/sensor/ for more details.

        -h Help! Print usage information.
```

Example:
```shell
$~> varbcdiagnose -i /path/to/directory/with/varbc_files/ -o varbc_diag_out
```

### VarBC coefficient time-series plotting

**plotvarbccoeff** produces a plot for a selected satellite, sensor, channel and hour (optional, if not present all the assimilation cycles with observations are included). 

Help/usage:
```
$~> plotvarbccoeff -h
       -h, --help  show this help message and exit
       -i IPATH    Input file name
       -l LLOC     Legend location using matplotlib syntax
       -d LABELS   Optional experiment description
       -b          Batch mode, produce png only
```

Example: For METOP-B (3), MHS (15), Channel 9 at 21 UTC (or at all assimilation cycles with data), IPATH is output-directory/VARBC_3_15_9_210000 (or output-directory/VARBC_3_15_9).
```
plotvarbccoeff -i varbc_diag_out/VARBC_3_16_3309_210000 -b
```

## 4. Interpretation

Time-series of VarBC coefficients can be used for monitoring the performance of variational bias correction, specially at the initial steps of the bias correction, when usually initial bias is not corrected (cold-start) and satellite observations are assimilated in passive. Coefficients usually stabilize in time (it depends in many factors), and when they are stable it is assumed that satellite observations should be evaluated for active assimilatedo.


