# DIACOV

The **DIACOV** tool is used to "_diagnose what's in covariance files_".
## Usage
 1. First, you should have Festat output and the DIACOV binary compiled by Harmonie stored somewhere.
 2. Then, create the new directory, copy the DIACOV binary and Jb files there. Example:

## Tools
```
mkdir DiacovTest
cd DiacovTest
cp /path/to/DIACOV .
mkdir dataTest
cp /path/to/Jb/data dataTest/.
```
#### Run Diacov
Use rundiacov script to initiate the processing of the Jb files.
Help/usage:
```
rundiacov -h
```

Example:
```
rundiacov -i dataTest -c METCOOP25D_65_2021100300-2021111006_412 -g 2.5 -d /path/to/DIACOV
```

#### Produce diagonal stats
When you get DIACOV output, use diag_diacov script to produce stats in ASCII format, which are suitable for further plotting.
Help/usage:
```
diag_diacov -h
```

Example:
```
diag_diacov -i dataTest -o stats
```

#### Produce plots
Use plotdiacov script to produce the plots from ACSII files.
Help/usage:
```
plotdiacov -h
```

You can either produce the plots for all of the available stat files, or to choose specific ones. Examples:
```
plotdiacov -d ./stats                                     # the plots will be saved to the same directory as stat files
plotdiacov -i ./stats/corqu65.dat -o ./stats/corqu65.png  # if the output file name is not specified, the plot is saved to the pwd with the default name "output.png"
```

