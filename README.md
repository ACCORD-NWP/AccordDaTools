# AccordDaTools
Collection of ACCORD DA tools. So far we have jbdiagnose and associated plotting. 

Desirables:
- diacov
- festat
- TuneBR
- obstat
- Other ...

## Installation
```
cd AccordDaTools
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=/path/to/da_tools 
make 
ctest
make install
```
Add DA Tools to your PATH:
```
export PATH=/path/to/da_tools:$PATH
```

## Tools
### Structure functions
#### Jb diagnose
Full documentation is available [here](doc/tools/jbdiagnose.md).

Help/usage:
```
jbdiagnose -h
```

Example:
```
jbdiagnose -b jb_data/stabfiltn_IRELAND75L65_jbconv.bal -c jb_data/stabfiltn_IRELAND75L65_jbconv.cv -g 15000 -l harmL65 -e IRELAND75L65
```

#### plotjbbal
Help/usage:
```
plotjbbal -h
```

Example:
```
plotjbbal -t stdv -p QQ -r ./ -e IRELAND75L65
```

#### plotjbdiag
Help/usage:
```
plotjbdiag -h
```

Example:
```
plotjbdiag -l 50 -t vercor -p QQ -r ./ -e IRELAND75L65 
```
### VarBC
#### VARBC.cycle data extraction
Help/usage:
```
varbcdiagnose -h
```

Example:
```
varbcdiagnose -i /path/to/directory/with/varbc_files/ -o varbc_diag_out
```

#### VarBC predictor time-series plotting
Help/usage:
```
plotvarbcpred -h
```

Example:
```
plotvarbcpred -i varbc_diag_out/VARBC_3_16_3309_210000 -b
```

### DFS
#### DFS computation
The dfscomp tool reads the (ASCII) data from an unperturbed CCMA and a perturbed CCMA.

Help/usage:
```
dfscomp -h
```

Example:
```
dfscomp -p CCMA_2022050112.dat -u CCMA_2022050112_unpert.dat -o dfs2022050112.dat
```
