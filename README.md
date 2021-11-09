# AccordDaTools
Collection of ACCORD DA tools. So far we have jbdiagnose and associated plotting. 

Desirables:
- diacov
- festat
- VarBC coeff plotting
- TuneBR
- obstat
- DFS?
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
### Jb diagnose
Full documentation is available [here](doc/tools/jbdiagnose.md).

Help/usage:
```
jbdiagnose -h
```

Example:
```
jbdiagnose -b jb_data/stabfiltn_IRELAND75L65_jbconv.bal -c jb_data/stabfiltn_IRELAND75L65_jbconv.cv -g 15000 -l harmL65 -e IRELAND75L65
```

### plotjbbal
Help/usage:
```
plotjbbal -h
```

Example:
```
plotjbbal -t stdv -p QQ -r ./ -e IRELAND75L65
```

### plotjbdiag
Help/usage:
```
plotjbdiag -h
```

Example:
```
plotjbdiag -l 50 -t vercor -p QQ -r ./ -e IRELAND75L65 
```
