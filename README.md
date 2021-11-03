# AccordDaTools
Collection of ACCORD DA tools

## Installation
```
cd AccordDaTools
mkdir build
cd build
cmake .. -DCMAKE_INSTALL_PREFIX=/path/to/da_tools 
make 
make install
```
Add DA Tools to your PATH:
```
export PATH=/path/to/da_tools:$PATH
```

## Tools
### Jb diagnose
 Help/usage:
```
jbdiagnose -h
```

 Example:
```
jbdiagnose -b jb_data/stabfiltn_IRELAND75L65_jbconv.bal -c jb_data/stabfiltn_IRELAND75L65_jbconv.cv -g 15000 -l harmL65
```
