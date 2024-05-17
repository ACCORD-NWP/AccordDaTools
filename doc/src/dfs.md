
# DFS
The dfscomp tool reads the (ASCII) data from an unperturbed CCMA and a perturbed CCMA.

## Calculate DFS
Help/usage:
```
scripts/dfscomp.sh -h
```

Example:
```
scripts/dfscomp.sh -u $DTG_CCMA_unpert.dat -p $DTG_CCMA.dat -o dfs.dat
```

## Plot DFS
Help/usage:
```
python3 scripts/plotdfs.py -h
```

Example:
```
python3 scripts/plotdfs.py -i dfs.dat
```

