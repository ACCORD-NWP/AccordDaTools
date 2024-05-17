
# Degree of Freedom Signal

"Degree of Freedom Signal" (DFS) is a used to quantify the influence of observational data on the analysis in numerical weather prediction (NWP) and other geophysical systems. It provides a measure of the information content of the observations with respect to the analysis.

### Definition
The DFS is defined as the trace of the product of the observation sensitivity matrix and the observation error covariance matrix. Mathematically, it can be expressed as:

$$ \text{DFS} = \text{Tr}(\mathbf{K} \mathbf{H}) $$

where:
- $\mathbf{K}$ is the Kalman gain matrix, which represents how much weight is given to the observations in the assimilation process.
- $\mathbf{H}$ is the observation operator, which maps the model state variables to the observed variables.

### Interpretation
1. **Information Content**: DFS indicates how much the observations have influenced the analysis. A higher DFS means that the observations have a significant impact on the analysis, providing more information.
2. **Observational Weight**: It reflects the relative weight of the observations compared to the background information (prior model state). A higher DFS suggests that the observations are trusted more compared to the model forecast.
3. **Data Quality and Quantity**: The DFS can help assess the quality and the effective number of independent observations. High DFS values can indicate good quality data or a large number of observations contributing to the analysis.

### Importance in Data Assimilation
- **Performance Monitoring**: DFS can be used to monitor and evaluate the performance of the data assimilation system. By tracking the DFS over time, one can identify periods where observations are particularly influential or where the system might rely too heavily on the background model.
- **Observation System Design**: In designing observational networks, DFS can help determine the most valuable locations and times for taking observations to maximize their impact on the analysis.
- **Error Diagnostics**: By analyzing the DFS, one can diagnose potential issues with observation errors, model errors, or the assimilation scheme itself.

### Practical Example
In NWP, suppose you assimilate satellite radiance data into a global atmospheric model. The DFS would tell you how much the satellite data has altered the model's initial conditions. If the DFS is high, the satellite data has significantly adjusted the model state, indicating that the data is highly informative. Conversely, a low DFS might suggest that the observations are not very influential, possibly due to high observational errors or redundancy with existing data.

### References
 - Chapnik, B., Desroziers, G., Rabier, F., & Talagrand, O. (2006). Diagnosis and tuning of observational error statistics in a quasi-operational data assimilation setting. *Quarterly Journal of the Royal Meteorological Society, 132*(616), 543-565. https://doi.org/10.1256/qj.05.82
 - Cardinali, C., Pezzulli, S., & Andersson, E. (2004). Influence-matrix diagnostic of a data assimilation system. *Quarterly Journal of the Royal Meteorological Society, 130*(603), 2767-2786. https://doi.org/10.1256/qj.03.205


## Calculate DFS

The dfscomp tool reads the (ASCII) data from an unperturbed CCMA and a perturbed CCMA.

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

