
# Degree of Freedom Signal

"Degree of Freedom Signal" (DFS) is a used to quantify the influence of observational data on the analysis in numerical weather prediction (NWP) and other geophysical systems. It provides a measure of the information content of the observations with respect to the analysis.

### Definition
The DFS is defined as the trace of the product of the observation sensitivity matrix and the observation error covariance matrix. Mathematically, it can be expressed as:

```math
\text{DFS}_i = \sum_i \frac{\partial H_i \mathbf{x}_a}{\partial y_i}
```
or
```math
\text{DFS}_i = \text{Tr}(\mathbf{K} \mathbf{H})_i
```

where:
- \( \mathbf{K} \) is the Kalman gain matrix, which represents how much weight is given to the observations in the assimilation process.
- \( \mathbf{H} \) is the observation operator, which maps the model state variables to the observed variables.
- \( H \mathbf{x}_a \) is the analysis mapped to observation space.
- \( y_i \) is the i-th observation.

As there is no explicit \( K \) in the variational assimilation, a _Monte Carlo_ approach can be applied:
```math
\partial y'^T \mathbf{H} \mathbf{K} \partial y' = \text{Tr} (\mathbf{H} \mathbf{K})y'^T\partial y') = \text{Tr}(\mathbf{K} \mathbf{H})_i
```

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

## `datool_dfs`

The `datool_dfs` tool is written in Python and reads ASCII input files that have been produced using the following ODB SQL:
```bash
odbsql -q 'select obstype, codetype, statid, varno, vertco_reference_1, degrees(lat), degrees(lon), an_depar, tdiff(date,time,andate,antime)/60, fg_depar, obsvalue from hdr,desc,body where varno/=91 and an_depar/="NULL" and obstype/=7' > ccma.dat
```

## Calculate and plot DFS

The `datool_dfs` tool reads the (ASCII) data from an unperturbed CCMA and a perturbed CCMA.

Help/usage:
```bash
user@pc:~$ ./datool_dfs.py -h
usage: datool_dfs.py [-h] [--file1 FILE1] [--file2 FILE2] [--write-dfs] [--plot [{raw,perobs,percent}]] [--plot-style PLOT_STYLE] [--list-plot-styles]

Compare DFS from perturbed and unperturbed CCMA ODB queries.

optional arguments:
  -h, --help            show this help message and exit
  --file1 FILE1         ODB ASCII file from perturbed CCMA
  --file2 FILE2         ODB ASCII file from unperturbed CCMA
  --write-dfs           Write raw DFS data to dfs.dat
  --plot [{raw,perobs,percent}]
                        Generate a DFS plot:
                          percent - percentage contribution (default)  perobs  - DFS per observation
                          raw     - total DFS
  --plot-style PLOT_STYLE
                        Matplotlib style for plotting
  --list-plot-styles    List available matplotlib plot styles and exit
ewhelan@realin23:~/git/AccordDaTools_git/AccordDaTools/scripts (feature/move_dfs_to_python)$
```

Example:
```bash
user@pc:~$ datool_dfs.py  --file1=test_data/mbr000/ccma.dat --file2=test_data/mbrprt/ccma_pert.dat --plot=perobs --plot-style=fivethirtyeight && eog dfs_plot_perobs.png
user@pc:~$ datool_dfs: Options OK. Let's process data ...
user@pc:~$ datool_dfs: Observations used   : 249751
user@pc:~$ datool_dfs: Observations unused : 0
user@pc:~$ datool_dfs: Bar chart saved to dfs_plot_perobs.png
ewhelan@realin23:~/gi
```

