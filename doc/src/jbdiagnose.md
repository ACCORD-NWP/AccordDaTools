
# Jb diagnose 

A tool to diagnose HARMONIE structure function files and a comparison between structure functions for different AROME domains
Converted from [original](assets/jbdiagnose.pdf) by Nils Gustafsson



## jbdiagnose

Help/usage:
```
jbdiagnose -h
```

Example:
```
jbdiagnose -b jb_data/stabfiltn_IRELAND75L65_jbconv.bal -c jb_data/stabfiltn_IRELAND75L65_jbconv.cv -g 15000 -l harmL65 -e IRELAND75L65
```

## plotjbbal
Help/usage:
```
plotjbbal -h
```

Example:
```
plotjbbal -t stdv -p QQ -r ./ -e IRELAND75L65
```

## plotjbdiag
Help/usage:
```
plotjbdiag -h
```

Example:
```
plotjbdiag -l 50 -t vercor -p QQ -r ./ -e IRELAND75L65
```


## 1 Introduction
The development and implementation of the HARMONIE data assimilation are now in a quite advanced stage with pre-operational testing at most of the HIRLAMmember weather services. At the same time, the technical and scientific knowledge about various components of this data assimilation is not so widely spread and a joint effort is needed forthe validation of these locally installed system. One of the important components of the HARMONIE data assimilation is the background error statistics. A tool for diagnostics of the background error statistics directly from the background error statistics files has therefore been developed and applied to the recently developed statistics files for HARMONIE at mesoscale (AROME)resolution (2.5 km grid resolution) at SMHI, met.no, FMI, DMI, KNMI and AEMET. This note describes the diagnostic tool (jbdiagnose) and presents results from a comparison of the different statistics from the AROME implementations. Since this diagnostic software is quite fresh, there may certainly be coding errors that could affect the results. Any comments on the comparison and the graphs presented here are therefore most welcome.

## 2 The diagnostic tool
The first part of the diagnostic tool is a simple standalone fortran program (jbdiagnose.F90) that reads the two main background error statistics files (*.bal and *.cv), calculates various diagnostic quantities and writes these diagnostic quantitiesin simple ASCII files that can be used by, for example, GNUPLOT to produce various graphs. The input background error statistics files are opened with the local file names stabal96.bal and stabal96.cv and a namelist variable gsizein, describing the grid resolution, and namelist variables describing the vertical levels (AHALF, BHALF) are needed. Various information about the domain geometry is extracted from the input files. Oneexample of a script to run the diagnostic tool is provided in jbdiagnose.sh. The output ASCII files are stored in the
subdirectory ”diag”.

 Output files of the following forms are produced:
 - spdensVVLL: spectral horizontal covariance densities, variable VV and level LL
 - vercorVVLL: vertical correlations averaged over all horizontal wave numbers, variable VV and level LL
 - baloperVVV: balance operators averaged over horizontal wavenumbers and presented as standard-deviations and percentages of explained variances as functions of vertical level.
 - balwnVVV: balance operators averaged over vertical levels and presented as percentages of explained variances for different horizontal wave-numbers.
 - standdevs: standard deviation vertical profiles for vorticity, unbalanced divergence, unbalanced temperature and unbalanced specific humidity.
where:
 - LL = model level number
 - VV = PP (vorticity), DD (unbalanced divergence), TT (unbalanced temperature and surface pressure), QQ (unbalanced humidity)
 - VVV = div (unbalanced divergence), tps (unbalanced temperature and surface pressure), hum (unbalanced specific humidity)

Note that the input background error statistics files are un-formatted fortran files, that may
be readable only on a particular type of computers (to be confirmed).
## 3 Plotting scripts
The following GNUPLOT plotting scripts are provided (and used toproduce the graphs in this
report):
plotspdensarome.sh: plotting of horizontal spectral covariance densities for a selected level.
plotvercorarome.sh: plotting of vertical correlations between a selected level and other
levels.
plotbaloper.sh: plotting for diagnosis of balance operators.
For the plotting, the input ASCII files (produced by jbdiagnose)should be stored in different
subdirectories diagDDD, where DDD is the name of a particularmodel domain.
## 4 Comparison of AROME structure functions
Structure function diagnostic plots are presented in figures below for the AROME domains
mentioned above. Diagnostic plots from the structure functions for the SMHI ALARO domain
with 5.5 km horizontal resolution have also been included for reference purposes. Most of the
plots ”speek by themselves”. Only a few additional remarks are provided here.

### 4.1 Vorticity standard deviations.

Vertical profiles of standard deviations for the control variable vorticity are shown in Figure 1. Standard deviations for the other control variables are shown and discussed below together with the discussion of the balance operators. The most striking feature of the vorticity standard deviations is the significant increase of the values from the 5 kmdomain to the 2.5 km domains. As will be discussed in connection with the horizontal spectral covariance densities, this can partly be understood from the fact that the vorticity is a linear combination of horizontal partial derivatives of the wind field components. (A horizontal partial derivative in spectral space is essentially a multiplication with the horizontal wave-number, thus a partial derivative means a stronger amplification for smaller scale waves.) Comparing with (what we mainly are used to) standard deviations for vorticity at 15-20 km scale with the NMC method, we may also notice a shift toward lower levels for the maximum of the standard deviations. This may just be a signature of the dominance of smaller scales at lower vertical levels. However, it needs also to be understood whether the perturbation techniques applied to obtain the statistics can have any effects in this connection.

### 4.2 Horizontal spectral densities
Horizontal spectral covariance densities are simply extractedfrom the diagonal of the univariate vertical covariance matrices available for each horizontal wave-number on the input files. Plots for these spectral densities are presented in Figure 2 for the variables vorticity, unbalanced divergence, unbalanced temperature and unbalanced humidity at model level 46 (SE, NO, NL and ES 60 level data) or model level 44 (DK and FI 65 level data). Included in the figure are graphs for the SMHI ALARO 5 km structure functions (SE5) and the 6 different AROME domains SEarome, NLarome, NOarome, FIarome, DKarome and ESarome. Since the wind field is represented by vorticity and divergencethere is a clear shift in the maxima of the spectral densities toward smaller scales. To convert these spectral densities to kinetic energy spectra, for example, one would need to divideby (kstar)^2 where kstar is the total wave-number (kstar ≈ √k^2 +l^2 , where k and l are the horizontal wave-numbers in 2 dimensions). The spectral densities for SE5 and all the arome domains appear to be OK with one exception, the spectral densities for NOarome are significantly smaller than for the other domains (indicating much smaller perturbations in the inputdata sets) and the spectrum for the NOarome humidities appears to be corrupt.

### 4.3 Vertical correlations
Vertical correlations are calculated by extraction of vertical covariances from vertical covariance matrices and by averaging over the horizontal wave-numbers. Vertical correlations with reference to model level 46 (SE, NL, NO and ES) or model level 44 (DK and FI) are shown in Figure 3. These vertical correlation look completely reasonable with a tightening of the vertical correlations for the AROME domains with smaller horizontal scales as compared to the 5 km SMHI domain (true at least for vorticity and divergence). It is also interesting to note the broader vertical correlations for un-balanced temperature in the ESarome domain. One possible (maybe too naive) explanation is the stronger importance of convection in this domain than in the other domains.

### 4.4 Balance operators
The balance operators were not as easily available from the input background error statistics file, since only the regression coefficients for the statistical relations between different control variables were stored in these files. By some simple calculations it was possible to derive the variances of different balanced components. Assume, for example. that SDIV is the vertical balance operator to derive the balanced part of divergence from vorticity (in spectral space DIVbal=SDIV V OR). Then the explained variance can be obtained from the diagonal of SDIV COV(V OR)SDIV T whereCOV(V OR) is the vertical covariance matrix for vorticity. Note that in the background error statistics calculations SDIVconsists of two balance operators, one from vorticity to balanced linearized geopotential and another from this quantity to
balanced divergence.

### Figure 1: Standard deviations of vorticity as function of vertical level for the SMHI 5 km domain and for the six AROME domains.
### Figure 2: Horizontal spectral densities at model level 46 for vorticity (upper left), for unbalanced divergence (upper right), for unbalanced temperature (lower left) and for unbalanced humidity (lower right).
### Figure 3: Vertical correlations between model level 46 and the other model levels for vorticity (upper left), for unbalanced divergence (upper right), forunbalanced temperature (lower left) and for unbalanced humidity (lower right). The vertical correlations were derived by averaging over all horizontal wave-numbers.

#### 4.4.1 Vorticity and divergence balances
Standard deviations of total divergence, divergence explained by vorticity and un-balanced divergence as function of vertical level for the SMHI 5 km domain and for the six AROME domains are shown in Figure 4. As we already have seen from the spectral densities, the standard deviations for divergence increases with model resolution. The fraction of divergence variance explained by vorticity is quite small. The verticalprofiles of divergence variance are rather similar for the different AROME domains. Figure 5 shows directly the percentage of divergence variance explained by vorticity as vertical profiles and Figure 6 the horizontal scale variation of the same percentage of divergence variance explanation. From both figures we may see that a large part of thedivergence variance explained by vorticity relates to large horizontal scales and low levels. One likely interpretation is that this corresponds to frictional inflow in connection with low pressure system.

#### 4.4.2 Balances with temperature involved
Figures 7 - 9 shows the same series of balance diagnostics for temperature as we already have seen for divergence. The coupling between vorticity (via balanced linearized geopotential Pb) can be interpreted as geostrophic balance and this balance isclearly scale- dependent with stronger coupling for the larger horizontal scales. For smallerhorizontal scales we may recognize a coupling between unbalanced divergence and temperature that possibly can be interpreted as a signature of gravity wave oscillations (?). For the smallest horizontal scales we may notice a quite noisy behavour of the balance operator.

#### Figure 4: Standard deviations of total divergence, divergence explained by vorticity and unbalanced divergence as function of vertical level for the SMHI 5 km domain and for the six AROME domains.
#### Figure 5: Vertical variation of the percentage of divergence variance explained by vorticity for the SMHI 5 km domain and the six AROME domains.
#### Figure 6: Horizontal scale variation of the percentage of divergence variance explained by vorticity for the SMHI 5 km domain and the six AROME domains.

Since the spectral densities for these small horizontal scales are relatively small, this ”noise” may not have any practical effects on the assimilation increments. One possible remedy would be to extrapolate the statistical characteristics from a slightly larger horizontal scales, less affected by numerical noise. Some of the background error statistics for the AROME domains were generated with virtual temperature as the input variable instead of temperature. This error may have influenced the statistics that just have been described. More likely, the balance operators between temperature and humidity would have been affected more significantly.

#### 4.4.3 Balances with moisture involved
The diagnostics of the effects of the balance operators with moisture involved are presented in Figures 10 - 12. With regard to the standard deviations, again the standard deviations for the NOarome domain are significantly smaller than for the other domains and it was not meaningful to plot the percentages of explained variances for this domain. With regard to the other domains, the standard deviations for the ESarome domainare larger than for the other domains, most likely because the warmer temperatures over the ESarome domain also mean more moisture. With regard to the percentage of explained variance, the explanantion by vorticity (linearized balanced geopotential) is most dominant for low vertical levels and large horizontal scales. This is also observed for synoptic scale background error statistics and is generally explained by moistening due to frictional inflow in low pressure developments. The percentage of humidity variance explained by un-balanced temperature provides a mixed picture. The vertical variations of the humidity variance explained by un-balanced temperature are quite similar for the SE5, SEarome and DKarome domains, with a strong mid-tropospheric and small horizontal scale coupling between humidity and un-balanced temperature. This can be interpreted as a signature of condensation coupled to latent heating. For the NLarome, FIarome and ESarome domains this coupling appears at much lower vertical levels, possibly indicating a stronger dominance of boundary layer and surfaceflux processes. We can not exclude that what we have seen with regard to moisturebalances could also partly
reflect the erroneous use of virtual temperature instead of temperature in the generation of the
background error statistics.

## 5 Concluding remarks
A tool for diagnosis of background error statistics directly from the background error statistics used in the assimilation has been developed. This tool has been used to compare background error statistics from different AROME domains applied within the HIRLAM community. With exception for the statistics for the NOarome domain, these background error staistics appear reasonable and comparable. Balance operators including humidity may have been influenced by an erroneous use of virtual temperature instead of temperature in the estimation of the background error statistics. This generation of background error statistics should be corrected and this comparison be repeated. Another issue to pay attention to is of course also the perturbation technique that we use to derive the background error statistics with.

#### Figure 7: Standard deviations of total temperature, temperature explained by vorticity, temperature explained by un-balanced divergence and un-balancedtemperature as function of vertical level for the SMHI 5 km domain and for the six AROME domains.
#### Figure 8: Vertical variation of the percentage of temperature variance explained by vorticity and unbalanced divergence for the SMHI 5 km domain and the six AROME domains.
#### Figure 9: Horizontal scale variation of the percentage of temperature variance explained by vorticity and un-balanced divergence for the SMHI 5 km domainand the six AROME domains.
#### Figure 10: Standard deviations of total specific humidity, specific humidity explained by vorticity, specific humidity explained by un-balanced divergence and un-balanced specific humidity as function of vertical level for the SMHI 5 km domain and for the six AROME domains.
#### Figure 11: Vertical variation of the percentage of specific humidity variance explained by vorticity, un-balanced divergence and un-balanced temperature and surface pressure for the SMHI 5 km domain and five AROME domains.
#### Figure 12: Horizontal scale variation of the percentage of specific humidity variance explained by vorticity, un-balanced divergence and un-balanced temperature and surface pressure for the SMHI 5 km domain and five AROME domains.
