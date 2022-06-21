#!/usr/bin/env python3

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np
import datetime


obindex,obnum,obdfsabs= np.loadtxt("dfs.dat", unpack=True)

#fig = plt.figure(figsize=(8.27,1.8))
fig = plt.figure()
ax = fig.add_subplot(111)

lw=3
#     1 : SYNOP_Z #     2 : SYNOP_T2 #     3 : SYNOP_R2 #     4 : SYNOP_U10 #     5 : SYNOP_ZTD
#     6 : TEMP_U  #     7 : TEMP_T   #     8 : TEMP_Z   #     9 : TEMP_Q    #    10 : AIREP_T
#    11 : AIREP_U #    12 : SATOB_U  #    13 : DRIBU_Z  #    14 : DRIBU_U   #    15 : PILOT_Z
#    16 : PILOT_U #    17 : AMSUA    #    18 : MHS      #    19 : MWHS2     #    20 : ATMS
#    21 : IASI    #    22 : CRIS     #    23 : SEV_WV   #    24 : SEV_WIN   #    25 : SEV_C11
#    26 : SCATT_U #    27 : RADAR_Z  #    28 : RADAR_U  #    29 : TEMP_CLS
dfsobsstr= ('SYNOP-Z','SYNOP-T2','SYNOP-R2','SYNOP-U10','GNSS-ZTD',
            'TEMP-U','TEMP-T','TEMP-Z','TEMP-Q','AIREP-T',
            'AIREP-U','SATOB-U','BUOY-Z','BUOY-U','PILOT-Z',
            'PILOT-U','AMSU-A','MHS','MWHS','ATMS',
            'IASI','CRIS','SEV-WV','SEV-WIN','SEV_C11',
            'SCATT-U','RADAR-Z','RADAR-U','TEMP_CLS')

strpos= np.arange(len(dfsobsstr))
#plt.bar(strpos,obdfsabs/obnum, align='center', alpha=0.5)
plt.bar(strpos,obdfsabs, align='center', alpha=0.5)

plt.xticks(strpos, dfsobsstr,rotation=60)

plt.ylabel('DFS',fontsize=10)

# Hide the right and top spines
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)
# Only show ticks on the left and bottom spines
ax.yaxis.set_ticks_position('left')
ax.xaxis.set_ticks_position('bottom')
#ax.xaxis.set_major_locator(ticker.MultipleLocator(6))

#ax.set_yscale("log", nonposx='clip')
ax.xaxis.set_visible(True)
ax.yaxis.set_visible(True)
ax.yaxis.grid() #horizontal lines

plt.tight_layout()

plt.savefig('dfsrel.png',format='png')

plt.show()
