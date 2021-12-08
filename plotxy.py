#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt
import os
import glob
from pylab import *
# Authors Magnus Lindskog and Ulf Andrae, Dec 2021

file = 'data.dat'

# Read header
f = open(file, 'r')
dimxy  = f.readline().rstrip().lstrip(' #')
aa= dimxy.split("          ")
dim1=int(aa[0])
dim2=int(aa[1])
xlabel = f.readline().rstrip().lstrip('#')
ylabel = f.readline().rstrip().lstrip('#')
title  = f.readline().rstrip().lstrip('#')
f.close()


# Read data
d=np.loadtxt(file,skiprows=4)
xx = d[:,2].reshape(dim1,dim2).transpose()

# Plot
cc=plt.contour(xx,colors='black')
plt.clabel(cc, inline=True, fontsize=12)


plt.contourf(xx, cmap='RdBu_r', alpha=0.5)

plt.colorbar(orientation='vertical');
if dim1==dim2:
   plt.gca().invert_xaxis()

plt.gca().invert_yaxis()
plt.xlabel(xlabel, fontsize=14)
plt.ylabel(ylabel, fontsize=14)
plt.title(title, fontsize=14)
#plt.show()
savefig('data.png')
