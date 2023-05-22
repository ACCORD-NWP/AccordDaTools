#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt
import os
import glob
from pylab import *
matplotlib.use('Agg')
import argparse

# Authors Magnus Lindskog and Ulf Andrae, Dec 2021

def plotme(idata, oplot) :
  filename = idata

  # Read header
  f = open(filename, 'r')
  dimxy  = f.readline().rstrip().lstrip(' #')
  aa= dimxy.split("          ")
  dim1=int(aa[0])
  dim2=int(aa[1])
  xlabel = f.readline().rstrip().lstrip('#')
  ylabel = f.readline().rstrip().lstrip('#')
  title  = f.readline().rstrip().lstrip('#')
  f.close()


  # Read data
  d=np.loadtxt(filename,skiprows=4)
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

  savefig(oplot)

#############################################################################################
def main(argv) :

  parser = argparse.ArgumentParser(description='Plotting DIACOV diagnostics')
  parser.add_argument('-i',dest="idata",help='Input data files',default='DUMMY',required=True)
  parser.add_argument('-o',dest="oplot",help='Level',default='output.png',required=False)

  if len(argv) == 1:
        parser.print_help()
        sys.exit(1)

  args = parser.parse_args()

  plotme(args.idata,args.oplot)

if __name__ == "__main__":
    sys.exit(main(sys.argv))


