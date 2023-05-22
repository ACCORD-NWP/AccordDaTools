#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt
import argparse
import sys
import glob

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

  plt.savefig(oplot)

#############################################################################################
def main(argv) :

  parser = argparse.ArgumentParser(description='Plotting DIACOV diagnostics')
  parser.add_argument('-d',dest="indir",help='iPlot all input data files in this directory',default=False,required=False)
  parser.add_argument('-i',dest="idata",help='Input data files',default=False,required=False)
  parser.add_argument('-o',dest="oplot",help='Level',default='output.png',required=False)

  if len(argv) == 1:
    parser.print_help()
    sys.exit(1)

  args = parser.parse_args()

  if args.idata != False and args.indir == False:
    plotme(args.idata,args.oplot)
  elif args.indir != False and args.idata == False:
    datlist=glob.glob(args.indir+'/*.dat') 
    for infile in datlist:
      output=infile.replace('.dat','.png')
      print (' ... plotting '+output)
      plotme(infile,output)
  else:
    print ('Cannot do both!')
    sys.exit(1)

if __name__ == "__main__":
  sys.exit(main(sys.argv))
