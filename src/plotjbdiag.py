#!/usr/bin/env python3

import sys
import matplotlib.pyplot as plt
import argparse

global parname,expmap

parname = { 'TT':'unbalanced temperature',
            'PP':'vorticity',
            'DD':'divergence',
            'QQ':'unbalanced humidity'
          }

expmap  = { 
     'stab_mix_METCOOP25B_janjun_992' : 'Operational, No PertSFC, No PertAna, No LSMIX, ELDA BD',
     'stab_150km_65_20200601_248' : '150km PertSFC, PertAna, LSMIX, IFSENS BD',
     'stab_50km_65_20200601_248' : '50km PertSFC, PertAna, LSMIX, IFSENS BD',
     'stab_50kmnoLSM_65_20200601_248' : '50km PertSFC, PertAna, no LSMIX, IFSENS BD',
     'stab_150kmEBD_65_20200601_248' : '150km PertSFC, PertAna, LSMIX, ELDA BD',
     'stab_150kmNOPRTANA_65_20200601_248' : '150km PertSFC, No PertAna, LSMX, IFSENS BD',
     'stab_EDANPRTLSM_65_20100601_248' : '150km PertSFC, No PertAna, No LSMIX, ELDA BD',
     'stab_nptnlsm_65_20200601_248' : ' 150km PertSFC, No PertAna, No LSMIX, IFSENS BD',
     'test' : 'Test'
          }

#############################################################################################
def plotme(data,par,lev,batch,labels,lloc,type,range) :

  fig, ax = plt.subplots()
  figname = 'fig'
  i = -1
  for exp in data :
   i += 1

   if labels is None :
    label = data[exp]['desc']
   elif i >= len(labels):
    label = data[exp]['desc']
   else :
    label = labels[i]

   if type == 'spdens' :
     ax.loglog(data[exp]['x'],data[exp]['y'],label=label)
   else : 
     ax.plot(data[exp]['x'],data[exp]['y'],label=label)
   figname = '{0}_{1}'.format(figname,exp)

  if lloc is None :
   ax.legend()
  else :
   ax.legend(loc=lloc)

  if par in parname :
    pname=parname[par]
  else :
    pname=par

  if type == 'spdens' :
   ax.invert_xaxis()
   title='Spectral density for {0} at level {1}'.format(pname,lev)
   plt.xlabel('Horizontal wave length (km)')
   plt.ylabel('Spectral density')
   if range is not None :
    ax.set_ylim([float(range[0]),float(range[1])])

  else:
   ax.invert_yaxis()
   title='Vertical correlation for {0}'.format(pname)
   xlabel ='Vertical correlation to level {0}'.format(lev)
   plt.xlabel(xlabel)
   plt.ylabel('Pressure (hPa)')
   if range is not None :
    ax.set_xlim([float(range[0]),float(range[1])])

  ax.set(title=title)

  figname ='{3}_{0}_{1}_{2}'.format(figname,par,lev,type)
  plt.savefig(figname)
  if not batch :
    plt.show()

#############################################################################################
def read_data(filename):
  data = {}
  
  if 'spdens' in filename :
   x = 1
   y = 2
   ys = 1.
  else :
   x = 2
   y = 1
   ys = 100.
  data['x'] = []
  data['y'] = []
  print("Read:",filename)
  with open(filename, "r") as a_file:
   for line in a_file:
    line = line.strip()
    tmp = line.split() ;
    data['x'].append(float(tmp[x]))
    data['y'].append(float(tmp[y])/ys)

  a_file.close()
  return data

#############################################################################################
def main(argv) :

  parser = argparse.ArgumentParser(description='Plotting jb diagnostics')
  parser.add_argument('-lloc',dest="lloc",help='Legend location using matplotlib syntax ',default=None,required=False)
  parser.add_argument('-l',dest="lev",help='Level',default=65,required=False)
  parser.add_argument('-d',dest="labels",help='Optional experiment description',default=None,required=False)
  parser.add_argument('-t',dest="type",help='Type of plot',default='spdens',required=False,choices=['spdens','vercor'])
  parser.add_argument('-p',dest="par",help='Parameter to plot',default='PP',required=False,choices=['PP','DD','QQ','TT'])
  parser.add_argument('-r',dest="rpath",help='Path to data. Default is ./diag_',default='./diag_',required=False)
  parser.add_argument('-e',dest="exps",help='Experiments separated by :',required=True)
  parser.add_argument('-a',dest="range",help='Axis range separated by :',required=False,default=None)
  parser.add_argument('-b',action='store_true',help='Batch mode, produced png only',default=False,required=False)

  if len(argv) == 1:
        parser.print_help()
        sys.exit(1)

  args = parser.parse_args()
  if args.labels is not None :
   labels = args.labels.split(':')
  else :
   labels = None 
  if args.range is not None :
   range = args.range.split(':')
  else :
   range = None 
  exps = args.exps.split(':')

  data = {}
  for exp in exps :
   path = '{4}{0}/{3}{1}{2}'.format(exp,args.par,args.lev,args.type,args.rpath)
   data[exp] = read_data(path)
 
   if exp in expmap :
    data[exp]['desc'] = expmap[exp]
   else :
    data[exp]['desc'] = exp

  plotme(data,args.par,args.lev,args.b,labels,args.lloc,args.type,range)

if __name__ == "__main__":
    sys.exit(main(sys.argv))

