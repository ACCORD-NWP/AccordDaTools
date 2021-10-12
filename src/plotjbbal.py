#!/usr/bin/env python3

import sys
import matplotlib.pyplot as plt
import argparse

global parname,expmap,parfile,map_exp,pb

pb = 'Balanced linearized geopotential (Pb)'

parname = { 'TT':'temperature',
            'PP':'vorticity',
            'DD':'divergence',
            'QQ':'humidity'
          }

parfile = { 'stdv' : { 'TT': 'balopertps',
                       'PP': 'stand_devs',
                       'DD': 'baloperdiv',
                       'QQ': 'baloperhum'
                     },
            'evar_ver' : { 'TT': 'balopertps',
                           'DD': 'baloperdiv',
                           'QQ': 'baloperhum'
                         },
            'evar_hor' : { 'TT': 'bal_wn_tps',
                           'DD': 'bal_wn_div',
                           'QQ': 'bal_wn_hum'
                         }
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
          }

map_exp = { 1 : [1,1], 2 : [1,2], 3 : [1,3],
            4 : [2,2], 5 : [2,3], 6 : [2,3],
            7 : [3,3], 8 : [3,3], 9 : [3,3] }

#############################################################################################
def plot_stdv(data,par,batch,labels,lloc) :

  ld = len(data)
  fig, axs = plt.subplots(map_exp[ld][0],map_exp[ld][1],squeeze=False)
  title='Background error standard deviation for {0}'.format(parname[par])
  fig.suptitle(title)
  figname = 'stdv_{0}_fig'.format(par)
  for i,exp in enumerate(data) :

   if labels is None :
    label = data[exp]['desc']
   else :
    label = labels[i]

   j = int(i/map_exp[ld][1])
   k = i%map_exp[ld][1]

   if par == 'PP' :
    axs[j,k].plot(data[exp][2],data[exp][1],label='Vorticity')
   elif par == 'DD' :
    axs[j,k].plot(data[exp][2],data[exp][1],label='Total')
    axs[j,k].plot(data[exp][3],data[exp][1],label='Explained by pb')
    axs[j,k].plot(data[exp][4],data[exp][1],label='Un-balanced')
   elif par == 'TT' : 
    axs[j,k].plot(data[exp][2],data[exp][1],label='Total')
    axs[j,k].plot(data[exp][3],data[exp][1],label='Explained by pb')
    axs[j,k].plot(data[exp][4],data[exp][1],label='Explained by divergence')
    axs[j,k].plot(data[exp][5],data[exp][1],label='Un-balanced')
   elif par == 'QQ' :
    axs[j,k].plot(data[exp][2],data[exp][1],label='Total')
    axs[j,k].plot(data[exp][3],data[exp][1],label='Explained by pb')
    axs[j,k].plot(data[exp][4],data[exp][1],label='Explained by divergence')
    axs[j,k].plot(data[exp][5],data[exp][1],label='Explained by T and ps')
    axs[j,k].plot(data[exp][6],data[exp][1],label='Un-balanced')
   else :
    msg = 'No handle for {0}'.format(parname[par])
    sys.exit(msg)

   figname = '{0}_{1}'.format(figname,exp)

   if lloc is None :
    axs[j,k].legend()
   else :
    axs[j,k].legend(loc=lloc)
   axs[j,k].set(title=label)

   axs[j,k].invert_yaxis()

  xlabel ='Divergence standard deviation'
  for ax in axs.flat:
    ax.set(xlabel=xlabel,
           ylabel='Pressure (hPa)')

  # Hide x labels and tick labels for top plots and y ticks for right plots.
  for ax in axs.flat:
    ax.label_outer()

  plt.savefig(figname)
  if not batch :
    plt.show()

#############################################################################################
def plot_evar_ver_split(data,par,batch,labels,lloc) :

  map_par = { 'DD' : [5] , 'TT' : [6,7] , 'QQ' : [7,8,9] }
  titles = (pb,'Unbalanced divergence','Unbalanced temp and ps')

  ld = len(map_par[par])
  fig, axs = plt.subplots(map_exp[ld][0],map_exp[ld][1],squeeze=False)
  title='Percentage of explained {0} variance'.format(parname[par])
  fig.suptitle(title)
  figname = 'evar_ver_split_{0}_fig'.format(par)
  for i,col in enumerate(map_par[par]) :

   j = int(i/map_exp[ld][1])
   k = i%map_exp[ld][1]

   for l,exp in enumerate(data) :

    if labels is None :
     label = data[exp]['desc']
    else :
     label = labels[l]

    axs[j,k].plot(data[exp][col],data[exp][1],label=label)

   figname = '{0}_{1}'.format(figname,exp)

   if lloc is None :
    axs[j,k].legend()
   else :
    axs[j,k].legend(loc=lloc)
   axs[j,k].set(title=titles[i])

   axs[j,k].invert_yaxis()
   axs[j,k].set_xlim([0,1])

  for ax in axs.flat:
    ax.set(xlabel='Explained variance',
           ylabel='Pressure (hPa)')

  # Hide x labels and tick labels for top plots and y ticks for right plots.
  for ax in axs.flat:
    ax.label_outer()

  plt.savefig(figname)
  if not batch :
    plt.show()

#############################################################################################
#############################################################################################
def plot_evar_ver(data,par,batch,labels,lloc) :

  ld = len(data)
  fig, axs = plt.subplots(map_exp[ld][0],map_exp[ld][1],squeeze=False)
  title='Percentage of explained {0} variance'.format(parname[par])
  fig.suptitle(title)
  figname = 'evar_ver_{0}_fig'.format(par)
  for i,exp in enumerate(data) :

   if labels is None :
    label = data[exp]['desc']
   else :
    label = labels[i]

   j = int(i/map_exp[ld][1])
   k = i%map_exp[ld][1]

   if par == 'DD' :
    axs[j,k].plot(data[exp][5],data[exp][1],label=pb)
   elif par == 'TT' : 
    axs[j,k].plot(data[exp][6],data[exp][1],label=pb)
    axs[j,k].plot(data[exp][7],data[exp][1],label='Unbalanced divergence')
   elif par == 'QQ' :
    axs[j,k].plot(data[exp][7],data[exp][1],label=pb)
    axs[j,k].plot(data[exp][8],data[exp][1],label='Unbalanced divergence')
    axs[j,k].plot(data[exp][9],data[exp][1],label='Unbalanced temp and ps')
   else :
    msg = 'No handle for {0}'.format(parname[par])
    sys.exit(msg)

   figname = '{0}_{1}'.format(figname,exp)

   if lloc is None :
    axs[j,k].legend()
   else :
    axs[j,k].legend(loc=lloc)
   axs[j,k].set(title=label)

   axs[j,k].invert_yaxis()
   axs[j,k].set_xlim([0,1])

  for ax in axs.flat:
    ax.set(xlabel='Explained variance',
           ylabel='Pressure (hPa)')

  # Hide x labels and tick labels for top plots and y ticks for right plots.
  for ax in axs.flat:
    ax.label_outer()

  plt.savefig(figname)
  if not batch :
    plt.show()

#############################################################################################
def plot_evar_hor(data,par,batch,labels,lloc) :

  ld = len(data)
  fig, axs = plt.subplots(map_exp[ld][0],map_exp[ld][1],squeeze=False)
  title='Percentage of explained {0} variance'.format(parname[par])
  fig.suptitle(title)
  figname = 'evar_hor_{0}_fig'.format(par)
  for i,exp in enumerate(data) :

   if labels is None :
    label = data[exp]['desc']
   else :
    label = labels[i]

   j = int(i/map_exp[ld][1])
   k = i%map_exp[ld][1]

   if par == 'DD' :
    axs[j,k].semilogx(data[exp][1],data[exp][2],label=pb)
   elif par == 'TT' : 
    axs[j,k].semilogx(data[exp][1],data[exp][2],label=pb)
    axs[j,k].semilogx(data[exp][1],data[exp][3],label='Unbalanced divergence')
   elif par == 'QQ' :
    axs[j,k].semilogx(data[exp][1],data[exp][2],label=pb)
    axs[j,k].semilogx(data[exp][1],data[exp][3],label='Unbalanced divergence')
    axs[j,k].semilogx(data[exp][1],data[exp][4],label='Unbalanced temp and ps')
   else :
    msg = 'No handle for {0}'.format(parname[par])
    sys.exit(msg)

   figname = '{0}_{1}'.format(figname,exp)

   if lloc is None :
    axs[j,k].legend()
   else :
    axs[j,k].legend(loc=lloc)
   axs[j,k].set(title=label)

   axs[j,k].invert_xaxis()
   axs[j,k].set_ylim([0,1])

  for ax in axs.flat:
    ax.set(ylabel='Explained variance',
           xlabel='Horizontal wave length km')

  # Hide x labels and tick labels for top plots and y ticks for right plots.
  for ax in axs.flat:
    ax.label_outer()

  plt.savefig(figname)
  if not batch :
    plt.show()

#############################################################################################
def read_data(filename):
  data = {}
  print("Read:",filename)
  with open(filename, "r") as a_file:
   for line in a_file:
    line = line.strip()
    tmp = line.split() ;
    for x in range(1,len(tmp)) :
      sca = 1.0
      if '_wn_' not in filename and x == 1 :
        sca = 0.01
      if x not in data :
        data[x] = []
      data[x].append(float(tmp[x])*sca)

  a_file.close()
  return data

#############################################################################################
def main(argv) :

  parser = argparse.ArgumentParser(description='Plotting jb balances')
  parser.add_argument('-lloc',dest="lloc",help='Legend location using matplotlib syntax',default=None,required=False)
  parser.add_argument('-d',dest="labels",help='Optional experiment description',default=None,required=False)
  parser.add_argument('-t',dest="type",help='Type of plot',default='stdv',required=False,choices=['stdv','evar_ver','evar_hor'])
  parser.add_argument('-p',dest="par",help='Parameter to plot',default='TT',required=False,choices=['PP','DD','QQ','TT'])
  parser.add_argument('-r',dest="rpath",help='Path to data. Default is ./diag_',default='./diag_',required=False)
  parser.add_argument('-e',dest="exps",help='Experiments separated by :',required=True)
  parser.add_argument('-b',action='store_true',help='Batch mode, produced png only',default=False,required=False)
  parser.add_argument('-s',action='store_true',help='Split by stat, else by exp. Only for evar_ver',default='False',required=False)

  if len(argv) == 1:
        parser.print_help()
        sys.exit(1)

  args = parser.parse_args()

  if args.par not in parfile[args.type] :
   print(parname[args.par],"not available for",args.type)
   sys.exit(0)

  if args.labels is not None :
   labels = args.labels.split(':')
  else :
   labels = None 
  exps = args.exps.split(':')

  data = {}
  for i,exp in enumerate(exps) :
   path = '{2}{0}/{1}'.format(exp,parfile[args.type][args.par],args.rpath)
   if exp in data:
    exp = '{0}_{1}'.format(exp,i)
   data[exp] = read_data(path)
 
   if exp in expmap :
    data[exp]['desc'] = expmap[exp]
   else :
    data[exp]['desc'] = exp

  if args.type == 'stdv' :
    plot_stdv(data,args.par,args.b,labels,args.lloc)
  elif args.type == 'evar_ver' :
    if args.s :
     plot_evar_ver_split(data,args.par,args.b,labels,args.lloc)
    else:
     plot_evar_ver(data,args.par,args.b,labels,args.lloc)
  if args.type == 'evar_hor' :
    plot_evar_hor(data,args.par,args.b,labels,args.lloc)

if __name__ == "__main__":
    sys.exit(main(sys.argv))

