#!/usr/bin/env python3

import sys
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import datetime as dt
import numpy as np
import argparse

global pred_map,sat_map,inst_map

pred_map = {   1 : '1 (constant)         ',
               2 : '1000-300hPa thickness',
               3 : '200-50hPa thickness  ',
               4 : 'T_skin               ',
               5 : 'total column water   ',
               6 : '10-2hPa thickness    ',
               7 : '50-5hPa thickness    ',
               8 : 'surface wind speed   ',
               9 : 'nadir view angle  ',
              10 : 'nadir view angle **2 ',
              11 : 'nadir view angle **3 ',
              12 : 'nadir view angle **4 ',
              13 : 'cos solar zen angle  ',
              14 : 'solar elevation      ',
              15 : 'TMI diurnal bias     ',
              16 : 'land or sea ice mask ',
              17 : 'view angle (land)    ',
              18 : 'view angle **2 (land)',
              19 : 'view angle **3 (land)',
              20 : 'ln(rain rate+1)   (1)',
              21 : 'ln(rain rate+1)**2(1)',
              22 : 'ln(rain rate+1)**3(1)',
              23 : 'ln(rain rate+1)   (2)',
              24 : 'ln(rain rate+1)**2(2)',
              25 : 'ln(rain rate+1)**3(2)',
              26 : 'ascent rate  (hPa/s) ',
              27 : 'descent rate (hPa/s) ',
              28 : 'land mask times winds',
              29 : 'day/night            ',
              30 : 'thermal contrast     ',
              31 : 'Radiosonde T 100-850 ',
              32 : 'Radiosonde T  30-200 ',
              33 : 'Radiosonde T   0- 60 ',
              34 : 'Radiosonde T s.elv**1',
              35 : 'Radiosonde T s.elv**2',
              36 : 'Radiosonde log press ',
              37 : 'cos solar zen (full) ',
           }

pred_cols = {  1 : 'black',
               2 : 'red',
               3 : 'orange',
               4 : 'black',
               5 : 'red',
               6 : 'black',
               7 : 'black',
               8 : 'green',
               9 : 'purple',
              10 : 'magenta',
              11 : 'blue',
              12 : 'black',
              13 : 'black',
              14 : 'black',
              15 : 'black',
              16 : 'black',
              17 : 'black',
              18 : 'black',
              19 : 'black',
              20 : 'black',
              21 : 'black',
              22 : 'black',
              23 : 'black',
              24 : 'black',
              25 : 'black',
              26 : 'black',
              27 : 'black',
              28 : 'black',
              29 : 'black',
              30 : 'black',
              31 : 'black',
              32 : 'black',
              33 : 'black',
              34 : 'black',
              35 : 'black',
              36 : 'black',
              37 : 'black',
           }

sat_map = {
           3   : "Metop-B",
           4   : "Metop-A",
           5   : "Metop-C",
           70  : "METEOSAT-11",
           206 : "NOAA-15",
           207 : "NOAA-16",
           209 : "NOAA-18",
           223 : "NOAA-19",
           225 : "NOAA-20",
           523 : "FY-3D",
          }
sen_map = {
            3 : "AMSU-A",
            4 : "AMSU-B",
           15 : "MHS",
           16 : "IASI",
           19 : "ATMS",
           27 : "CRIS",
           29 : "SEVIRI",
           73 : "MWHS2",
          }

#############################################################################################
def plot_varbc_pred_ts(datetime,data,labels,lloc,batch) :


  # Grab meta data from the first row of data 
  nsat=int(data[1][1])
  nsen=int(data[2][1])
  nchn=int(data[3][1])

  npred=int(data[5][1])

  # Number of observations
  nobs=data[3]

  fig, ax = plt.subplots(figsize=(8.27,3.6))

  title_string='VarBC Predictors for '+sat_map[nsat]+': ' +sen_map[nsen]+ ' Channel '+str(nchn)
  plt.title(title_string)

  ax2=ax.twinx()

  # Hide the right and top spines
  ax.spines['right'].set_visible(False)
  ax.spines['top'].set_visible(False)

  # Only show ticks on the left and bottom spines
  ax.yaxis.set_ticks_position('left')
  ax.xaxis.set_ticks_position('bottom')

  ax.xaxis.set_visible(True)
  ax.yaxis.set_visible(True)
  ax.yaxis.grid() #vertical lines
  ax.xaxis.grid() #horizontal lines

  dfmt = mdates.DateFormatter('%d')
  ax.xaxis.set_major_formatter(dfmt)
  ax2.xaxis.set_major_formatter(dfmt)

  ax2.plot_date(x=datetime,y=data[4],fmt=':',color='lightgrey',label='nobs',linewidth=1)

  for pred in range(1,npred+1):
    totpred=np.add(data[pred+5],data[5+npred+pred])
    label_entry=int(data[5+npred+npred+pred][1])
    ax.plot_date(x=datetime,y=totpred,fmt='-',color=pred_cols[label_entry+1],label=pred_map[label_entry+1],linewidth=2)
  
  ax.set_xlabel('Day',fontsize=10)

  majdfmt = mdates.DateFormatter("%b\n%d")
  ax.xaxis.set_major_formatter(majdfmt)

  # Ensure a major tick for each week using (interval=1) 
  ax.xaxis.set_major_locator(mdates.WeekdayLocator(interval=1))
  ax.set_ylabel('Normalised Predictor Value',fontsize=10)
  ax2.set_ylabel('Number of observations',fontsize=10)

  #plot legend
  ax.legend(loc=lloc,prop={'size':10},labelspacing=0,fancybox=False, frameon=True, ncol=1)

  #defining display layout
  plt.tight_layout()

  figsuffix=str(nsat)+'_'+str(nsen)+'_'+str(nchn)
  figname = 'varbc_pred_'+figsuffix+'.png'
  plt.savefig(figname)
  print("Output:",figname)
  if not batch :
    plt.show()

#############################################################################################
def read_data(filename):
  data = {}
  dtdata = []

  print("Read:",filename)
  with open(filename, "r") as a_file:
   for line in a_file:
    line = line.strip()
    tmp = line.split()
    dto = dt.datetime.strptime(tmp[0], '%Y%m%d:%H%M%S')
    dtdata.append(dto)

    for x in range(1,len(tmp)) :
      if x not in data :
        data[x] = []
      data[x].append(float(tmp[x]))

  a_file.close()
  return dtdata, data

#############################################################################################
def main(argv) :

  parser = argparse.ArgumentParser(description='Plot VarBC predictor time-series')
  parser.add_argument('-i',dest="ipath",help='Input file name',default=None,required=True)
  parser.add_argument('-l',dest="lloc",help='Legend location using matplotlib syntax',default=None,required=False)
  parser.add_argument('-d',dest="labels",help='Optional experiment description',default=None,required=False)
  parser.add_argument('-b',action="store_true",help='Batch mode, produce png only',default=False,required=False)

  if len(argv) == 1:
        parser.print_help()
        sys.exit(1)

  args = parser.parse_args()

  if args.labels is None :
    labels = None 
  else:
    labels = args.labels

  data = {}
  ipath = args.ipath
  tsdatetime,tsdata = read_data(ipath)
 
  plot_varbc_pred_ts(tsdatetime,tsdata,args.labels,args.lloc,args.b)

if __name__ == "__main__":
    sys.exit(main(sys.argv))

