###
import argparse
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from mpl_toolkits.basemap import Basemap


def main(Loop1, Loop2, Loop3):
     INPUT1="{}".format(Loop1)
     INPUT2="{}".format(Loop2)
     INPUT3="{}".format(Loop3)
#load data from ASCII from odb
## From loop1:
###   date , time, statid@hdr, lat, lon, endtime, timeslot, fg_depar, lores
###     0      1       2,3      4     5        6         7       8      9
### where, loop1 start: fg_depar=FirstGuessDep and loop1 end: lores=dep after 4DVminim1
#####
## From loop2:
###   date , time, statid@hdr, lat, lon, endtime, timeslot, lores, hires
###     0      1       2,3      4     5       6         7      8      9
### where, loop2 start hires = depa after 4DVtraj1 and loop2 end lores=dep after 4DVminim2
###
###
###
     print('++++++++++++++++++++++++++++ Reading ODB files : ', INPUT1) 
     df1 = pd.read_csv(INPUT1, sep=" ")

     df1.replace('NULL',np.NaN)
     lat1 = pd.to_numeric(df1['lat@hdr'], errors='coerce')
     lon1 = pd.to_numeric(df1['lon@hdr'], errors='coerce')
#
     print('++++++++++++++++++++++++++++ Reading ODB files : ', INPUT2) 
     df2 = pd.read_csv(INPUT2, sep=" ")

     df2.replace('NULL',np.NaN)
     lat2 = pd.to_numeric(df2['lat@hdr'], errors='coerce')
     lon2 = pd.to_numeric(df2['lon@hdr'], errors='coerce')

#
     print('++++++++++++++++++++++++++++ Reading ODB files : ', INPUT3) 
     df3 = pd.read_csv(INPUT3, sep=" ")

     df3.replace('NULL',np.NaN)
     lat3 = pd.to_numeric(df3['lat@hdr'], errors='coerce')
     lon3 = pd.to_numeric(df3['lon@hdr'], errors='coerce')

###################### 2D departures
     timeslot1 =  pd.to_numeric(df1['timeslot@timeslot_index'], errors='coerce')
     fg_dep =  pd.to_numeric(df1['fg_depar@body'], errors='coerce')
     minim1 =  pd.to_numeric(df1['lores@update_1'], errors='coerce')
## time series    

     print('++++++++++++++++++++++++++++ Statistics by TSlot, loop1 ') 
     FG_dep, FG_std = getStatsTimeSlot(fg_dep, timeslot1)
     MINI1_dep, MINI1_std = getStatsTimeSlot(minim1, timeslot1)

###################### 2D departures
     timeslot2 =  pd.to_numeric(df2['timeslot@timeslot_index'], errors='coerce')
     traj1 = pd.to_numeric(df2['hires@update_2'], errors='coerce')
     minim2   = pd.to_numeric(df2['lores@update_2'], errors='coerce')
## time series    
     print('++++++++++++++++++++++++++++ Statistics by TSlot, loop2 ') 
     TRAJ1_dep, TRAJ1_std = getStatsTimeSlot(traj1, timeslot2)
     MINI2_dep, MINI2_std = getStatsTimeSlot(minim2, timeslot2)

###################### 2D departures
     timeslot3 =  pd.to_numeric(df3['timeslot@timeslot_index'], errors='coerce')
     traj2 = pd.to_numeric(df3['hires@update_3'], errors='coerce')
## time series    
     print('++++++++++++++++++++++++++++ Statistics by TSlot, loop3 (end of loop2) ') 
     TRAJ2_dep, TRAJ2_std = getStatsTimeSlot(traj2, timeslot3)
#
#############################################################
###   figures with statistics 

     print('++++++++++++++++++++++++++++ figures ') 


     createPlots (lat1, lon1, fg_dep, minim1, 
                  lat2, lon2, traj1, minim2, lat3, lon3, traj2, FG_dep, 
                  MINI1_dep, TRAJ1_dep, MINI2_dep, TRAJ2_dep, FG_std, 
                  MINI1_std, TRAJ1_std, MINI2_std, TRAJ2_std)


#############################################################
###   ASCII files with time series

     print('++++++++++++++++++++++++++++ writes departures as a function of TSlot in an ASCII file ') 

     createASCII (FG_dep,MINI1_dep, TRAJ1_dep, MINI2_dep, TRAJ2_dep, 
                  FG_std,MINI1_std, TRAJ1_std, MINI2_std, TRAJ2_std)

#############################################################
def getStatsTimeSlot(field, timeslot):
    
    Function_mean = np.empty(10)
    Function_std = np.empty(10)
    for i in range (0, 9):
        Function_mean[i] = field.loc[timeslot == i + 1 ].mean()
        Function_std[i] = field.loc[timeslot == i + 1 ].std()

    return Function_mean, Function_std
#
#############################################################
def createPlots (lat1, lon1, fg_dep, minim1, 
                 lat2, lon2, traj1, minim2, lat3, lon3, traj2, 
                 FG_dep, MINI1_dep, TRAJ1_dep, MINI2_dep, TRAJ2_dep, 
                 FG_std, MINI1_std, TRAJ1_std, MINI2_std, TRAJ2_std):

##
##   Maps:
##    minlim = colorbar min
##    maxlim = colorbar max
##    cm  = colorbar chhosen
##    Name = filename produced
##  
###########   user settings:
##
     minlim = -2
     maxlim =  2
     cm = plt.cm.get_cmap('RdBu_r')
     Name = "teste.png"



##   To basemap:
     ulat = np.max(lat1)
     llat = np.min(lat1)
     rlon = np.max(lon1)
     llon = np.min(lon1)


     plt.close("all")

     fig = plt.figure(1,figsize=(12,8))

     Tsl = np.array([el for el in range(1,11, 1)])

     ax0 = plt.subplot2grid((2, 4), (0, 0), colspan=2)
     ax0.set_title("departures")
     ax0.set_xlim((0, 11))
     ax0.set_xticks((1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,10))
     ax0.plot(Tsl, FG_dep, color='blue', linestyle='--', marker='^', fillstyle='none', markersize=8, label='fg')
     ax0.plot(Tsl, MINI1_dep, color='dodgerblue', linestyle='--', marker='x', fillstyle='none', markersize=8, label='minim1')
     ax0.plot(Tsl, TRAJ1_dep,color='red', linestyle='--', marker='o', fillstyle='none', markersize=8,label='traj1')
     ax0.plot(Tsl, MINI2_dep,color='darkred', linestyle='--', marker='d', fillstyle='none', markersize=8, label='minim2')
     ax0.plot(Tsl, TRAJ2_dep,color='green', linestyle='--', marker='+', fillstyle='none', markersize=8, label='traj2')
     ax0.grid(True)
     ax0.legend(loc='upper right', borderaxespad=0.)
     ax0.set_xlabel(" time slot number")
     ax0.set_ylabel("mean")
##
     ax1 = plt.subplot2grid((2, 4), (1, 0), colspan=2)
     ax1.set_xlim((0, 11))
     ax1.set_xticks((1,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,2,3,4,5,6,7,8,9,10))
     ax1.plot(Tsl, FG_std, color='blue', linestyle='--', marker='^', fillstyle='none', markersize=8, label='fg')
     ax1.plot(Tsl, MINI1_std, color='dodgerblue', linestyle='--', marker='x', fillstyle='none', markersize=8, label='minim1')
     ax1.plot(Tsl, TRAJ1_std,color='red', linestyle='--', marker='o', fillstyle='none', markersize=8,label='traj1')
     ax1.plot(Tsl, MINI2_std,color='darkred', linestyle='--', marker='d', fillstyle='none', markersize=8, label='minim2')
     ax1.plot(Tsl, TRAJ2_std,color='green', linestyle='--', marker='+', fillstyle='none', markersize=8, label='traj2')
     ax1.grid(True)
     ax1.legend(loc='lower left', borderaxespad=0.)
     ax1.set_xlabel(" time slot number")
     ax1.set_ylabel(" std")

     ax2 = plt.subplot2grid((2, 4), (0, 2), colspan=2)
     plt.title('departures loop1: Fg-4DVtraj1')
     m=Basemap(projection='cyl', llcrnrlon=llon, urcrnrlon=rlon, 
               llcrnrlat=llat, urcrnrlat=ulat, lat_0=llat, lon_0=llon, resolution='h')
     x, y = m(lon1,lat1)
     m.drawcoastlines()
     m.drawparallels(np.arange(0.,360.,5.),labels=[1,0,0,0], fontsize=7)
     m.drawmeridians(np.arange(-180.,180.,5.),labels=[0,0,0,1], fontsize=7)
     im=m.scatter(x,y,c=fg_dep-traj1, marker='s',vmin = minlim , vmax = maxlim, s= 1,cmap=cm)
     cbar=m.colorbar(im, location='bottom',pad="15%")

##
     ax3 = plt.subplot2grid((2, 4), (1, 2), colspan=2)
     plt.title('departures loop2 : 4DVminim2-4DVtraj2')
     m=Basemap(projection='cyl', llcrnrlon=llon, urcrnrlon=rlon, llcrnrlat=llat, urcrnrlat=ulat, lat_0=llat, lon_0=llon
, resolution='h')
     x, y = m(lon2,lat2)
     m.drawcoastlines()
     m.drawparallels(np.arange(0.,360.,5.),labels=[1,0,0,0], fontsize=7)
     m.drawmeridians(np.arange(-180.,180.,5.),labels=[0,0,0,1], fontsize=7)
     im=m.scatter(x,y,c=minim2-traj2,  marker='s',vmin = minlim, vmax = maxlim, s= 1,cmap=cm)
     cbar=m.colorbar(im, location='bottom',pad="15%")

     plt.savefig(Name)
     plt.show()

############
def createASCII (FG_dep, MINI1_dep, TRAJ1_dep, MINI2_dep, TRAJ2_dep,
                 FG_std, MINI1_std, TRAJ1_std, MINI2_std, TRAJ2_std):
   
##    fName = ASCII file name where statistics should be written
##  
###########   user settings:
     fName = "teste.txt"

     Tsl = np.array([el for el in range(1,11, 1)])
     
     a = np.array([Tsl, FG_dep, MINI1_dep, TRAJ1_dep, MINI2_dep, TRAJ2_dep, FG_std,MINI1_std, TRAJ1_std, MINI2_std, TRAJ2_std])
     b = np.transpose(a)
     HEADER = 'Tsl FG 4DVminim1 4DVtraj1 4DVminim2 4DVtraj2 FG_std 4DVminim1_std 4DVtraj1_std 4DVmini2_std 4DVtraj2_std'

     np.savetxt(fName, b, fmt=['%0.2d','%2.4f', '%2.4f','%2.4f','%2.4f','%2.4f','%2.4f','%2.4f','%2.4f','%2.4f','%2.4f'], header=HEADER, delimiter="  ", newline='\n')

     return

#############################################################
##
def create_parser():
    
    parser = argparse.ArgumentParser()
    parser.add_argument("Filename1", help = "name and path 1st loop ASCII file ($SCRATCH/)", type = str)
    parser.add_argument("Filename2", help = "name and path 2nd loop ASCII file ($SCRATCH/)", type = str)
    parser.add_argument("Filename3", help = "name and path 3rd loop ASCII file ($SCRATCH/)", type = str)

    return parser

if __name__ == "__main__":
#
    parser = create_parser()
    args = parser.parse_args()
    main(args.Filename1, args.Filename2, args.Filename3)


