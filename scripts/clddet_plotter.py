
#!/usr/bin/env python3

import sys
import os.path
import argparse
import matplotlib.pyplot as plt
import subprocess

#--------------------------------------------------------------
# Routine to convert subprocess stdout to a numeric-list
def cl(x):
    y = x.stdout
    z = y.split('\n') # list of words of x.stdout, removing \n
    z.remove('')      # remove '' in the list
    y = list(map(float,z)) # convierte los elementos de la lista de string a real
    return y
#--------------------------------------------------------------
# number of observations
def nmax():
    filename="clddet_sorted_smoothed.dat"
    a="cat"; b="|grep observations"; cmd=a+" "+filename+b
    x = subprocess.run(cmd, shell=True, capture_output=True, text=True)
#    print(x.stdout)
    y=x.stdout.split() # list of words of x.stdout
    print(y)
    jmax=y[3]
    return jmax

#--------------------------------------------------------------
# string 'bt' to search in file
filename="clddet_sorted_smoothed.dat"

if os.path.exists(filename):
    print('clddet_sorted_smoothed.dat file exists')
    a="cat"; b="|grep bt";c="|cut -c3-21"; cmd=a+" "+filename+b+c
    x = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    y=x.stdout.split('\n') #convierte cada linea en un elemento de una lista
    y=y[0]+y[1]+y[3]+y[4] # seleccionamos algunos elementos de las listas
    z=y.split()
    z=list(map(float,z))

    xt = [] ; xt.append(z[0]) ; xt.append(z[2])
    tn = [] ; tn.append(z[1]) ; tn.append(z[3])
    tp = [] ; tp.append(z[5]) ; tp.append(z[7])
else:
    print('file clddet_sorted_smoothed.dat does not exist')
    sys.exit()
#--------------------------------------------------------------
# Read O-B departures in vertically-ranked channel space

def plot_clddet(j,batch) :

    filename="clddet_sorted_smoothed.dat"

    a="awk '$9 == " ; b=j ; c="{print $5}'" ; cmd=a+" "+b+" "+c+" "+filename
    ranked = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    xr = cl(ranked)

    a="awk '$9 == " ; b=j ; c="{print $6}'" ; cmd=a+" "+b+" "+c+" "+filename
    raw = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    yr = cl(raw)

    a="awk '$9 == " ; b=j ; c="{print $8}'" ; cmd=a+" "+b+" "+c+" "+filename
    smooth = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    ys = cl(smooth)

#--------------------------------------------------------------
# Read cloud top divider in vertically-ranked channel space xc and yc
    if float(j) != 0 :
       a="awk '$9 == " ; b=j ; c="{print $0}'" ; d="|grep ' 0$'|wc -l" ; cmd=a+" "+b+" "+c+" "+filename+d
       x = subprocess.run(cmd, shell=True, capture_output=True, text=True)
       x1 = cl(x)
       xc = [] 
       xc.append(x1) ; xc.append(x1)
    else :
       xc = [139, 139]

    yc = [-200.0,200.0]

# Create the figure
    fig = plt.figure(num=1, clear=True)

# Plot gray shading to show the area between O-B thresholds
    plt.fill_between( xt, tn, tp, facecolor='gray', color='gray',
                  alpha=0.5, label='Thresholds')

# Plot raw and smoothed O-B departures
    plt.plot( xr, yr, color='black', linestyle='none', marker='o',
          markerfacecolor='black', markersize=2, label='Raw data')
    plt.plot( xr, ys, color='red', label='Smoothed')

# Plot the cloud top divider
    plt.plot( xc, yc, color='red', linestyle='dashed',
          label='Cloud divider')

# Set the x- and y-axis ranges and labels
    plt.xlim(0.5,len(yr)+0.5)
    plt.xlabel('Ranked channel index')
    plt.ylim(-10., 10.)
    plt.ylabel('Departure [K]')
#plt.title(j)

# Show and save in a PNG file
    plt.legend()
    fig.savefig('clddet.png', format='png', dpi=100)
    print("Figure saved in file clddet.png")

    if not batch :
       plt.show()

#############################################################################################
def main(argv) :

  parser = argparse.ArgumentParser(description='Cloud detection')
  parser.add_argument('-j',dest="iob",help='index profile',default=None,required=False)
  parser.add_argument('-b',action="store_true",help='Batch mode, produce png only',default=False,required=False)
 
  parser.print_help()

  args = parser.parse_args()

  if args.iob is None :
    iob = '0' 
  else:
    iob = args.iob
    print('Profile',iob)


  jmax = nmax() # def nmax return

  if float(iob) > float(jmax) :
     print(); print('  -j ',iob,'larger than jmax',jmax);print()
     sys.exit(1)

 
  plot_clddet(iob,args.b)

if __name__ == "__main__":
    sys.exit(main(sys.argv))

