import matplotlib.pyplot as plt

#--------------------------------------------------------------
# Routine to read one-dimensional array data from an ascii file
def readIn(filename):
    f=open(r""+filename)
    f=f.read().split("\n")
    f=f[0:len(f)-1]
    x=[]
    for i in f:
        x.append(float(i))
    return x
#--------------------------------------------------------------

# Read the cloud detection O-B threshold information 
xt=readIn("threshold_x.dat")
tn=readIn("threshold_lower.dat")
tp=readIn("threshold_upper.dat")

# Read O-B departures in vertically-ranked channel space
xr=readIn("ranked_indices.dat")
yr=readIn("raw_departures.dat")
ys=readIn("smoothed_departures.dat")

# Read cloud top divider in vertically-ranked channel space
xc=readIn("cloud_x.dat")
yc=readIn("cloud_y.dat")

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
plt.ylim(-2.5, 2.5)
plt.ylabel('Departure [K]')

# Show and save in a PNG file
plt.legend()
plt.show()
fig.savefig('clddet.png', format='png', dpi=100)
print("Figure saved in file clddet.png")
