import sys
import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter, FixedLocator

ref=sys.argv[1]
exp=sys.argv[2]
ddir=sys.argv[3]
out=sys.argv[4]


obtype=['tempu','tempv','tempuv','tempt','tempq','tempz','temprh',
        'airepu','airepv','airepuv','airept',
        'tpau','tpav','tpauv','tat',
        'radarrh','radarwind',
        'amsua','mhs','mwhs2',
        'iasiwv','iasilw','iasilw_wn',
        'surface','atms',
        'criswv','crislw','crislw_wn']

plottype=['pressure_log','pressure_log','pressure_log','pressure_log','pressure','pressure_log','pressure',
          'pressure','pressure','pressure','pressure',
          'pressure_log','pressure_log','pressure_log','pressure_log',
          'pressure','pressure',
          'channel','channel','channel',
          'ppressure','ppressure_log','lwwn',
          'surface','channel2',
          'ppressure','ppressure_log','lwwn']

titles=['TEMP U','TEMP V','TEMP U/V','TEMP T','TEMP Q','TEMP Z','TEMP RH',
        'AIREP U','AIREP V','AIREP U/V','AIREP T',
        'TEMP/AIREP U','TEMP/AIREP V','TEMP/AIREP U/V','TEMP/AIREP T',
        'Radar RH', 'Radar Wind',
        'AMSU-A','MHS','MWHS2',
        'IASI WV','IASI LW','IASI LW channel index space',
        'SURFACE','ATMS',
        'CrIS WV','CrIS LW','CrIS LW channel index space']

ylabels=['Pressure [hPa]','Pressure [hPa]','Pressure [hPa]','Pressure [hPa]','Pressure [hPa]','Pressure [hPa]','Pressure [hPa]',
        'Pressure [hPa]','Pressure [hPa]','Pressure [hPa]','Pressure [hPa]',
        'Pressure [hPa]','Pressure [hPa]','Pressure [hPa]','Pressure [hPa]',
        'Pressure [hPa]','Pressure [hPa]',
        'Channel index','Channel index','Channel index',
        'Peak pressure [hPa]','Peak pressure [hPa]','Channel',
        '','Channel index',
        'Peak pressure [hPa]','Peak pressure [hPa]','Channel']
         
xlabels=['Mean','Mean','Std','Std','Norm Std','Norm Std']
subtitles=['OmA Mean', 'OmB Mean', 'OmA St.Dev.', 'OmB St.Dev.', 'Control-normalized OmA St.Dev', 'Control-normalized OmB St.Dev']

limdefmean=[{'xmin':-0.0014, 'xmax':0.0014, 'xticks':[-0.001,0.001], 'dx':0.0002},
            {'xmin':-0.005, 'xmax':0.005, 'xticks':[-0.003,0.003], 'dx':0.001},
            {'xmin':-0.014, 'xmax':0.014, 'xticks':[-0.01,0.01], 'dx':0.002},
            {'xmin':-0.05, 'xmax':0.05, 'xticks':[-0.03,0.03], 'dx':0.01},
            {'xmin':-0.14, 'xmax':0.14, 'xticks':[-0.1,0.1], 'dx':0.02},
            {'xmin':-0.5, 'xmax':0.5, 'xticks':[-0.3,0.3], 'dx':0.1},
            {'xmin':-1.4, 'xmax':1.4, 'xticks':[-1.,1.], 'dx':0.2},
            {'xmin':-5., 'xmax':5., 'xticks':[-3.,3.], 'dx':1.},
            {'xmin':-14., 'xmax':14., 'xticks':[-10.,10.], 'dx':2.}]

limdefstd=[{'xmin':0, 'xmax':0.00012, 'xticks':[0,0.0001], 'dx':0.00002},
           {'xmin':0, 'xmax':0.0005, 'xticks':[0,0.0002,0.0004], 'dx':0.0001},
           {'xmin':0, 'xmax':0.0012, 'xticks':[0,0.001], 'dx':0.0002},
           {'xmin':0, 'xmax':0.005, 'xticks':[0,0.002,0.004], 'dx':0.001},
           {'xmin':0, 'xmax':0.012, 'xticks':[0,0.01], 'dx':0.002},
           {'xmin':0, 'xmax':0.05, 'xticks':[0,0.02,0.04], 'dx':0.01},
           {'xmin':0, 'xmax':0.12, 'xticks':[0,0.1], 'dx':0.02},
           {'xmin':0, 'xmax':0.5, 'xticks':[0,0.2,0.4], 'dx':0.1},
           {'xmin':0, 'xmax':1.2, 'xticks':[0,1.], 'dx':0.2},
           {'xmin':0, 'xmax':5., 'xticks':[0,2.,4.], 'dx':1.},
           {'xmin':0, 'xmax':12, 'xticks':[0,10.], 'dx':2.}]

limdefnorm=[]

def deflim_mean(xval1,xval2):
    lims=np.array([0.0014, 0.005, 0.014, 0.05, 0.14, 0.5, 1.4, 5., 14.])
    if ((np.abs(xval1[0]) > 2.2*np.max(np.abs(xval1[1:]))) or (np.abs(xval2[0]) > 2.2*np.max(np.abs(xval2[1:])))) and (len(xval1)>5) and (len(xval2)>5):
        xext = np.max(np.abs(np.concatenate((xval1[1:],xval2[1:]), axis=None)))
    else:
        xext = np.max(np.abs(np.concatenate((xval1,xval2), axis=None)))
    
    if (len(lims[lims>xext])==0):
        x = -1
    else:
        x = np.where(lims == lims[lims>xext][0])[0][0] #find nearest x-axis limit value ge xext
    
    return limdefmean[x]

def deflim_std(xval1,xval2):
    lims=np.array([0.00012, 0.0005, 0.0012, 0.005, 0.012, 0.05, 0.12, 0.5, 1.2, 5.])
    xext = np.max(np.concatenate((xval1[1:],xval2[1:]), axis=None))
    if (len(lims[lims>xext])==0):
        x = -1
    else:
        x = np.where(lims == lims[lims>xext][0])[0][0] #find nearest x-axis limit value ge xext
    return limdefstd[x]

def deflim_norm(xval,left,right):
    ticklims=np.array([0.4,0.5,0.6,0.7,0.8,0.85,0.9,0.95,0.96,0.97,0.98,0.99,0.995,0.999])
    lims=np.array([0.,0.,0.3,0.6,0.65,0.75,0.86,0.9,0.93,0.95,0.97,0.98,0.99,0.998])
    dx=np.array([0.2,0.1,0.1,0.1,0.05,0.05,0.02,0.01,0.01,0.01,0.01,0.002,0.001,0.0002])
    xext = np.max(np.abs(1-np.concatenate(((xval+left)[1:],(xval+right)[1:]), axis=None)))
    
    if (len(ticklims[ticklims>xext])==0):
        x = 0
    else:
        x = np.where(ticklims == ticklims[ticklims>(1-xext)][0])[0][0]
    xticks = [ticklims[x],2-ticklims[x]]
    
    return {'xmin':lims[x], 'xmax':2-lims[x], 'xticks':xticks, 'dx':dx[x]}
    
    
           
for i in range(len(obtype)):
    fig, axs = plt.subplots(3, 2, figsize=(8.27,11.69), constrained_layout=True)
    fig.suptitle(titles[i], fontsize='xx-large')
    
    fin=[ddir+ref+'_'+obtype[i]+'_oma_mean', ddir+exp+'_'+obtype[i]+'_oma_mean',
         ddir+ref+'_'+obtype[i]+'_omb_mean', ddir+exp+'_'+obtype[i]+'_omb_mean',
         ddir+ref+'_'+obtype[i]+'_oma_sdev', ddir+exp+'_'+obtype[i]+'_oma_sdev',
         ddir+ref+'_'+obtype[i]+'_omb_sdev', ddir+exp+'_'+obtype[i]+'_omb_sdev',
         ddir+exp+'_'+ref+'_'+obtype[i]+'_oma_norm',
         ddir+exp+'_'+ref+'_'+obtype[i]+'_omb_norm']
    
    for j,ax in enumerate(axs.flat):
        if j<4: #plots nr 1-4
            datain1=np.loadtxt(fin[2*j]); datain2=np.loadtxt(fin[2*j+1])
            
            if len(datain1)!=0 and len(datain2)!=0: #if datafile is NOT empty
                profile1=datain1[:,0]; vlev=datain1[:,1]; profile2=datain2[:,0]
            
                if j<2: #mean plots
                    ax.axvline(x = 0, color = 'k', linestyle = '--', linewidth=0.7)
                    xdict = deflim_mean(profile1,profile2)
                    ax.set_xlim(xdict['xmin'],xdict['xmax'])
                    ax.set_xticks(xdict['xticks'])
                    ax.set_xticks(np.arange(xdict['xmin'], xdict['xmax'], xdict['dx']), minor=True)
                elif j>=2: #std plots
                    xdict = deflim_std(profile1,profile2)
                    for l in range(1,len(xdict['xticks'])):
                        ax.axvline(x = xdict['xticks'][l], color = 'k', linestyle = '--', linewidth=0.7)
                    ax.set_xlim(xdict['xmin'],xdict['xmax'])
                    ax.set_xticks(xdict['xticks'])
                    ax.set_xticks(np.arange(xdict['xmin'], xdict['xmax'], xdict['dx']), minor=True)
                
                if plottype[i]=='pressure_log' or plottype[i]=='ppressure_log':
                    ax.semilogy(profile2,vlev,color='red', label=exp)
                    ax.semilogy(profile1,vlev,color='blue', label=ref)
                    ax.yaxis.set_major_formatter(ScalarFormatter())
                    ax.yaxis.set_major_locator(FixedLocator([10.,30.,100.,300.,1000.]))
                    ax.set_ylim(top=1000,bottom=4)
                    ax.legend(loc='upper left', frameon=False)
                    ax.invert_yaxis()
                elif plottype[i]=='pressure' or plottype[i]=='ppressure':
                    ax.plot(profile2,vlev,color='red', label=exp)
                    ax.plot(profile1,vlev,color='blue', label=ref)
                    ax.set_ylim(top=1000, bottom=0)
                    ax.set_yticks([0.,200.,400.,600.,800.,1000.])
                    ax.legend(loc='upper left', frameon=False)
                    ax.invert_yaxis()
                elif plottype[i]=='surface':
                    ax.plot(profile2,vlev,color='red', label=exp)
                    ax.plot(profile1,vlev,color='blue', label=ref)
                    ax.set_ylim(top=8, bottom=1)
                    ax.set_yticks([1.,2.,3.,4.,5.,6.,7.,8.])
                    ax.set_yticklabels(['In-situ Zs','In-situ U/V10m','APD','In-Situ RH2m','In-situ Q2m','In-situ T2m','Skin T','ASCAT U/V'])
                    ax.legend(loc='upper left', frameon=False)
                elif plottype[i]=='lwwn':
                    ax.plot(profile2,vlev,color='red', label=exp, linewidth=0.7)
                    ax.plot(profile1,vlev,color='blue', label=ref, linewidth=0.7)
                    ax.set_ylim(top=4500, bottom=0)
                    ax.set_yticks([0.,500.,1000.,1500.,2000.,2500.,3000.,3500.,4000.])
                    ax.legend(loc='upper left', frameon=False)
                elif plottype[i]=='channel':
                    ax.plot(profile2,vlev,color='red', label=exp)
                    ax.plot(profile1,vlev,color='blue', label=ref)
                    ax.set_ylim(top=np.max(vlev)+0.5, bottom=np.min(vlev)-0.5)
                    ax.set_yticks(np.arange(np.min(vlev),np.max(vlev)+1,1))
                    ax.legend(loc='upper left', frameon=False)
                elif plottype[i]=='channel2':
                    pos = np.where(np.diff(vlev) > 1)[0]                                            #find the discontinuity spot
                    dy = np.diff(vlev)[0]                                                           #find difference between levels
                    add = np.arange(float(vlev[pos])+1, float(vlev[pos+1]), dy)                     #add missing levels
                    addnan = np.empty(len(add))
                    addnan[:] = np.nan
                    newvlev = np.insert(vlev, pos+1, add)
                    newprofile1 =  np.insert(profile1, pos+1, addnan)                                    #apply nan to missing levels to produce the
                    newprofile2 =  np.insert(profile2, pos+1, addnan)
                    ax.plot(newprofile2,newvlev,color='red', label=exp)
                    ax.plot(newprofile1,newvlev,color='blue', label=ref)
                    ax.set_ylim(top=np.max(newvlev)+0.5, bottom=np.min(newvlev)-0.5)
                    ax.set_yticks(np.arange(np.min(newvlev),np.max(newvlev)+1,1))
                    
                    
            else: #if datafile is empty
                if j<2:
                    ax.axvline(x = 0, color = 'k', linestyle = '--', linewidth=0.7)
                    xdict=limdefmean[-1]
                    ax.set_xlim(xdict['xmin'],xdict['xmax'])
                    ax.set_xticks(xdict['xticks'])
                    ax.set_xticks(np.arange(xdict['xmin'], xdict['xmax'], xdict['dx']), minor=True)
                elif j>=2:
                    xdict=limdefstd[-1]
                    for l in range(1,len(xdict['xticks'])):
                        ax.axvline(x = xdict['xticks'][l], color = 'k', linestyle = '--', linewidth=0.7)
                    ax.set_xlim(xdict['xmin'],xdict['xmax'])
                    ax.set_xticks(xdict['xticks'])
                    ax.set_xticks(np.arange(xdict['xmin'], xdict['xmax'], xdict['dx']), minor=True)
                
                if plottype[i]=='pressure_log' or plottype[i]=='ppressure_log':
                    ax.set_yscale('log')
                    ax.yaxis.set_major_formatter(ScalarFormatter())
                    ax.yaxis.set_major_locator(FixedLocator([10.,30.,100.,300.,1000.]))
                    ax.set_ylim(top=1000,bottom=4)
                    ax.invert_yaxis()
                elif plottype[i]=='pressure' or plottype[i]=='ppressure':
                    ax.set_ylim(top=1000, bottom=0)
                    ax.set_yticks([0.,200.,400.,600.,800.,1000.])
                    ax.legend(loc='upper left', frameon=False)
                    ax.invert_yaxis()
                elif plottype[i]=='surface':
                    ax.set_yticks([1,2,3,4,5,6,7,8])
                    ax.set_yticklabels(['In-situ Zs','In-situ U/V10m','APD','In-Situ RH2m','In-situ Q2m','In-situ T2m','Skin T','ASCAT U/V'])
                elif plottype[i]=='lwwn':
                    ax.set_ylim(top=4500, bottom=0)
                    ax.set_yticks([0.,500.,1000.,1500.,2000.,2500.,3000.,3500.,4000.])
                elif plottype[i]=='channel' or plottype[i]=='channel2':
                    if obtype[i]=='mhs':
                        nchan=5 #number of channels
                    elif obtype[i]=='mwhs2' or obtype[i]=='amsua':
                        nchan=15
                    elif obtype[i]=='atms':
                        nchan=22
                    ax.set_ylim(top=0.5, bottom=nchan+0.5)
                    ax.set_yticks(np.arange(1,nchan+1,1))
                    
            
        else: #plots nr 4-5
            datain=np.loadtxt(fin[2*j-j%2])
            if len(datain)!=0: #if datafile is NOT empty
                profile=datain[:,0]; vlev=datain[:,1]
                lconf=datain[:,2]; rconf=datain[:,3]
                ax.axvline(x = 1, color = 'k', linestyle = '--', linewidth=1)
                xdict = deflim_norm(profile,lconf,rconf)
                ax.set_xlim(xdict['xmin'],xdict['xmax'])
                ax.set_xticks(xdict['xticks'])
                ax.set_xticks(np.arange(xdict['xmin'], xdict['xmax'], xdict['dx']), minor=True)
                if plottype[i]=='pressure_log' or plottype[i]=='ppressure_log':
                    ax.semilogy(profile, vlev, color='r',linewidth=2)
                    ax.hlines(y = vlev, xmin=profile+lconf, xmax=profile+rconf, color = 'red', linewidth=2)
                    ax.yaxis.set_major_formatter(ScalarFormatter())
                    ax.yaxis.set_major_locator(FixedLocator([10.,30.,100.,300.,1000.]))
                    ax.set_ylim(top=1050,bottom=4)
                    ax.invert_yaxis()
                elif plottype[i]=='pressure' or plottype[i]=='ppressure':
                    ax.plot(profile, vlev, color='r',linewidth=2)
                    ax.hlines(y = vlev, xmin=profile+lconf, xmax=profile+rconf, color = 'red', linewidth=2)
                    ax.set_ylim(top=1050, bottom=0)
                    ax.set_yticks([0.,200.,400.,600.,800.,1000.])
                    ax.invert_yaxis()
                elif plottype[i]=='surface':
                    ax.plot(profile, vlev, color='r',linewidth=2)
                    ax.hlines(y = vlev, xmin=profile+lconf, xmax=profile+rconf, color = 'red', linewidth=2)
                    ax.set_ylim(top=8, bottom=1)
                    ax.set_yticks([1,2,3,4,5,6,7,8])
                    ax.set_yticklabels(['In-situ Zs','In-situ U/V10m','APD','In-Situ RH2m','In-situ Q2m','In-situ T2m','Skin T','ASCAT U/V'])
                elif plottype[i]=='lwwn':
                    ax.plot(profile, vlev, color='r',linewidth=0.7)
                    ax.hlines(y = vlev, xmin=profile+lconf, xmax=profile+rconf, color = 'red', linewidth=0.7)
                    ax.set_ylim(top=4500, bottom=0)
                    ax.set_yticks([0.,500.,1000.,1500.,2000.,2500.,3000.,3500.,4000.])
                elif plottype[i]=='channel':
                    ax.plot(profile, vlev, color='r')
                    ax.hlines(y = vlev, xmin=profile+lconf, xmax=profile+rconf, color = 'red')
                    ax.set_ylim(top=np.max(vlev)+0.5, bottom=np.min(vlev)-0.5)
                    ax.set_yticks(np.arange(np.min(vlev),np.max(vlev)+1,1))
                elif plottype[i]=='channel2':
                    pos = np.where(np.diff(vlev) > 1)[0]                                            #find the discontinuity spot
                    dy = np.diff(vlev)[0]                                                           #find difference between levels
                    add = np.arange(float(vlev[pos])+1, float(vlev[pos+1]), dy)                     #add missing levels
                    addnan = np.empty(len(add))
                    addnan[:] = np.nan
                    newvlev = np.insert(vlev, pos+1, add)
                    newprofile =  np.insert(profile, pos+1, addnan)                                    #apply nan to missing levels to produce the
                    newlconf =  np.insert(lconf, pos+1, addnan)                                        #discontinuity in the profile plot
                    newrconf =  np.insert(rconf, pos+1, addnan)
                    ax.plot(newprofile, newvlev, color='r')
                    ax.hlines(y = newvlev, xmin=newprofile+newlconf, 
                              xmax=newprofile+newrconf, color = 'red')
                    ax.set_ylim(top=np.max(newvlev)+0.5, bottom=np.min(newvlev)-0.5)
                    ax.set_yticks(np.arange(np.min(newvlev),np.max(newvlev)+1,1))
            
            else: #if datafile is empty
                ax.axvline(x = 1, color = 'k', linestyle = '--', linewidth=1)
                ax.set_xlim(0.99,2-0.99)
                ax.set_xticks([0.995,2-0.995])
                ax.set_xticks(np.arange(0.99, 2-0.99, 0.001), minor=True)
                if plottype[i]=='pressure_log' or plottype[i]=='ppressure_log':
                    ax.set_yscale('log')
                    ax.yaxis.set_major_formatter(ScalarFormatter())
                    ax.yaxis.set_major_locator(FixedLocator([10.,30.,100.,300.,1000.]))
                    ax.set_ylim(top=1050,bottom=4)
                    ax.invert_yaxis()
                elif plottype[i]=='pressure' or plottype[i]=='ppressure':
                    ax.set_ylim(top=1050, bottom=0)
                    ax.set_yticks([0.,200.,400.,600.,800.,1000.])
                    ax.invert_yaxis()
                elif plottype[i]=='surface':
                    ax.set_yticks([1,2,3,4,5,6,7,8])
                    ax.set_yticklabels(['In-situ Zs','In-situ U/V10m','APD','In-Situ RH2m','In-situ Q2m','In-situ T2m','Skin T','ASCAT U/V'])
                elif plottype[i]=='lwwn':
                    ax.set_ylim(top=4500, bottom=0)
                    ax.set_yticks([0.,500.,1000.,1500.,2000.,2500.,3000.,3500.,4000.])
                elif plottype[i]=='channel' or plottype[i]=='channel2':
                    if obtype[i]=='mhs':
                        nchan=5 #number of channels
                    elif obtype[i]=='mwhs2' or obtype[i]=='amsua':
                        nchan=15
                    elif obtype[i]=='atms':
                        nchan=22
                    ax.set_ylim(top=0.5, bottom=nchan+0.5)
                    ax.set_yticks(np.arange(1,nchan+1,1))
            
        ax.set(ylabel=ylabels[i], xlabel=xlabels[j])
        ax.set_title(subtitles[j])
        ax.grid(which='major', axis='both', linestyle=':')
    
    plt.savefig(out+obtype[i]+'.png')
