#
import os
from datetime import datetime, timedelta

#---------INPUT NEEDED-----------
EXP    = "SURF_CARRA2_CY46"              # Change the experiment name here
dtgbeg =  datetime(2022, 1, 15, 00)       # Change Start date 
dtgend =  datetime(2022, 2, 12, 18)       # Change End date
#--------------------------------
#
cycl =  timedelta(hours=6)                # Assimilation cycle at 00 06 12 and 18 UTC
SCRATCH = os.environ['SCRATCH']
HM_DATA = os.path.join(SCRATCH,'hm_home',EXP,'archive')
TMPOUT  = os.path.join(SCRATCH,'tmp_festat') # Working directory 'tmp_festat' 
os.makedirs(TMPOUT, mode=0o755, exist_ok=True)
os.chdir(TMPOUT)
ens_mem=range(1,10)                       # Ensemble member (ens1,ens2.....,ens9)
filename = "ICMSHHARM+0006"               # 6 hour forecast FA file
fnumber=1
#
while dtgbeg <= dtgend:
    YY=dtgbeg.isoformat()[0:4]
    MM=dtgbeg.isoformat()[5:7]
    DD=dtgbeg.isoformat()[8:10]
    HH=dtgbeg.isoformat()[11:13]
    for mm in ens_mem:
        mem='mbr00'.ljust(6,str(mm))
        target = os.path.join(HM_DATA, YY, MM, DD, HH, mem, filename)
        print(target)
        #
        if   ( fnumber <= 9 ):
            filelink = filename[:-1]+str(fnumber)
        elif ( fnumber >= 10 and fnumber <= 99 ):
            filelink = filename[:-2]+str(fnumber)
        elif ( fnumber >= 100 and fnumber <= 999 ):
            filelink = filename[:-3]+str(fnumber)
        else:
            break
        #
        if os.path.isfile(filelink):
           os.remove(filelink)
        os.symlink(target ,filelink)
        fnumber += 1

    dtgbeg += cycl
#
print(TMPOUT)
quit()
