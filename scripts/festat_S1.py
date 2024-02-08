#
import os
from datetime import datetime, timedelta

#---------INPUT STAFF-----------
EXP    = "SURF_CARRA2_CY46"                            # Change the experiment name here
dtg    = datetime.strptime('2022011500', '%Y%m%d%H')   # Change Start date
dtgend = datetime.strptime('2022021218', '%Y%m%d%H')   # Change End date
#--------------------------------
cycl =  timedelta(hours=6)                             # Assimilation cycle at 00 06 12 and 18 UTC
SCRATCH = os.environ['SCRATCH']
HM_DATA = os.path.join(SCRATCH,'hm_home',EXP,'archive')
TMPOUT  = os.path.join(SCRATCH,'tmp_festat')           # Working directory 'tmp_festat' 
os.makedirs(TMPOUT, mode=0o755, exist_ok=True)
os.chdir(TMPOUT)
ens_mem=range(1,10)                                    # Ensemble member (ens1,ens2.....,ens9)
filename = "ICMSHHARM+0006"                            # 6 hour forecast FA file
fnumber=1
##
while dtg <= dtgend:
    #
    YY = str(dtg.year).ljust(4, '0')
    MM = str(dtg.month).zfill(2)
    DD = str(dtg.day).zfill(2)
    HH = str(dtg.hour).zfill(2)
    # 
    for mm in ens_mem:
        mem='mbr00'.ljust(6,str(mm))
        target = os.path.join(HM_DATA, YY, MM, DD, HH, mem, filename)
        filelink = filename[:-4] + str(fnumber).zfill(4)
        if os.path.isfile(str(filelink)):
              os.remove(str(filelink))
        os.symlink(str(target),str(filelink))
        print(str(filelink),str(target))
        fnumber += 1
    dtg += cycl
#
print(TMPOUT)
quit()
