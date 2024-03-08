# -*- coding: utf-8 -*-
"""
This package is a "pure python" re-implementation of an existing tool ( tuneBR )
which has been already developed by the RC-LACE colleagues
Benedikt Strajnar &  Gergely Boloni ( shared on RC-LACE forum 12/2010 )
It was a mixed of bash/fortran code which is "translated" to python classes (OOP approach)

It's an  "easy and quick" application of a' posteriori diagnosis and tuning
for B and R variances and based on the method of "covariance of residuals in observation space"
proposed by Desroziers et al (2005), see below:
https://rmets.onlinelibrary.wiley.com/doi/pdf/10.1256/qj.05.108

@__AUTHOR :Idir DEHMOUS  
@__E-MAIL : idehmous@meteo.be  
@__RMI    : Royal Meteorological Institute 
@__LAST MODIFICATION: 20/10/2023 
"""

import os
import sys  
sys.path.append("./modules")
import configparser
from   datetime            import  datetime ,timedelta 
from   statistics          import  mean 
from   modules.sigma_bo    import  Predef, Diag , Ratios 
from   modules.setting_env import  TuneEnv 
from   modules.odb         import  Odb
import gsacov 


StartTime = datetime.now()

# GET CONFIG FILE AS ARGUMENT 
nargv = len(sys.argv)
if nargv > 1 :
  ini_file = sys.argv[1]
  if not os.path.exists(ini_file) :
    print("File " + ini_file + " not found.")
    exit(1)
else :
  print("You need to provide the config.ini file!\n")
  print("Usage:")
  print("> python tuneBR.py  config.ini\n")
  exit(1)



# PARSE config FILE    
config=configparser.ConfigParser()
# ALL ITEMS IN UPPER CASE 
config.optionxform = str
config.read(ini_file)

# INIT ENV 
env = TuneEnv ( config  )
PathDict, ModelDict  = env.__Dicts__()
bdate    =env.BeginDate 
edate    =env.EndDate 
cycle_inc=env.cycle_inc  


# INIT GSA COV FORTRAN ROUTINE VARIABLES 
statfile=env.stabal
nlev    =env.nflev
nsmax   =env.nsmax
deltax  =env.deltax
lverb   =env.llverb
lwrite  =env.lwrite

# GET BACKGROUND STANDARD DEVIATIONS (PROFILES &  MEANS)
g=gsacov.GSA(PathDict , statfile ,nsmax ,nlev , deltax , lverb ,lwrite )
tsig_ver ,  sb_pred_t  = g.GetSigmaB (2)    # TEMPERATURE  KPAR=2
qsig_ver ,  sb_pred_q  = g.GetSigmaB (3)    # SPECIFIC HUM KPAR=3
vsig_ver ,  sb_pred_v  = g.GetSigmaB (4)    # VORTICITY    KPAR=4
dsig_ver ,  sb_pred_d  = g.GetSigmaB (5)    # DIVERGENCE   KPAR=5
kesig_ver,  sb_pred_ke = g.GetSigmaB (999)  # UV COMPONENT NOT IN stabal FILE, SET ARBITRARY NUMBER 999
print( "FINISHED EXTRACTION OF SIGMA_B VALUES !" +"\n")


# CREATE DATE TIME LIST 
cdtg=[]
bdate =datetime.strptime( bdate , "%Y%m%d%H")
edate =datetime.strptime( edate , "%Y%m%d%H")
delta =timedelta(hours=int(cycle_inc))
while bdate <= edate:
      strdate=bdate.strftime("%Y%m%d%H")
      cdtg.append( strdate )
      bdate += delta 

# ODB EXTRACTION 
print( "PROCEED TO ODB EXTRACTION ..." +"\n")
db=Odb ( PathDict )

# SPLIT DATE LIST INTO PARALLEL JOBS 
nslice=env.njobs
db.DispatchJobs( cdtg, nslice )

# PREDEFINED SIGMA_O  
print("COMPUTE PREDEFINED SIGMA_O ...!" +"\n")
so_pred_t =Predef ( PathDict , cdtg , lverb , lwrite).GetSigmaP ("t" )
so_pred_bt=Predef ( PathDict , cdtg , lverb , lwrite).GetSigmaP ("bt") 
so_pred_q =Predef ( PathDict , cdtg , lverb , lwrite).GetSigmaP ("q" )
so_pred_ke=Predef ( PathDict , cdtg , lverb , lwrite).GetSigmaP ("ke") 

# COMPUTE SIGMA_O AND SIGMA_B DIAGNOSTICS 
print("COMPUTE SIGMA_O, SIGMA_B DIAGS ...!"+"\n")
sb_diag_t  , so_diag_t ,  pt =Diag(PathDict ,cdtg, lverb , lwrite).GetSigmaD("t" )
sb_diag_bt , so_diag_bt,  pbt=Diag(PathDict ,cdtg, lverb , lwrite).GetSigmaD("bt")
sb_diag_q  , so_diag_q ,  pq =Diag(PathDict ,cdtg, lverb , lwrite).GetSigmaD("q" )
sb_diag_ke , so_diag_ke,  pke=Diag(PathDict ,cdtg, lverb , lwrite).GetSigmaD("ke")

# USE THE SAME VAR NOTATION AS IN RC-LACE FORTRAN CODE
sb_pred=[sb_pred_t,sb_pred_q,sb_pred_ke]               # PREDEFINED Sb (sb_bt PREDEFINED DOESN'T EXIST FOR BRIGHTNESS T)
so_pred=[so_pred_t,so_pred_bt,so_pred_q,so_pred_ke]    #    //      So
sb_diag=[sb_diag_t,sb_diag_bt,sb_diag_q,sb_diag_ke]    # DIAGNOSED  Sb 
so_diag=[so_diag_t,so_diag_bt,so_diag_q,so_diag_ke]    #    //      So

# TOTAL N OBS (DEVIDE pke/2 TO GET N OBS WIND )
Nobs =[ pt , pbt , pq , pke/2. ]

# OBS MEAN 
Mobs =int(sum(Nobs)/len(Nobs))

# INIT RATIO OBJECT WITH CORRESPONDING PREDEF AND DIAG LISTS 
rednmc=env.rednmc
r=Ratios(PathDict, Nobs, rednmc , so_pred , so_diag , sb_pred  , sb_diag ,lwrite)

# GET RATIOS
rot  , robt  , roq  , roke  ,roav = r.RatioSo()   # SIGMAO
rbt  , rbq   , rbke , rbav        = r.RatioSb()   # SIGMAB

# PRINT ON THE SCREEN 
print( 60*"-" +"\n"+    \
      "Var     |      cases      |    Ratio_o    |    Ratio_b".center(50 , ' ') ,"|" \
                +"\n"+  \
       60*"-"   +"\n"+  \
       "t       |" ,str(pt ).center(15,' '),str(round(rot ,5)).center(15,' '),str(round(rbt,5)).center(15,' ') \
                +"\n" + \
       "bt      |" ,str(pbt).center(15,' '),str(round(robt,5)).center(15,' '),"None".center(15,' ')            \
                +"\n"+  \
       "q       |" ,str(pq ).center(15,' '),str(round(roq ,5)).center(15,' '),str(round(rbq,5)).center(15,' ') \
                +"\n"+  \
       "ke      |" ,str(pke).center(15,' '),str(round(roke,5)).center(15,' '),str(round(rbke,5)).center(15,' ')\
                +"\n"+  \
       60*"-"   +"\n"+  \
       "Mean    |",str(Mobs).center(15,' '),str(round(roav,5)).center(15,' '),str(round(rbav,5)).center(15,' ')\
                +"\n"+ 60*"-")
if lverb == True:
   print("\n"+"Input/output files are written in "+os.getenv("PWD")+"/out")

print( " ")
EndTime = datetime.now()
Duration=EndTime - StartTime
print( " SCRIPT RUN TIME : \n" , Duration )
# END 
quit()

