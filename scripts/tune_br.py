#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os , sys  
import configparser
import argparse
from   datetime      import  datetime ,timedelta 
from   statistics    import  mean 
from   pathlib       import  Path

# TuneBR modules 
# Insert parent directory to get "modules" directory 
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from modules import Predef, Diag , Ratios
from modules import Odb ,TuneEnv 
from modules import GSA 



# Get args 
parser = argparse.ArgumentParser( description=" ")
parser.add_argument("--config_file" ,required=True ,type=str ,default="../share/tunebr_conf/config.ini",choices=None ,help="Path to tuneBR config file" )

if len(sys.argv) == 1:
   parser.print_help()
   sys.exit(1)

# Check  config filename 
args=parser.parse_args()
ini_file=args.config_file
if not os.path.isfile(ini_file):
  print("The given .ini file {} not found !\n".format(ini_file )) 
  print("Usage:")
  print("> python tune_br.py  config.ini\n")
  sys.exit(1)

# Start 
StartTime = datetime.now()

# GET CONFIG FILE AS ARGUMENT 
"""nargv = len(sys.argv)
if nargv > 1 :
  ini_file = sys.argv[1]
  if not os.path.exists(ini_file) :
    print("File " + ini_file + " not found.")
    exit(1)
else :
  print("You need to provide the config.ini file!\n")
  print("Usage:")
  print("> python tune_br.py  config.ini\n")
  exit(1)"""


# Parse config file  
config=configparser.ConfigParser( 
interpolation=configparser.ExtendedInterpolation()  )

# All items in upper case 
config.optionxform = str
config.read(ini_file)

# Init TuneBR env  
env = TuneEnv ( config  )
PathDict, ModelDict  = env.__Dicts__()
bdate    =env.BeginDate 
edate    =env.EndDate 
cycle_inc=env.cycle_inc  


# Init GSA fortran routine 
statfile =env.stabal
nlev     =env.nflev
nsmax    =env.nsmax
deltax   =env.deltax
lverb    =env.llverb
lwrite   =env.lwrite
outfile  =env.outfile 
prog_bar =env.prog_bar  
ndca_cpu =env.dca_cpu 
rows_path=env.rows_path


# Get background standard deviations  (Profiles +  means ) 
g=GSA(PathDict , statfile ,nsmax ,nlev , deltax , lverb ,lwrite )
tsig_ver ,  sb_pred_t  = g.GetSigmaB (2)    # Temperature   KPAR=2
qsig_ver ,  sb_pred_q  = g.GetSigmaB (3)    # Specific  q   KPAR=3
vsig_ver ,  sb_pred_v  = g.GetSigmaB (4)    # Vorticity     KPAR=4
dsig_ver ,  sb_pred_d  = g.GetSigmaB (5)    # Divergence    KPAR=5
kesig_ver,  sb_pred_ke = g.GetSigmaB (999)  # UV Components not in stabal file --> set arbitrary unique number  999
print("Extraction of SIGMA_B values, done !\n")


# Create datetime list
cdtg=[]
bdate =datetime.strptime( bdate , "%Y%m%d%H")
edate =datetime.strptime( edate , "%Y%m%d%H")
delta =timedelta(hours=int(cycle_inc))
while bdate <= edate:
      strdate=bdate.strftime("%Y%m%d%H")
      cdtg.append( strdate )
      bdate += delta 


# ODB extraction  
print( "Proceed to ODB extraction ...!\n")
db=Odb ( PathDict )
#nslice=env.ntaks     # If in parallel (Later !!)

# Get ODB rows  
db.CreateDca  ( cdtg , ndca_cpu   )
db.OdbExtract ( cdtg ,rows_path ,  prog_bar  )

# Predefined SIGMA_O  
print("Compute predefined SIGMA_O ...!\n")
so_pred_t =Predef ( PathDict , cdtg , lverb , lwrite).GetSigmaP ("t" )
so_pred_bt=Predef ( PathDict , cdtg , lverb , lwrite).GetSigmaP ("bt")
so_pred_q =Predef ( PathDict , cdtg , lverb , lwrite).GetSigmaP ("q" )
so_pred_ke=Predef ( PathDict , cdtg , lverb , lwrite).GetSigmaP ("ke") 


# Compute SIGMA_O AND SIGMA_B  diagnostics 
print("Compute SIGMA_O, SIGMA_B diagnostics ...!\n")
sb_diag_t  , so_diag_t ,  pt =Diag(PathDict ,cdtg, lverb , lwrite).GetSigmaD("t" )
sb_diag_bt , so_diag_bt,  pbt=Diag(PathDict ,cdtg, lverb , lwrite).GetSigmaD("bt")
sb_diag_q  , so_diag_q ,  pq =Diag(PathDict ,cdtg, lverb , lwrite).GetSigmaD("q" )
sb_diag_ke , so_diag_ke,  pke=Diag(PathDict ,cdtg, lverb , lwrite).GetSigmaD("ke")


# Use the same notation as in the RC-LACE version  
sb_pred=[sb_pred_t,sb_pred_q,sb_pred_ke]               # PREDEFINED Sb (sb_bt PREDEFINED DOESN'T EXIST FOR BRIGHTNESS T)
so_pred=[so_pred_t,so_pred_bt,so_pred_q,so_pred_ke]    #    //      So
sb_diag=[sb_diag_t,sb_diag_bt,sb_diag_q,sb_diag_ke]    # DIAGNOSED  Sb 
so_diag=[so_diag_t,so_diag_bt,so_diag_q,so_diag_ke]    #    //      So

# Total wind obs is : nobs ke/2 
nobs =[ pt , pbt , pq , pke/2. ]

# OBS Mean 
nobs_mean =int(sum(nobs)/len(nobs))

# INIT RATIO OBJECT WITH CORRESPONDING PREDEF AND DIAG LISTS 
rednmc=env.rednmc
r=Ratios(PathDict, nobs, rednmc , so_pred , so_diag , sb_pred  , sb_diag ,lwrite)

# GET RATIOS
sigo = r.RatioSo()
sigb = r.RatioSb()

rot  , robt  , roq  , roke,roav = sigo[0],sigo[1],sigo[2],sigo[3],sigo[4]
rbt  , rbq   , rbke , rbav      = sigb[0],sigb[1],sigb[2],sigb[3]

# Output  
text = (
    60*"-" +   "\n"
    + "Var     |      cases      |    Ratio_o    |    Ratio_b".center(50, " ") +                "|   \n"
    + 60*"-" + "\n"
    + f"t       |{str(pt).center(15)}{str(round(rot,5)).center(15)}{str(round(rbt,5)).center(15)}    \n"
    + f"bt      |{str(pbt).center(15)}{str(round(robt,5)).center(15)}{'None'.center(15)}             \n"
    + f"q       |{str(pq).center(15)}{str(round(roq,5)).center(15)}{str(round(rbq,5)).center(15)}    \n"
    + f"ke      |{str(pke).center(15)}{str(round(roke,5)).center(15)}{str(round(rbke,5)).center(15)} \n"
    + 60*"-" + "\n"
    + f"Mean    |{str(nobs_mean).center(15)}{str(round(roav,5)).center(15)}{str(round(rbav,5)).center(15)}\n"
    + 60*"-"
)                

# Simple print on screen  
print( text)  

# Write in file given in config  +  date1_date2 
if lwrite == True :    
   period = str(cdtg[0]) +"_"+ str(cdtg[-1] )
   outfile= "_".join(  ( outfile, period))
   dir_ = os.getcwd()
   print( f"SIGMA B/O written in file:  {dir_}/{outfile} " ) 
   with open( outfile  , "w", encoding="utf-8") as f:
        f.write(text) 

# 
if lverb == True:
   print("\n"+"The different I/O files are written in "+os.getenv("PWD")+"/out")
   print( " ")
   EndTime = datetime.now()
   Duration=EndTime - StartTime
   print( " EXECUTION  TIME : \n" , Duration )   
# END 
quit()

