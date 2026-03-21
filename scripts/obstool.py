#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os, sys  
import pandas as pd
from   datetime  import  datetime
from   pathlib   import  Path
import argparse

# odb4py  
from odb4py.utils import SqlParser , OdbObject  

# Obstool modules 
from setting     import Setting 
from utils       import OdbReader , Rows2Df
from conv_stats  import DHLStat


# Insert parent directory to get "modules" directory 
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))



# Simple methods to parse arguments from the command line
def Usage():
    help_= "Description  :\n\
            Diagnostic tool 'obstool'.\n\
            Estimates the spatiale  covariances ,  correlations  and \n\
            standard deviations from the obs-guess and the obs-analysis\n\
            departures.\n\
            The output   :\n\
                 Estimated optimal thinning distance for a given observation\n\
                 type variable.\n\
              \n\
            The implemented approaches are ones proposed by :\n\
            Hollingsworth/Lönnberg (1986) and Desroziers (2005)\n"
    return help_

def ParseArgs():
    # The var list must be given as a liste sperated with ":"
    var=lambda s:s.split(":")    
    parser  = argparse.ArgumentParser( description=" ")

    # Args significations 
    arg1 ="Path to ODB directory.  (default=.)"
    arg2 ="ODB format/type.  (default=CCMA)"
    arg3 ="Begin date. Format YYYYMMDDHH. ( default=1970010100)"
    arg4 ="End   date. Format YYYYMMDDHH. ( default=1970010103)"
    arg5 ="Observation category. (default=conv)"
    arg6 ="List of observation types separated by ':'  (default=airep_t:airep_v)"
    arg7 ="Cycle increment in hours.  (default= 3)"
    arg8 ="Maximum distance for diagnostics in [Km]  (default=100 Km)"
    arg9 ="Bin distance for diagnostics in  [Km]     (default=10  Km)"

    # Required 
    parser.add_argument("-odb_path"    ,required=True ,type=str   ,default="."         ,choices=None    ,help=arg1)
    parser.add_argument("-odb_type"    ,required=True ,type=str   ,default="CCMA"      ,choices=["CCMA"],help=arg2)
    parser.add_argument("-bdate"       ,required=True ,type=str   ,default="1970010100",choices=None    ,help=arg3)
    parser.add_argument("-edate"       ,required=True ,type=str   ,default="1970010103",choices=None    ,help=arg4)
    parser.add_argument("-obs_category",required=True ,type=str   ,default="conv"      ,choices=["conv","satem"], help=arg5)
    parser.add_argument("-var_list"    ,required=True ,type=var   ,default="synop_t:airep_v",choices=None,help=arg6)
    parser.add_argument("-cycle_inc"   ,required=True ,type=int   ,default=3           ,choices=None    ,help=arg7)
    parser.add_argument("-max_dist"    ,required=False,type=float ,default=100.0       ,choices=None    ,help=arg8)
    parser.add_argument("-bin_dist"    ,required=False,type=float ,default=10.0        ,choices=None    ,help=arg9)
    if len(sys.argv) == 1:
       Usage() 
       parser.print_help()
       sys.exit(1)
    return parser.parse_args()


# Start 
start_time = datetime.now()

# Get args  
args= ParseArgs ()   
# Odb 
odbpath= args.odb_path  
odbtype= args.odb_type

# Period  and cycle  
bdate   = args.bdate
edate   = args.edate
cycle_inc=args.cycle_inc

# Set variable list
var_list= []
for v in   args.var_list: var_list.append(  v.lstrip () )

# Max and binning distances 
max_dist= args.max_dist  
bin_dist= args.bin_dist 

# Instantiate    
st = Setting ()
rd = Rows2Df ()
rr = OdbReader(odbpath , odbtype )

# Set period   
period=st.set_period(  bdate, edate  )

# Set and check the var list  
st.set_obs_list(  var_list     )

# Collection of pre-selected frames with distances <= max_dist    
frame_liste  = rr.get_odb_rows (period ,var_list, max_dist ,bin_dist , odbpath ,  cycle_inc,  pbar =True , verbosity =2)

# Concat Df for the final stats 
cdf = rd.DfPrep( frame_liste )

# Time duration of ODB extraction 
ext_time = datetime.now()

# Diff 
extraction_time = ext_time - start_time
print(f"Extraction time : {extraction_time}")

# Get the stats by var
for var  in var_list:
    dhl= DHLStat ( cdf[var] , max_dist = max_dist ,bin_dist = bin_dist   )
    cov= dhl.getCov( var , inplace=False )
    sig= dhl.getSig( var , inplace=False )
    cor= dhl.getCor( var , inplace=False )
    diag=dhl.getStatFrame(var) 
    # Save stats in a csv file 
    diag.to_csv ( "py_"+var+".csv",  index=False , header=True  )


# Finalize 
end_time = datetime.now()

# Time duration of stats computation 
duration  = end_time - ext_time
print(f"Statistics calculation duration  : {duration}")

# Total 
tot_duration  = end_time - start_time        
print(f"Total duration  runtime : {tot_duration}")        
quit()
