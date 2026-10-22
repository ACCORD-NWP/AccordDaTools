#-*- coding:utf-8  -*- 
import gc  
import pandas as pd 
import numpy  as np 
from   itertools import repeat 


from   .io_base import DataIO 

 

class SplitDf:
    """
        Class :Split and subset  DF 
    """

    def __init__ (self, df , var , cdtg ,   max_dist =None  , bin_dist =None , time_int=None ):
        self.max_dist  = max_dist     # Maxumum distance for binning in [Km]
        self.bin_int   = bin_dist     # Binning interval in   [Km]
        self.time_int  = time_int     # Time between OmG/OmA  pairs in [+/- min]
        self.dist_df   = df  
        self.var       =  var  
        self.cdtg      = cdtg  
        return None 

    def SubsetDf (self , bin_dist , max_dist ):
        dbin   = [0,1]+list(np.arange(bin_dist, max_dist + bin_dist , bin_dist ))
        dlabel = [0  ]+list(np.arange(bin_dist, max_dist + bin_dist , bin_dist ))
        ldist_bin= pd.cut( self.dist_df ["dist"], bins=dbin,labels=dlabel, right=True,include_lowest=True )
                
        # DIVIDE BY DIST INTERVALS
        df_dist    = self.dist_df[self.dist_df ["dist"] <= max_dist  ]
        df_dist ["ldist"] = ldist_bin.astype("int32")

        # Prepare  Desroziers /H.L statistics  (departres cross product )
        stat_df = df_dist.assign(
        AFGsqr  = df_dist.OA1 * df_dist.FG2,
        FGsqr   = df_dist.FG1 * df_dist.FG2,
        FGsqr1  = df_dist.FG1 * df_dist.FG1,
        FGsqr2  = df_dist.FG2 * df_dist.FG2,
        Asqr1   = df_dist.OA1 * df_dist.OA1,
        A1F1    = df_dist.OA1 * df_dist.FG1    )

        # Binning 
        spdf      = ( stat_df.groupby("ldist", observed=True).agg(
                    Asum1     = ("OA1"   , "sum"), 
                    FGsum1    = ("FG1"   , "sum"),
                    FGsum2    = ("FG2"   , "sum"),
                    AFGsqr    = ("AFGsqr", "sum"),
                    FGsqr     = ("FGsqr" , "sum"),
                    num       = ("dist"  , "count"),
                    FGsqr1    = ("FGsqr1", "sum"),
                    FGsqr2    = ("FGsqr2", "sum"),
                    Asqr1     = ("Asqr1" , "sum")   ).reset_index()  ).round(6) 

        # Add var and date 
        spdf["var" ]=[ self.var   for v in range(len( spdf["num"] ) ) ]
        spdf["date"]=[ pd.to_datetime( str(self.cdtg),    format="%Y%m%d%H",    errors="raise") for v in range(len( spdf["num"] ) ) ] 
        return spdf 


class ConcatDf:
    """
    Class :Concat DF 
           Returns a concatenated dataframes for one variable and the whole period 
    """
    
    def __init__(self ):
        return None 

    def ConcatFromListe (self, sub_df  ):
        merged_dict={}
        merged_df  =pd.DataFrame ()
        for k , v in  sub_df.items():
            if len(v) !=0 :
               merged_df    =pd.concat ( v ) 
               merged_dict[k]=merged_df.reset_index().drop(columns =["index"])            
        return merged_dict


class GroupDf:
    """
    Class : Group DF  and produce the pre-stat for cov, cor and sigma computation 
    """
    def __init__(self  ):
        return None 

    def GroupByBins(self, merged_df, max_dist, bin_int):
        # Set dist and max dist 
        self.max_dist  = max_dist
        self.bin_int   = bin_int
    
        # Binning at the middles of the intervals 
        d_bins  = np.arange(0, max_dist + bin_int, bin_int)
        d_label = np.arange(bin_int/2, max_dist, bin_int)
        merged_df["DIST"] = pd.cut(merged_df["ldist"], bins=d_bins, labels=d_label, include_lowest=False )

        # Group/sum  by dist bins
        g = merged_df.groupby("DIST", observed=True)
        d1   = g["Asum1" ].sum()
        d2   = g["FGsum1"].sum()
        d3   = g["FGsum2"].sum()
        d4   = g["AFGsqr"].sum()
        d5   = g["FGsqr" ].sum()
        d6   = g["FGsqr1"].sum()
        d7   = g["FGsqr2"].sum()
        d8   = g["Asqr1" ].sum()
        dobs = g["num"   ].sum()

        # Get  var and period 
        var = merged_df["var" ].iloc[ 0]
        dt1 = merged_df["date"].iloc[ 0]
        dt2 = merged_df["date"].iloc[-1]
        return d1, d2, d3, d4, d5, d6, d7, d8, dobs, d1.index, dt1, dt2, var

