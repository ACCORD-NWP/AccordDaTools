# -*-coding :utf-8 -*- 
import os , sys  
import pandas as pd
import numpy  as np
from pathlib import Path



class DataIO:
    """
    Class:  DataIO : Contains methods to write and read the statstics dataframes  
                     for each cycle (in .csv files)
                     It uses the format 'feather'. Fast for I/O  Compatible with pandas 
            Methods  : 
                      FlushFrame 
                      ReadFrame 
                     
    """
    def __init__(self, compression="lz4"):
        # If binary format is used , feather is the best for compression 
        # Write frames into feather format file with  either "lz4" or "zstd" compression          
        # We use  ".csv"  for the moment 
        self.compression = compression  
        if self.compression not in  ["ls4", "zstd"]:
           self.compression = "ls4"    # Fallback to default

    def FlushFrame(self, df,  subdir, cdtg, var, fid=None, verbose =None, filetype=None ):
        """
        Flush a DataFrame to Feather format with lz4 compression algo'.
        Naming based on var, cdtg
        """
        vrb = verbose 
        if df is None or df.empty:
            print("Warning : Empty DataFrame for var={} datatime={}, nothing written".format( var , cdtg  ) )
            return None

        # Final directory:  dbpath / subdir / cdtg
        outdir = Path(subdir) / cdtg
        outdir.mkdir(parents=True, exist_ok=True)
        if fid is not None:
            filename = "_".join(  (var,cdtg,fid ) ) +".csv"
        else:
            filename = "_".join(  (var,cdtg))   +".csv"
        
        # Dir+Filename 
        filepath = "/".join(  (str(outdir) , str(filetype) +"_"+str(filename)   )  )
        try:
            df.to_csv(filepath, index = False , sep=",")
            if vrb in  [3]:
               print( "The dataframe has been written into {} for var: {} , datetime: {}".format(os.path.basename(filepath) , var , cdtg    ))
        except Exception:
               print( "ERROR writing dataframe : var: {} and datetime: {}".format( var , cdtg    ))
               return None
        return str(filepath)





    def ReadFrame(self, filepaths , verbose =None ):
        """
        Read files.  If var is provided, filters only matching files.
        Returns a single concatenated DataFrame.
        """
        vrb    = verbose 
        frames = []
        if isinstance( filepaths, str ):           
           df = pd.read_csv( filepaths ,  header='infer', engine='python', sep=",")
           return df  

        elif isinstance (filepaths, list ):
            for fp in filepaths:
                if not os.path.isfile (fp):
                   print("File not found : " , fp  ) 
                   frames.append( pd.DataFrame() )  
                else:
                   df = pd.read_csv( fp ,  header='infer', engine='python', sep=",")
                   if vrb in [3]:
                      print( "Dataframe loaded with : {} rows)".format( df.shape[0] ))
                   frames.append(df)

        if not frames:
            return None

        # Concatenate all chunks
        all_df = pd.concat(frames, axis=0, ignore_index=True)
        print( "Total: {} rows merged".format(  all_df.shape[0] )) 
        return all_df 


