# -*-coding :utf-8 -*- 
import os , sys  , gc  
import pandas as pd 
import numpy  as np 
from   pathlib import Path  
from   collections import defaultdict  
import multiprocessing as mp  
from   multiprocessing import Pool , cpu_count,  shared_memory 
from   itertools import chain 


"""sys.path.insert(0,"/home/idehmous/Desktop/rmib_dev/github/pyodb_1.1.0/build/lib.linux-x86_64-cpython-39")
from pyodb_extra  import OdbEnv
env= OdbEnv ("/home/idehmous/Desktop/rmib_dev/github/pkg", "libodb.so")
env.InitEnv ()
# pyodb modules 
from pyodb_extra.odb_ob    import  OdbObject  
env= OdbEnv ("/home/idehmous/Desktop/rmib_dev/github/pkg", "libodb.so")
env.InitEnv ()
# --> NOW pyodb could be imported  !
import pyodb   
from   pyodb   import  odbDict , odbGcdistance
from   pyodb   import  odbConnect , odbClose 
from   pyodb   import  odbDca  """

# odb4py  
from odb4py.utils import SqlParser , OdbObject
from odb4py.core  import odb_dca, odb_open , odb_close ,odb_dict, odb_gcdist



# Obstool, Desroziers & Jarvinen  , Tools &  modules 
from build_sql       import SqlHandler
from obstype_info    import ObsType
from setting         import Setting  , Conv
from handle_df       import *
from io_base         import DataIO




class DCAFiles:
    """
    Class :  Checks and creates the DCA (Direct Access Column )files 
             if they are not already in the ODB 
             If the DCA files are already there , this class is not called 
             Methods :  
                     CheckDca 

    """
    def __init__(self):
        return None

    def CheckDca( self,   dbpath , sub_base=None   , verbose = False   ):
        # Prepare DCA files if not  in ODB 
        try:  # os.path.isdir( dbpath  ):
           db      = OdbObject ( dbpath )
           dbname  = db.get_attrib()["name"]
        
           if not os.path.isdir ("/".join(  [dbpath , "dca"] )  ):
              if verbose in [2,3 ]:
                 print( "No DCA files in {} 'directory'".format(dbpath ) )
                 #env.OdbVars["CCMA.IOASSIGN"]="/".join(  (dbname, "CCMA.IOASSIGN" ) )
                 #env.OdbVars["ECMA.IOASSIGN"]="/".join(  (dbname, "ECMA.IOASSIGN" ) )
                 status =    odb_dca ( database=dbpath , dbtype=dbname , ncpu=8  )
                 if status < 0 :
                    print("Failed to create DCA files ... \n Another attempt will be done with odb_dict !" )
              else :
                 if  verbose ==True :
                     print("DCA files already in database: '{}'".format( dbname )  )
                 else:
                     pass 
        except:
          FileNotFoundError
          print("WARNING : ODB path {} not found".format(dbpath))
          pass 





class OdbReader:
    """
    Class : Prepare the query according to the user setting (Obs list , period etc)
            The SQL query is sent directly to the ECMA or CCMA ODB .

            Returns data are as python dictionary 
          Methods : get_odb_rows

    """
    def __init__(self , dbpath ,type_  ):
       # Path to the directory containing the ODB(s)
       self.odb_path = dbpath    
       self.odb_type = type_
       self.vrb  = 1
       if  not os.path.isdir (self.odb_path): 
           print("ODB(s) directory '{}' not found.".format( self.odb_path ))  
           sys.exit(0)

       # Setting 
       self.st    = Setting ()
        
       # DCA Files 
       self.dca_f = DCAFiles()

       # CONV 
       self.conv      = Conv()
       self.conv_obs  = self.conv.conv_obs
       self.cols      = self.conv.cols 
       self.tables    = self.conv.tables
       self.other_sql = self.conv.other_sql
    
       # IO 
       self.io = DataIO  ()
       # SQL      
       self.sql   =SqlHandler()

       # DF 
       self.rd    =Rows2Df () 
       self.cnt   =ConcatDf() 

       # List of odb rows
       self.list_rows =[]

       # return a list of dicts
       self.dlist = defaultdict(list)

    def get_odb_rows  (self,     period      ,      
                                 obs_list    ,
                                 max_dist    ,
                                 bin_dist    ,
                                 #file_io     ,  later !
                                 file_path   ,
                                 cycle_inc  =3, 
                                 pbar       =False ,
                                 verbosity  =0,
                                 chunk_size = None 
                                  ):

        vrb=verbosity
        if vrb not in [0,1,2,3]:
           print("Min and max verbosity levels: 0 ->  3.  Got :  ", vrb )
           print("Fallback to default value:  verbosity=", self.vrb  )  


        # Default dict  (Collect dataframes with variable as a key )
        df_vars =  defaultdict(list)

        # Diags period 
        period_interval=  [ min( period ) , max(period)  ]

        # ODB  Paths 
        paths =  [ "/".join( (self.odb_path , prd , self.odb_type )) for prd in period  ]

        # Obs attributes 
        obs_names , codetype , obstype , varno , lrange , vertco ,sensor =self.st.set_obs_list(obs_list  )

        list_dict=[]
        for i, cma_path in enumerate(paths):

            # Open ODB
            conn= odb_open ( cma_path )

            # UPDATE IOASSIGN , ODB_SRCPATH & ODB_DATAPATH
            os.environ["IOASSIGN"]=cma_path+"/IOASSIGN"
            os.environ["ODB_SRCPATH_CCMA"] =cma_path
            os.environ["ODB_DATAPATH_CCMA"]=cma_path
            os.environ["ODB_IDXPATH_CCMA" ]=cma_path

            # Check DCA directory  (if not there they will be created )
            dca_f=DCAFiles()
            dca_f.CheckDca ( cma_path  )

            if vrb == [ 2 , 3]:
               print("ODB PATHS set to  :")
               print("IOASSIGN          :",cma_path+"IOASSIGN" )
               print("ODB_SRCPATH_CCMA  :",cma_path )
               print("ODB_DATAPATH_CCMA :",cma_path )
               print("ODB_IDXPATH_CCMA  :",cma_path )
            
            for jo, obs in enumerate(obs_names) :
                query=self.sql.BuildQuery(  columns            =self.cols      ,
                                                 tables        =self.tables    ,
                                                 obs           =obs            ,
                                                 obstype       =obstype  [jo]  ,
                                                 obsvano       =varno    [jo]  , 
                                                 codetype      =codetype [jo]  ,                                                  
                                                 lrange        =lrange   [jo]  ,
                                                 vertco_type   ="height"       ,
                                                 vertco        =vertco   [jo]  , 
                                                 sensor        =sensor   [jo]  ,
                                                 remaining_sql =self.other_sql )
                nfunc , sql_query = self.sql.CheckQuery( query)
                cdtg =  period [i]
                if vrb in [0, 1, 2, 3]:
                   print( "Process observation type: {}    ODB date : {} ".format( obs , cdtg   ))                       
                query_file=None   ;
                poolmask = None   ; 
                pool      =None   ; 
                float_fmt= 15     ;   # 10 digits float values  
                verbose = False   ;
                pbar    = False 

                # Progress bar & Verbosity inside pyodb  
                # Progress bar is useful for huge ODBs
                if vrb in [3  ]: 
                   verbose= True 
                if vrb in [2,3]: 
                   pbar   = True 
                
                
                # Write odb rows once , if there is a rerun of the same period 
                # the odb extraction is skipped 
                filename = "_".join(  ( "df_rows" ,  obs ,cdtg))   +".csv"
                fpath    = "/".join(  (self.odb_path,cdtg , filename) )
                if os.path.isfile(  fpath   ):
                   if vrb in [1,2,3]:
                      print("ODB rows already in file obstype: {} , date: {}".format( obs, cdtg ) ) 
                      rows    = self.io.ReadFrame(  fpath )
                      # Process rows from file 
                      df_dist = self.rd.DfDist (rows, obs, cdtg,  max_dist )                                                                                 
                      
                      # Subset df
                      spl      =  SplitDf (df_dist , obs , cdtg  )                
                      ndist    =  df_dist.dropna(subset=["dist"]).copy()
                      df_stat  =  spl.SubsetDf(  bin_dist , max_dist) 
                      self.dlist[obs].append( df_stat ) 
                else:
                   # Get and process the rows from ODB 
                   if vrb in [ 2,3]:
                      print( "ODB rows not available from file :", filename  ) 
                      print( "Proceed to data extraction ...")
                   try:   
                      rows= conn.odb_dict (cma_path  ,
                                       sql_query , 
                                       nfunc     , 
                                       float_fmt , 
                                       query_file, 
                                       poolmask  , 
                                       pbar      ,
                                       verbose )         
                      conn.odb_close()
                      if not rows:
                         print( "WARNING : Data not available for the variable {} and varno={}".format( obs , varno[jo])  )
                         empty_df=  pd.DataFrame([])
                         self.dlist[obs].append( empty_df  )
                         
                   except:
                      RuntimeError
                      print( "WARNING : Data not available for the variable {} and varno={}".format( obs , varno[jo])  )
                      empty_df=  pd.DataFrame([])
                      self.dlist[obs].append( empty_df  )                      
                      pass 
                   else:                     
                      # Process rows directly  if the file not there 
                      df_dist = self.rd.DfDist (rows, obs, cdtg,  max_dist  )

                      # Subset df
                      spl    =  SplitDf (df_dist , obs , cdtg  )                
                      ndist  =  df_dist.dropna(subset=["dist"]).copy()
                      df_stat=  spl.SubsetDf(  bin_dist , max_dist) 
                      self.dlist[obs].append( df_stat )   

                      # Save data.
                      io_fpath   = "/".join( (  self.odb_path, cdtg  )) 
                      rows_file  = "_".join( ( "df_rows" ,  obs ,cdtg))   +".csv"
                      stat_file  = "_".join( ( "df_stat" ,  obs ,cdtg))   +".csv"

                      rows_fpath = "/".join( (  self.odb_path,cdtg , rows_file) )
                      stat_fpath = "/".join( (  self.odb_path,cdtg , stat_file) )

                      # Write if not done !
                      if not os.path.isfile(rows_fpath ):
                         df_rows = pd.DataFrame( rows )
                         self.io.FlushFrame( df_rows , self.odb_path,   cdtg , obs, fid=None ,verbose =vrb ,filetype="df_rows")

                      if not os.path.isfile (stat_file):
                         self.io.FlushFrame( df_stat  , self.odb_path,   cdtg , obs, fid=None ,verbose =vrb ,filetype="df_stat")
        return self.dlist







# The methods below are ouside classes 
# Since they may be used by multiple processes 
def CreateSharedMfile  (name, size):
    # Delete and recreate the shared memory file if exists 
    # under  /dev/shm 
    try:
        # If exists close and delete 
        old = shared_memory.SharedMemory(name=name)  # unlink and close 
        old.close()
        old.unlink()
    except FileNotFoundError:
        pass
    return shared_memory.SharedMemory(name=name, create=True, size=size)




def GcdistChunkShared (i0, i1, N):
    """
    Method :    Worker shared memory .
                 Only receives i0, i1 and global N.
                 Accesses shared memory numpy arrays directly.
    returns:    The matrix chunked blocks and begin/end index  
    """

    # 
    CreateSharedMfile
    shm_lon = shared_memory.SharedMemory(name="shm_lons")
    shm_lat = shared_memory.SharedMemory(name="shm_lats")

    # Recreate numpy  arrays as buffers 
    lon = np.ndarray((N,), dtype=np.float64, buffer=shm_lon.buf)
    lat = np.ndarray((N,), dtype=np.float64, buffer=shm_lat.buf)

    # Get latlon by chunk indices 
    sub_lon = lon[i0:i1]
    sub_lat = lat[i0:i1]

    # The method for computing the great circle distances is written in C 
    # The C code is more or less the same as the one used in R "sp" package   
    # --In order to get the same values as in the original obstool , the method 
    # in sp package was adapted to odb4py python  package (The method is called odb_gcdist)
    block = odb_gcdist (sub_lon, sub_lat, lon, lat)

    # Close memory buffers handle 
    shm_lon.close()
    shm_lat.close()
    return (i0, i1, block)




class DistMatrix:
    """@Class  : DistMatrix: computes the interdistances between all 
                 the latlon pairs  ( for N coordinates > 3000 , the process starts to slow down)

                 Approach to speed up  : Divide the coordinates arrays into chuncks and 
                 compute each block in a sparated process

       @Returns: A reconstructed matrix of all distances between latlon pairs. 
           
               Methods : 
                       GcdistParallel

    """

    def __init__(self, lons, lats):
        self.lons = np.asarray(lons, dtype=np.float64)  # Init with 64 bits 
        self.lats = np.asarray(lats, dtype=np.float64)
        self.N = len(self.lons)

    def GcdistParallel(self, chunk_size=200, workers=None):
        N = self.N
        lon = self.lons
        lat = self.lats

        # If the number of CPUs is set otherwise get from the machine 
        if workers is None:
           workers = mp.cpu_count()

        # Create shared memory file
        # More Safe
        shm_lons =CreateSharedMfile ( size=lon.nbytes  , name="shm_lons" )
        shm_lats =CreateSharedMfile ( size=lon.nbytes  , name="shm_lats" )

        # Copy data once
        np.ndarray(lon.shape, dtype=np.float64, buffer=shm_lons.buf)[:] = lon
        np.ndarray(lat.shape, dtype=np.float64, buffer=shm_lats.buf)[:] = lat

        # Chunk ranges 
        chunk_ranges = [(i0, min(i0 + chunk_size, N))  for i0 in range(0, N, chunk_size)]

        # Init the final matrix  
        distmat = np.zeros((N, N), dtype=np.float64)
        try:
            with mp.Pool(processes=workers) as pool:                
                # Use starmap method and give chunk ranges as args 
                results = pool.starmap( GcdistChunkShared   , [(i0, i1, N) for (i0, i1) in chunk_ranges])

            # Fill the matrix by block 
            for (i0, i1, block) in results:
                distmat[i0:i1] = block
        finally:
            # Cleanup and unlink memory
            shm_lons.close()
            shm_lons.unlink()
            shm_lats.close()
            shm_lats.unlink()

        return distmat



class Rows2Df :

    """
    Class:   Build the dataframes with O-G and O-A departures. 
             To speed up the computation of the distances between the 
             latlon pairs , the matrix is computed by chunks   

             Returns a DF of O-G , O-A , indices and distances 
          Methods :
                    DfDist  
                    DfPrep
    """

    def __init__(self):
        # I/O
        self.io    = DataIO ()
        return None 



    def DfDist (self ,  rows         ,
                        obs          , 
                        cdtg         ,   
                        max_dist     , 
                        verbosity =0 , 
                        write_file=None, 
                        file_path =None,
                        filetype  =None
                        ):
        #pd.set_option('display.max_rows',  20 )
        vrb=verbosity
        if vrb not in [0,1,2,3]:
            print("WARNING : Min and max verbosity levels: 0 ->  3.  Got :  ", vrb )
            print("Fallback to default value:  verbosity=", vrb  ) 

        if rows is None:
           print("Rows from ODB not available for  var :  {}".format(var ) )
           sys.exit()
        else:
           df_rows = pd.DataFrame(rows )
           lats  = np.array(rows["degrees(lat)" ])
           lons  = np.array(rows["degrees(lon)" ])
           an_d  = np.array(rows["an_depar@body"])
           fg_d  = np.array(rows["fg_depar@body"])
           
           # ARGS order  :  lons1, lats1, lon2, lat2 : Compute distances between latlon pairs 
           d    = DistMatrix (lons , lats  )           

           # The distances ar rounded to 0 decimals 
           # Else , there will be a number of observations sets in each bins. 
           # It leads to different statistics  between R and   Python  whil perfomrming the cut , subset etc . 
           dist =  d.GcdistParallel().round(0)  #astype(int) 

           N = len(lats)
           dist_1d = dist.ravel()

           # Indices as in  as.table(matdist) : row/col in R version  
           d1, d2 = np.indices((N, N))
           d1 = d1.ravel().astype(np.int32, copy=False)
           d2 = d2.ravel().astype(np.int32, copy=False)

           # Invert d1, d2 to match the ones in R 
           ndist = pd.DataFrame({
                     "n1"  : d2,
                     "n2"  : d1,
                     "dist": dist_1d  })
 
           # Subset using the max distance 
           df_dist = ndist[ndist["dist"] <= max_dist].copy()

           # Add OA/FG departures to corresponding indices 
           df_dist["OA1"] = an_d[df_dist["n1"].values]
           df_dist["OA2"] = an_d[df_dist["n2"].values]
           df_dist["FG1"] = fg_d[df_dist["n1"].values]
           df_dist["FG2"] = fg_d[df_dist["n2"].values]

           # Set datatypes
           df_dist = df_dist.astype({
                              "n1": "int32",
                              "n2": "int32",
                            "dist": "int32",
                             "OA1": "float64",
                             "OA2": "float64",
                             "FG1": "float64",
                             "FG2": "float64"
                                    })
           # For safety  
           df_dist = df_dist[df_dist["dist"] <= max_dist].copy()           

           # Round to 4 decimal precision   
           return df_dist.round(4)



    def DfPrep   (self , 
                  frame_liste, 
                  dir_=None ,
                  period=None  , 
                  var_list=None  ): 
        # Concat df by  vars  for the whole period 
        cnt=  ConcatDf ()
        cdf=  cnt.ConcatFromListe (  frame_liste)
        return cdf   
