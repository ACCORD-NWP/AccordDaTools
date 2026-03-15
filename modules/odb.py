# -*- coding: utf-8 -*-
import os , sys  
from pathlib import Path
import shutil  
from   datetime   import date, timedelta , datetime 
import pandas as  pd  
import tarfile


# odb4py 
from odb4py.utils  import SqlParser , OdbObject
from odb4py.core   import odb_open , odb_dca ,odb_dict 


# TuneBR modules
from .config_env  import TuneEnv

class Odb:
      """
      Class : Odb , GET THE PATHS FROM TuneEnv OBJECT 
                    PRFORMS A PARALLEL ODB EXRACTION.
                    CONTAINS : ReadOdbRows   Method TO READ 
                    OBS-ERROR , obs-guess DEPARTURES AND obs-analysis 
                    DEPARTURES 
                    Return :Obs_err , obs-fg , obs-an  departures  
      """
      def __init__(self, Paths ):
          self.paths    =Paths
          self.dbtype   ="CCMA"   # Can be hard coded since it's NOT possible to use ECMA in TuneBR 
          self.sql_query="SELECT obstype,varno,an_depar,fg_depar,final_obs_error FROM  hdr, body, errstat"
          self.ext_list=[ item [0] for item in shutil.get_archive_formats() ]
          self.rows_path =Path (Paths["ROWS_PATH"] )


      def CheckTarball (self, tarpath ):
          try:
              with tarfile.open(str(tarpath) , 'r') as t:
                 try:
                    member = t.getmember(self.dbtype  )
                    return member.isdir()
                 except KeyError:
                    return False
          except:
            FileNotFoundError
            print("WARNING : tar file {} not found. ".format(tarpath))
            pass 



      def CopyOdb (self , dt  ):
          basedir     =Path (self.paths["BASEDIR"] )
          odbpath     =Path (self.paths["ODBPATH"] )
          odb_template=Path (self.paths["ODB_TEMPLATE"] )
          tmpdir      =Path (self.paths["WORKDIR"] )
          dtedir      = tmpdir / dt

          # rreate tmpdir if it doesn't exist
          tmpdir.mkdir(parents=True, exist_ok=True)
           
          # Delete and recreate the date dir 
          shutil.rmtree(dtedir, ignore_errors=True)
          dtedir.mkdir(parents=True)

          # Decompress ODB  archive  
          # (We assume that the CCMA is the first dir inside the tar file , 
          # if not, an Exception is raised !  )
          FileTemplate=odb_template
          filename=str(FileTemplate).replace( "YYYYMMDDHH", dt)
          tarfile = odbpath / filename
          status =  self.CheckTarball( tarfile  )
     
          if status == False:
             raise Exception ("The ODB archive must contain a CCMA directory" )

          # Which extension  .? It is often ".tar"  But never know !
          arc_ext =os.path.splitext(tarfile)[1][1:]
          if arc_ext  not in self.ext_list:
             raise Exception ("Supported formats for archive unpacking :", ext_list) # Supports: zip, tar, gztar, bztar, xztar  or zstdtar.

          if os.path.isfile(tarfile ):
             print( "Decompress the ODB archive ...", tarfile  )
             # Unpack in tmp dir 
             shutil.unpack_archive(tarfile, dtedir , arc_ext)






      def CreateDca  ( self,  dates , ncpu   ):
          NCPU = int(ncpu ) 
          basedir     =self.paths["BASEDIR"]
          tmpdir=  "/".join((basedir, "tmp" ))          
          # It is possible to have a string , be sure it s a list 
          if isinstance (dates , str ):           
             dates = [ dates ]
          
          # Loop over dates list 
          for dt in dates:
              if len( dt ) != 10: 
                 print("Malformatted start date. Must be YYYYMMDDHH\n") 
                 sys.exit(1)
                 
              print( "Prepare ODB : Date ...  {} \n".format( dt )  )
              self.CopyOdb (  dt )
              dtedir=  "/".join((tmpdir ,  dt   ))
              # Get attributes and create DCA                  
              dbpath="/".join( (dtedir ,"CCMA"  ))              
              if os.path.isdir  ( dbpath ):
                 # It can hapen to have  CCMA directory but a missing  CCMA.dd , .sch or .desc 
                 # Better use try 
                 conn = odb_open ( dbpath   )
                 try:
                    db      = OdbObject (dbpath)
                    db_attrs= db.get_attrib ()
                    tab_list= db_attrs["tables"]
                    ic   = conn.odb_dca(database =dbpath , 
                                   dbtype   ="CCMA" ,
                                   ncpu     = NCPU  ,
                                   extra_args ="-z -u -q ", # Means Update if existe,remove empty files and run in quite mode 
                                   tables =tab_list  )      # Only the tables found in the CCMA 
                    conn.odb_close()

                 except:
                    FileNotFoundError
                    print( f"Missing meta-data files in ODB : {dbpath}" )
              else:

                 # Missing  odbs 
                 print ("ODB file : " , dbpath , " is missing !\n")
                 pass 




      def OdbExtract (self,  dates, rows_path,  progress=False , verbose =False  ):          
          if isinstance (dates , str ):
             dates = [ dates ]

          # Get Basedir 
          basedir=self.paths["BASEDIR"]
          os.chdir(basedir)

          # Loop over dates list
          for dt in dates:
              if len( dt) != 10: 
                 print("Malformatted start date. Must be YYYYMMDDHH\n")
                 sys.exit(1)

              print( "Rows extraction, ODB date : "  , dt  )  
              tmpdir=  "/".join(  (basedir, "tmp" ) )
              dbpath=  "/".join(  (tmpdir , dt , "CCMA")   )

              # ODB ENV VARIBALES  
              # Update the variables. They are visible only inside this scope  'for each datetime'
              odb_env={ "ODB_SRCPATH_CCMA" :dbpath    ,
                        "ODB_DATAPATH_CCMA":dbpath    ,
                        "TO_ODB_ECMWF":"0"            ,
                        "ODB_STATIC_LINKING":"1"      ,
                        "ODB_CMA":"CCMA"              ,
                        "ODB_IO_METHOD":"4"           ,
                        "ODB_CTX_DEBUG":"0"           ,
                        "VERSION":"1"                 ,
                        "DEGRE":"1"                   ,
                        "DIRECT":"0"                  ,
                        "F_RECLUNIT":"BYTE" }
              # Export variables               
              for k , v in odb_env.items(): os.environ[k] =v                

              # Save extracted rows
              os.makedirs( self.rows_path   ,  exist_ok=True)

              # prepre a csv file name 
              filename="_".join(  ("odb_rows", dt     ))
              outfile ="/".join(  (rows_path , filename ))

              # Fetch ODB  rows
              if os.path.isdir ( dbpath ):
                 # We don't need to extract if we rerun for the same period
                 # Skip the datetime if the odb rows are already there !
                 if not os.path.isfile (outfile):
                    try:
                       # odb_dict method throws an exception if no rows returned 
                       conn      =odb_open( dbpath  )
                       data_dict =conn.odb_dict (database=dbpath,
                                     sql_query  =self.sql_query , 
                                     nfunc      =0        ,
                                     fmt_float  =10       , 
                                     queryfile  =None     , 
                                     poolmask   =None     , 
                                     pbar       =progress , 
                                     verbose    =verbose )
                 
                       # Save as csv file ( flush ) 
                       pd.DataFrame(data_dict).to_csv( outfile      , 
                                                       header=False , 
                                                       index= False , 
                                                       sep=","      ,
                                                       decimal='.' )
                       conn.odb_close()
                    except:
                       RuntimeError
                       print("No data returned for ODB {} \n".format( dt ) )
                       pass  
                 else:

                     print(f"Rows are already extracted for the ODB {dbpath} \n" )
                     continue 
              else:
                  print(f"WARNING : CCMA directory not found for the ODB {dbpath} \n")


      def ReadOdbRows ( basedir,  rows_path,  cdtg, target ):
          # If file exists else skip  datetime   (in case of a rerun )
          
          infile="/".join((  rows_path , "odb_rows_"+cdtg ))
          if os.path.isfile ( infile  ):
             # Load dataset from file 
             df = pd.read_csv( infile , sep=","  , names=["obstype","varno","an_depar","fg_depar","final_obs_error"] )
             # Get column Series  
             obstype=df["obstype"]
             varno  =df["varno"  ]             

             # Temperature 
             t_an =df.query(  "varno == 2"  )["an_depar"]
             t_fg =df.query(  "varno == 2"  )["fg_depar"]
             t_err=df.query(  "varno == 2"  )["final_obs_error"]
              
             # Specific Q
             q_an =df.query(  "varno == 7"  )["an_depar"]
             q_fg =df.query(  "varno == 7"  )["fg_depar"]
             q_err=df.query(  "varno == 7"  )["final_obs_error"]
                
             # Brightness Tb
             tb_an =df.query(  "varno == 119" )["an_depar"]
             tb_fg =df.query(  "varno == 119" )["fg_depar"]
             tb_err=df.query(  "varno == 119" )["final_obs_error"]

             # Wind speed U,V 
             uv_an =df.query(  "varno == 3 or varno==4"  )["an_depar"]
             uv_fg =df.query(  "varno == 3 or varno==4"  )["fg_depar"]
             uv_err=df.query(  "varno == 3 or varno==4"  )["final_obs_error"]

             # Obs errors 
             if target=="predef":                  # Returns predefined errors 
               return  (t_err, tb_err , q_err, uv_err)

             # Returns diagnosed fg departures & analysis departures 
             elif  target=="fg_diag":                 #  fg_dep
               return  (t_fg , tb_fg , q_fg , uv_fg)
 
             elif  target=="an_diag":                 #  an_dep
               return  (t_an  , tb_an , q_an , uv_an)
          else:
             return None 
