# -*- coding: utf-8 -*-
import os 
import sys  
import multiprocessing as mp 
import shutil  
from datetime     import date, timedelta , datetime 
from setting_env  import TuneEnv


class Odb:
      """
      Class : Odb , GET THE PATHS FROM TuneEnv OBJECT 
                    PRFORMS A PARALLEL ODB EXRACTION
                    CONTAINS ReadMandalay Method TO READ 
                    OBS-ERROR , obs-guess DEPARTURES AND obs-analysis 
                    DEPARTURES 
                    Return :Obs_err , obs-fg , obs-an  departures  
      """
      def __init__(self, Paths ):
          self.paths=Paths
          return None  

      def OdbExtract( self,  dates  ):
          basedir=self.paths["BASEDIR"]
          packdir=self.paths["PACKDIR"]
          odbpath=self.paths["ODBPATH"]
          binary =self.paths["BINARY"]
          odb_template=self.paths["ODB_TEMPLATE"]
          libpath=self.paths["LIBPATH"]
          mpirun =self.paths["MPIRUN" ]
          #nslice =self.paths["NSLICE"]
          # EXPORT DYNAMIC LIBRARIES TO PYTHON ENV
          os.environ["LD_LIBRARY_PATH"]=libpath
   
          # CLEAN out DIRECTORY 
          os.system( "rm    -rf   "+basedir+"/out/*")
          print( "Processing dates :" , dates) 
          for dt in dates:
              os.chdir(basedir)
              tmpdir=basedir+"/tmp/"+dt
              # DELETE AND CREATE NEW TMPDIR 
              if os.path.isdir(tmpdir):
                 shutil.rmtree(tmpdir)
              os.system("mkdir -p "+tmpdir)
              # BRING EXEC
              binpath=packdir+"/"+binary
              if os.path.isfile(binpath):
                 os.symlink(binpath, tmpdir+"/"+binary)
              else:
                 raise Exception (binpath +" NOT FOUND !")

              FileTemplate=odb_template 
              filename=FileTemplate.replace( "YYYYMMDDHH", dt)
              odbfile =odbpath+"/"+filename

              # WHICH EXTENSION .? IT IS OFTEN ".tar"  BUT NEVER KNOW !
              arc_ext =os.path.splitext(odbfile)[1][1:]
              if os.path.isfile(odbfile ):
                 shutil.unpack_archive(odbfile, tmpdir , arc_ext)
              else:
                   print ("ODB FILE :" , odbfile , " IS MISSING !")
                   pass  
             
              # ODB ENV VARIBALES 
              odb_env={"ODB_SRCPATH_CCMA":  tmpdir+"/CCMA"   ,
                        "ODB_DATAPATH_CCMA":tmpdir+"/CCMA"  ,
                        "IOASSIGN":tmpdir+"/CCMA/IOASSIGN"  ,
                        "TO_ODB_ECMWF":"0"            ,
                        "ODB_STATIC_LINKING":"1"      ,
                        "ODB_CMA":"CCMA"              ,
                        "ODB_CTX_DEBUG":"1"           ,
                        "VERSION":"1"                 ,
                        "DEGRE":"1"                   ,
                        "DIRECT":"0"                  ,
                        "F_RECLUNIT":"BYTE"}

              # EXPORT 
              for k , v in odb_env.items():
                  os.environ[k] =v
  
              # RUN 
              os.chdir(tmpdir)
              os.system(mpirun + " ./"+binary+" > out.log 2> out.err ")
              os.system( "mkdir -p    "+basedir+"/out"  )
              if os.path.isfile (tmpdir+"/fic_odb.lst"):
                 shutil.move( "fic_odb.lst" ,  basedir+"/out/depar_raw_"+dt )
                 print( "ODB data extracted ...  date :" , dt )
              else:
                 print( "MANDALAY :Failed to extract odb data , date :", dt   )


       # SPLIT DATE LIST INTO EVENLY SIZED SLICE 
      def Chunks( self  , l, n , nslice ):
         if len(l)   >= int(nslice):
            return [l[i:i+n] for i in range(0, len(l) , int(n) )]
         elif len(l) <  int(nslice):
            return l

      def DispatchJobs(self, data, nslice ):
         jobs=[]
         total = len(data)
         chunk_size = total / int(nslice)
         if chunk_size >= 1 : 
            slice = self.Chunks(data, int(chunk_size) , nslice )
            for i in range(len(slice)):
                j = mp.Process(target=self.OdbExtract, args=(slice[i] , ))
                jobs.append(j)  
                j.start()       
            for j in jobs: 
                j.join()
         else:
            print("NUMBER OF PROCESSES  > THAN THE DATE LIST LENGTH,\n NSLICE RESET TO "+str(len(data)))
            nslice=len(data )
            j = mp.Process(target=self.OdbExtract, args=(data , ))
 
      def ReadMandalay( basedir, cdtg, target ):
          # READ EXTRACTED ODB  FILE 
          # INIT LISTS (Equivalent to files I/O in the orginal fortran version )
          t_an =[] ; tb_an =[] ; q_an =[] ; uv_an =[]
          t_fg =[] ; tb_fg =[] ; q_fg =[] ; uv_fg =[]
          t_err=[] ; tb_err=[] ; q_err=[] ; uv_err=[]
    
          # IF THE MANDALAY EXTRACTION WAS SUCCESSFULL THEN 
          # THE FILE EXISTS OTHERWISE , SKIP THE DATE/TIME
        
          dataset=None 
          if os.path.isfile(basedir+"/out/depar_raw_"+cdtg ):
             dataset=basedir+"/out/depar_raw_"+cdtg
        
            
             file_=open(dataset , 'r')
             lines=file_.readlines()[10:]    # SKIP THE HEADER  
             for line in lines:
                 ncol= len(line.split() )
                 if ncol == 5:               # FIXED NUMBER OF COLUMNS FROM MANDALAY ( 5 ) 
                    l=line.rstrip().split() 
                    varno  =l[1]
                    an_dep =l[2]   
                    fg_dep =l[3]
                    obs_err=l[4]
                    if int(varno)  == 2  and an_dep !="NULL" and fg_dep !="NULL": # GET TEMPERATURE
                       t_an.append (float(an_dep) )
                       t_fg.append (float(fg_dep) )
                       t_err.append(float(obs_err))
                    elif int(varno)==119 and an_dep !="NULL" and fg_dep !="NULL": # BRIGHTNESS T 
                       tb_an.append (float( an_dep))
                       tb_fg.append (float(fg_dep) )
                       tb_err.append(float(obs_err))
                    elif int(varno) ==7  and an_dep !="NULL" and fg_dep !="NULL": # SPECIFIC Q  
                       q_an.append (float(an_dep))
                       q_fg.append (float(fg_dep))
                       q_err.append(float(obs_err))
                    elif int(varno)==3 and an_dep !='NULL' and fg_dep !='NULL' or int(varno)==4 and an_dep !='NULL' and fg_dep !='NULL':    # U , V 
                       uv_an.append (float(an_dep))
                       uv_fg.append (float(fg_dep))
                       uv_err.append(float(obs_err))

             # RETURN PREDEFINED OBS ERRORS
             if    target=="predef":                  # RETURN PREDEFINED  
               return t_err, tb_err, q_err, uv_err
             # RETURN LISTS FOR DIAGNOSTICS 
             elif  target=="fg_diag":                 # FOR fg_dep
               return t_fg , tb_fg ,q_fg , uv_fg
             elif  target=="an_diag":                 # FOR an_dep
               return t_an , tb_an , q_an, uv_an
         
     
