# -*- coding: utf-8 -*-
import os  
import configparser  


class TuneEnv:
      """
      Class: CONFIG OBJECT AS ARGUMENT 
             PARSES THE SECTIONS IN config 
             FILE AND EXTRACTS THE ITEMS 
             
      """
      def __init__(self, config ):
          self.BeginDate= config.get('DATES', 'DATESTART') 
          self.EndDate  = config.get('DATES', 'DATEEND'  )
          self.njobs    = config.get('DATES', 'NSLICE'       , fallback=2)
          self.llverb   = config.getboolean('OPTIONS' ,'LLVERB', fallback="false")
          self.lwrite   = config.getboolean('OPTIONS' ,'LWRITE', fallback="false")


          pp = config.items("PATHS")
          self.path_opt = { str(opt[0]):str(opt[1]) for opt in pp }

          self.basedir =self.path_opt["BASEDIR"]
          self.libpath =self.path_opt["LIBPATH"]
          self.mpirun  =self.path_opt["MPIRUN"]
          self.packdir =self.path_opt["PACKDIR"]
          self.binary  =self.path_opt["BINARY"]
          self.stabal  =self.path_opt["STATFILE"]
          self.odbpath =self.path_opt["ODBPATH"]
          self.odb_template=self.path_opt["ODB_TEMPLATE"]

          mm =  config.items("MODEL")
          self.model_opt= { str(opt[0]):str(opt[1]) for opt in mm }
                
          self.cycle_inc =self.model_opt["CYCLE_INC"]
          self.deltax    =self.model_opt["DELTAX"]
          self.nsmax     =self.model_opt["NSMAX"]
          self.nflev     =self.model_opt["NFLEV"]
          self.rednmc    =self.model_opt["REDNMC"] 
          return None    

      def __Dicts__(self ):
          PathAttr = self.path_opt
          ModelAttr= self.model_opt 
          return PathAttr ,ModelAttr 


