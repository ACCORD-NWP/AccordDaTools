# -*- coding: utf-8 -*-
import os  
import configparser  


class TuneEnv:
      """
      Class:  Parse the sections and get the items  as a dict              
      """
      def __init__(self, config ):
          self.BeginDate= config.get('DATES', 'DATESTART') 
          self.EndDate  = config.get('DATES', 'DATEEND'  )
          #self.ntaks    = config.get('DATES', 'NSLICE'     , fallback=2)
          self.outfile  = config.get('OPTIONS', 'OUTFILE'  , fallback = "sigmabo_ratios"  )
          self.dca_cpu  = config.get('OPTIONS' ,'DCA_CPU', fallback= 4 )
          self.llverb   = config.getboolean('OPTIONS' ,'LLVERB' , fallback="false")
          self.lwrite   = config.getboolean('OPTIONS' ,'LWRITE' , fallback="false")
          self.lplot    = config.getboolean('OPTIONS' ,'LPLOT'  , fallback="false")
          self.prog_bar = config.getboolean('OPTIONS' ,'PROGBAR', fallback="false")




          pp = config.items("PATHS")
          self.path_opt = { str(opt[0]):str(opt[1]) for opt in pp }

          self.basedir =self.path_opt["BASEDIR"]
          self.stabal  =self.path_opt["STATFILE"]
          self.odbpath =self.path_opt["ODBPATH"]
          self.odb_template=self.path_opt["ODB_TEMPLATE"]
          self.rows_path=self.path_opt["ROWS_PATH"]
          self.tmp_dir  =self.path_opt["WORKDIR"]

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


