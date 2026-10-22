# -*-coding:utf-8 -*-
import sys , os 


class ObsType:
      """
      Class: Contains the  variable names and their attributes  
             NOTE : ONLY THE CONV DATA ARE PROCESSED IN THIS VERSION  !

             Methods:
                    ConvDict
                    RenameVarno  
                    SelectConv
      """

      def _init__(self):
          # ObsType lists 
          # Init the observation lists : Conventional and Satem            
          self.conv_obs=[    
                        'gpssol' ,
                        'synop'  ,
                        'dribu'  ,
                        'airep'  ,
                        'airepl' ,
                        'radar'  ,
                        'temp'   ,
                        'templ'  ]

          self.sat_obs = [    
                         'amsua'  ,
                         'amsub'  ,    
                         'atms'   ,   
                         'iasi'   ,
                         'mwhs'   ,
                         'msh'    ,
                         'seviri' ]
          return None 

      def ConvDict(self) :
          # CONVENTIONAL  
          self.obs_conv=[


         # MAYBE WILL BE CHANGED TO A LIST AND LOOP  !
         # SURFACE 
         {"obs_name"            : "gpssol",
           "obstype"           : 1  ,
           "codetype"          : 110 , 
           "varno"             : 128  ,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None         } ,
         { "obs_name"          :  "synop_z",
           "obstype"           : 1 ,
           "codetype"          : [11, 14, 170, 182] ,
           "varno"             :  1,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None          },
         { "obs_name"          :  "synop_v",
           "obstype"           : 1 ,
           "codetype"          : [11, 14, 170, 182] ,
           "varno"             : 42,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None          },
         { "obs_name"          :  "synop_u",
           "obstype"           : 1 ,
           "codetype"          : [11, 14, 170, 182] ,
           "varno"             : 41,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None          },
         { "obs_name"          :  "synop_h",
           "obstype"           : 1 ,
           "codetype"          : [11, 14, 170, 182] ,
           "varno"             : 58, 
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None          },
         { "obs_name"          :  "synop_t",
           "obstype"           : 1 ,
           "codetype"          : [11, 14, 170, 182] ,
           "varno"             : 39,                 
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None          },
         { "obs_name"          : "dribu_z",
           "obstype"           : 4 ,
           "codetype"          : None,
           "varno"             : [1, 39, 41, 42],
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None          },
            { "obs_name"          : "dribu_t",
           "obstype"           : 4 ,
           "codetype"          : None,
           "varno"             : 39,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None          },
            { "obs_name"          : "dribu_u",
           "obstype"           : 4 ,
           "codetype"          : None,
           "varno"             : 41,             
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None          },
            { "obs_name"          : "dribu_v",
           "obstype"           : 4 ,
           "codetype"          : None,
           "varno"             : 42,             
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None          },

         { "obs_name"          : "ascat",
           "obstype"           : 9 ,
           "codetype"          : None,
           "varno"             : None,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None           } ,
         # RADAR 
        {  "obs_name"          : "radar_rh",
           "obstype"           : 13,
           "codetype"          : None,
            "varno"            : 29,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None            },
       {  "obs_name"           : "radar_dow",
           "obstype"           : 13,
           "codetype"          : None,
            "varno"            : 195 ,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None            },


       # UPPER AIR 
        {  "obs_name"          : "airep_t",
           "obstype"           : [2] ,
           "codetype"          : None,
           "varno"             : [2] ,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None           },
        {  "obs_name"          : "airep_u",
           "obstype"           : 2 ,
           "codetype"          : None,
           "varno"             : 3, 
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None           },
        {  "obs_name"          : "airep_v",
           "obstype"           : 2 ,
           "codetype"          : None,
           "varno"             : 4 ,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None           },
        {  "obs_name"          : "airepl_t" ,
           "obstype"           : 2 ,
           "codetype"          : None,
           "varno"             : 2,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : [25000, 35000] },
        {  "obs_name"          : "airepl_u" ,
           "obstype"           : 2 ,
           "codetype"          : None,
           "varno"             : 3,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : [25000, 35000] },
        {  "obs_name"          : "airepl_v" ,
           "obstype"           : 2 ,
           "codetype"          : None,
           "varno"             : 4, 
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : [25000, 35000] },
         { "obs_name"          : "temp_t",
           "obstype"           : 5,
           "codetype"          : None,
           "varno"             : 2,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None           },
         { "obs_name"          : "temp_u",
           "obstype"           : 5,
           "codetype"          : None,
           "varno"             : 3,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None           },
         { "obs_name"          : "temp_v",
           "obstype"           : 5,
           "codetype"          : None,
           "varno"             : 4, 
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None           },
         { "obs_name"          : "temp_q",
           "obstype"           : 5,
           "codetype"          : None,
           "varno"             :  7, 
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : None           },
         { "obs_name"          : "templ_t" ,
           "obstype"           : 5 ,
           "codetype"          : None,
           "varno"             : 2,  
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : [40000, 60000]} , 
         { "obs_name"          : "templ_u" ,
           "obstype"           : 5 ,
           "codetype"          : None,
           "varno"             :  3, 
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : [40000, 60000]} , 
         { "obs_name"          : "templ_v" ,
           "obstype"           : 5 ,
           "codetype"          : None,
           "varno"             : 4, 
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : [40000, 60000]} ,
         { "obs_name"          : "templ_q" ,
           "obstype"           : 5 ,
           "codetype"          : None,
           "varno"             : 7,
           "vertco_reference_1": None,
           "sensor"            : None,
           "level_range"       : [40000, 60000] }  ]

    
         # Varno dict 
          self.varno_dict ={ 128 : "ztd"  ,  # gpssol Zenith total delay 
                       1   :  "z"   ,  # geopotential 
                       42  :  "u"   ,  # 10m wind speed u
                       41  :  "v"   ,  # 10m wind speed v
                       58  :  "h"   ,  # 2 relative humidity
                       39  :  "t"   ,  # 2 meter temperature 
                       2   :  "t"   ,  # T temperature       (upper air)
                       3   :  "u"   ,  # U component of wind (upper air)
                       4   :  "v"   ,  # V component of wind (upper air)
                       7   :  "q"   ,  # specific humidity 
                       29  :  "rh"  ,  # upper rh (radar)
                      195  :  "dw"     # Dopp radial wind 
                      }

          # Add Units  
          self.unit= { 
                       128 :  "m"     ,  # gpssol Zenith total delay  
                       1   :  "mgp"   ,  # geopotential 
                       42  :  "m/s"   ,  # 10m wind speed u
                       41  :  "m/s"   ,  # 10m wind speed v
                       58  :  "%"   ,    # 2 relative humidity
                       39  :  "K"   ,    # 2 meter temperature 
                       2   :  "K"   ,    # T temperature       (upper air)
                       3   :  "m/s"   ,  # U component of wind (upper air)
                       4   :  "m/s"   ,  # V component of wind (upper air)
                       7   :  "g/kg"   , # specific humidity 
                       29  :  "%"  ,     # upper rh (radar)
                      195  :  "m/s"      # Dopp radial wind 
                  }
          return self.obs_conv  , self.varno_dict , self.unit 
 


      def RenameVarno (self, string_var  ):
          dict_={}
          obslist , dict_            = self.ConvDict()
          obsname=[  dobs["obs_name"] for dobs in obslist  ]
          name      =    string_var.split("_")[0]
          vr        =int(string_var.split("_")[1] )
          vname     =name + "_"+ dict_[vr]
          return vname 


      def SelectConv(self, list_   ):
          varobs=[]
          if not isinstance  ( list_ ):
             print("Parameter list must be a python list")
             sys.exit ()
          self.list          = list_
          obs_list, var_dict , var_unit  =self.ConvDict()


