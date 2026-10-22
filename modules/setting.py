# -*- coding:utf-8 -*- 
import os , sys 
from datetime      import datetime ,timedelta

# Obstool modules 
from .obstype_info  import  ObsType


class Setting:
    """
    class : contains methods to set datetime period and varlist for obstool           
            Methods : set_period 
                      set_obs_list
    """ 
    def __init__ (self ):
        self.obt = ObsType ()

    def set_period(self, bdate , edate , cycle_inc=3 ):
        # Create datetime liste 
        if not isinstance (bdate, str) or not isinstance ( edate , str):
           btype, etype =   type(bdate ) , type( edate)
           print("Start and end dates period argument must be strings. Got {}  {} ".format( btype, etype ) )
           sys.exit(0)

        if len( bdate) != 10: print("Malformatted start date") ; sys.exit(1)
        if len( edate) != 10: print("Malformatted end   date") ; sys.exit(2)
        period=[]
        bdate =datetime.strptime( bdate , "%Y%m%d%H")
        edate =datetime.strptime( edate , "%Y%m%d%H")
        delta =timedelta(hours=int(cycle_inc))        
        while bdate <= edate:
              strdate=bdate.strftime("%Y%m%d%H")
              period.append( strdate )
              bdate += delta
        return period 


    def set_obs_list(self, obs_list ):      
        obsdict , obsvarno  , units =  self.obt.ConvDict()
        if len(obs_list ) == 0 :
          print("Can not process empty observation liste. At least one obstype is required" )
          sys.exit(1)

        if not isinstance ( obs_list  , list):
          print("Observation types variable expects liste." )
          sys.exit(2)
        

        obs_name =[ item["obs_name"] for item in obsdict  ]
        for obs in obs_list:
            if obs.strip() not in obs_name:
               print("Can t process the given obstype {}. Not predefined in the list of conventional obstypes".format( obs ))
               sys.exit(3)
         
        # Obs attributes          
        obs_names=[]
        codetype=[]
        obstype =[]
        varno   =[]
        lrange  =[]
        vertco  =[]
        sensor  =[]
        for item in  obsdict:
            obsname = item["obs_name"] 
            if obsname in obs_list:
                obs_names.append(item["obs_name"])
                varno.append    (item["varno"   ])
                codetype.append (item["codetype"])
                obstype.append  (item["obstype" ])
                lrange.append   (item["level_range"])
                vertco.append   (item["vertco_reference_1"])
                sensor.append   (item["sensor"  ] )
        return obs_names , codetype , obstype , varno , lrange , vertco ,sensor 




# TODO 
# class Satem:
#        def __init__():
#            pass
# DATA CETEGORIES  SATEM 
# What we need   ?    
#         SATEM 
# NAME         : sensor 
# AMSUA        : 3
# AMSUB        : 4
# MHS          :15 
# IASI         :16  
# ATMS         :19  
# MWHS         :73  
# SEVIRI       :29 
#        return None



# CONV OBSTYPE LIST
#  obs        varno  
# gpssol :    128
# synop  :    1,29,58,41,42,7,39
# dribu  :    
# airep  :    2,3,4
# airepl :    2,3,4
# radar  :    29,195
# temp   :    2,3,4,39,58,41,42
# templ  :    2,3,4,39,58,41,42 

class Conv:
    """
    class : instantiate Conv obs category  and corresponding SQL query
    """
    def __init__(self ):
        # ---> CAUTION !: NEVER CHANGE THE ORDER OF COLUMNS 
        #      TO ADD NEW ONEs , ONE CAN DO ONLY AN APPEND !
        self.cols      =[ "",
                          "obstype"                 ,  # 1
                          "codetype"                ,  # 2
                          "statid"                  ,  # 3
                          "varno"                   ,  # 4
                          "degrees(lat)"            ,  # 5
                          "degrees(lon)"            ,  # 6 
                          "vertco_reference_1"      ,  # 7
                          "date"                    ,  # 8
                          "time@hdr"                ,  # 9
                          "an_depar"                ,  # 10
                          "fg_depar"                ,  # 11
                          "obsvalue"                ,  # 12
                         ]

        # Considered tables & additional sql statement 
        ODB_CONSIDER_TABLES="/hdr/desc/body/radar/radar_body"  
        self.tables        = ["hdr","desc","body",] #"errstat", "radar_body"]
        self.tbl_env       = "/".join( self.tables  )
        self.other_sql     = "  (an_depar is not NULL) AND (fg_depar is not  NULL)  AND (datum_event1.fg2big@body = 0)"
        
        # ObsType list 
        self.conv_obs=[ 'gpssol' ,
                        'synop'  ,
                        'dribu'  ,
                        'airep'  ,
                        'airepl' ,
                        'radar'  ,
                        'temp'   ,
                        'templ'  ]
        obstype     = ObsType() 
        self.obs, self.varno , self.unit = obstype.ConvDict()
        return None

