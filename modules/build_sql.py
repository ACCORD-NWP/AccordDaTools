# -*- coding:utf-8 -*- 
import os ,sys  
from  odb4py.utils   import SqlParser 



class SqlHandler:
    """
    Class : @ Parse the different observation dictionaries and build the 
              SQL query according to obstype, codetype, varno , sensor and level range  
            @ Checks the SQL statement before sending. 
    Returns : The sql statement according to variable and varno  

            Methods : BuildQuery  
                      CheckQuery


    """
    def __init__(self ):

        # For the moment !
        self.obstool_select=None 

        #self.derozier_select=None
        #self.jarvinen_select=None
        return None 


    def BuildQuery(self , **kwarg ):
        self.cols      =",".join(kwarg["columns" ])[1:]
        self.tabs      =",".join(kwarg["tables"  ])
        self.obs_      =kwarg["obs"]
        self.varno     =kwarg["obsvano"]
        self.ctype     =kwarg["codetype"]
        self.obst      =kwarg["obstype"]
        self.levels    =kwarg["lrange"]
        self.vtype     =kwarg["vertco_type"]
        self.vert_coord=kwarg["vertco"]
        self.sensor    =kwarg["sensor"        ]
        self.other     =kwarg["remaining_sql"]


        # Obstool columns and tables 
        self.obstool_select="SELECT  "+self.cols+" FROM " +self.tabs

        # Check obstype  
        type_list=[]
        if self.obst is not None:
           if isinstance (self.obst, list ):
              for tp in self.obst:
                  if tp is not None:
                     type_list.append (" obstype =="+str( tp ) )
           elif isinstance (self.obst, int  ):
              type_list.append (" obstype =="+str( self.obst ) )                
           else:
              print(self.obs_, " : obstype  must be integer or a list" )
              sys.exit()

        # Check varno  
        varno_list=[]
        if self.varno is not None:
           if isinstance (self.varno , list ) :
              for vr in self.varno:
                  if vr is not None:
                     varno_list.append (" varno =="+str( vr ) )
           elif  isinstance( self.varno , int  ):
               varno_list.insert(0, " varno =="+ str(self.varno)  )

           else:
              print(self.obs_ , ": varno must be integer ot list" )
              sys.exit()
        
        # Check codetype 
        """
        Not needed for the moment !
        ctype_list=[""]
        if self.ctype is not None:
            if isinstance( self.ctype,  list  ):
               for ct  in self.ctype:
                   if ct is not None:
                      ctype_list.append (" codetype =="+str( ct ) )
            elif isinstance( self.ctype , int  ):
                   ctype_list.append(  " codetype =="+str(self.ctype ) )
            else:
               print(self.obs_, ": codetype  must be integer or list "  )
               sys.exit()"""


        # Check sensor 
        sensor_list =[]
        if self.sensor != None:
            if isinstance   (self.sensor ,  list  ):
               for s in   self.sensor:
                   if s is not None:
                      sensor_list.append( " sensor=="+str(s)   )
            elif isinstance (self.sensor ,  int ):
                     sensor_list.append(  " sensor =="+str( self.sensor ) )
            else: 
                print(self.obs_, ": sensor  must be integer or list"  )
                sys.exit()


        # Check level range 
        level_list=[]
        if self.levels != None :
          if len (self.levels) <2  or  len( self.levels ) > 2:
             print ( "Level range must have two limites   [l1 , l2], but got argument:", self.levels  )
             sys.exit(0)
          elif not isinstance (self.levels[0], int ) or not isinstance ( self.levels [1], int):
             print( "One or the both level limites in the list  is/are not intger(s)" )
             sys.exit(0)
          elif self.levels[1] < self.levels[0]:
             print( "Bottom level limit can't be greater than top level limit" )
             sys.exit(0)
          else:
             l1 =str(self.levels[0] )
             l2 =str(self.levels[1] )
             level_cond="vertco_reference_1 >="+l1+" AND vertco_reference_1 <= "+l2 
             level_list.append(level_cond)

       
        where_cond_list=[]
        for tp in type_list:            
           if len( type_list) !=0 : 
               where_cond_list.append(  "".join( ( tp )  ) )
           for vr in varno_list:
               if  len( varno_list) !=0:
                   where_cond_list.append(  " AND ".join(  (tp, vr  ))   )
               if len(level_list) !=0:
                   for lv in level_list:
                       where_cond_list.append(  " AND ".join(  (tp, vr ,  lv  ))   )

        query=" " 
        if   self.other !=None and len(where_cond_list) == 0:
             query= self.obstool_select +" WHERE  "+self.other
        
        elif self.other !=None and len(where_cond_list) > 0:             
             for condition in where_cond_list:
                 query=self.obstool_select +" WHERE  " +  condition + "  AND  "+self.other 

        elif self.other ==None and len(where_cond) > 0:
             for condition in where_cond_list:
                 query= self.obstool_select  +" WHERE  " +  condition 
        
        print(query ) 
        #quit()
        return  query



    def CheckQuery(self  , query   ):
        p      =SqlParser()
        nfunc  =p.get_nfunc    ( query  )     # N Columns=N pure columns - N functions in the query 
        sql    =p.clean_string ( query  )     # Remove any unprintable character ( \r\t ....etc)
        return nfunc ,  sql 
