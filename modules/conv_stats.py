#-*-coding:utf-8 -*- 
import os 
import pandas as pd 
import numpy as np 
from   collections import defaultdict 

# obstool modules 
from .handle_df  import GroupDf  



class DHLStat:
    """
    Class :  Compute the diffrent statistics
             covariance, correlation and standard deviations 
             from FG1, FG2,OA1 and OA2 departures in observation space 

             Following the methods :
             Desroziers , Hollingsworth,Lonneberg ( DHL )
             
          functions :
                    getCov  
                    getSig  
                    getCor  
    """
    def __init__(self, df, max_dist =100, bin_dist =10 , delta_t =60 ):

        # Stats 
        # Get the values in the middle of the bins (the maximum distance and bin interval could be  reset !!)
        # DEFAULT VALUES 
        self.dist_max=100 #Km
        self.bin_int =10  #Km

        # Overwrite if different values  
        if max_dist != 100:
           self.dist_max =  max_dist
           print("New value for maximum distance has been set. Maximum distance value = {} Km".format( self.dist_max ) )
        else:
           print("Default value for maximum distance is used : {} Km".format(  self.dist_max )  )

        if bin_dist != 10 : 
           self.bin_int =  bin_dist
           print("New binning distance has been set.  bin_interval= {} Km".format(  self.bin_int) )
        else:
           print("Default value for binning interval is used : {} Km".format(  self.bin_int ) )

        self.gp       =GroupDf ()
        self.merged_df=df  
        return None 



    def getCov(self , var , inplace=None  ):
        d1,d2, d3, d4, d5, d6, d7, d8 , dobs , _ ,  dt1, dt2 , var =self.gp.GroupByBins (self.merged_df, self.dist_max, self.bin_int )
        
        # Bins distances 
        dist_list = d1.index.to_list() 
        
        # Var list and period  
        lvar    = [var]* len(dist_list)
        lperiod = [ str(dt1) +"_"+str(dt2) for _ in range(len( dist_list ))  ]       
        
        # Nobs 
        nobs=list(dobs )

        # HL (Holingsworth-LÖnnberg )
        t1 =  np.divide  (d5 ,dobs) 
        t2 =  np.divide  ( np.multiply(d2 ,d3  ) , np.power( dobs, 2 ))
        cov_hl=  np.subtract(t1, t2  )

        # Desroziers B 
        tb1 = np.divide( d4, dobs )
        tb2 = np.divide( np.multiply(d1, d3   ) , np.power(dobs, 2 ) )
        drb_ =np.subtract( tb1, tb2 )
        cov_drB =np.subtract(cov_hl , drb_  )

        # Desroziers R 
        tr1 = np.divide  ( d4 , dobs  )
        tr2 = np.divide  (np.multiply( d1 , d3 ) , np.power(dobs , 2 ) )
        cov_drR = np.subtract(tr1 , tr2 )

        # Covariances 
        df_cov =  pd.DataFrame ({ 
                                   "dist"     :dist_list,
                                   "nobs"     :nobs     ,
                                   "var"      :lvar     ,
                                   "period"   :lperiod  ,
                                   "COV_HL"   :cov_hl   ,
                                   "COV_DR-B" :cov_drB  ,
                                   "COV_DR-R" :cov_drR  } ).astype( {
                                                             "nobs"    :"int32"   ,
                                                             "var"     :"category",
                                                             "period"  :"category",
                                                             "COV_HL"  :"float64" ,
                                                             "COV_DR-B":"float64" ,
                                                             "COV_DR-R":"float64"
                                                             }) 
       
        df_cov       = df_cov.reset_index(drop=True).dropna(inplace=False ).reset_index()
        if inplace ==True :
           return cov_hl , cov_drB, cov_drR   # inside the class 
        else: 
           return df_cov  
        

    def getSig  ( self , var , inplace =None ):

        d1,d2, d3, d4, d5, d6, d7, d8 , dobs , _ ,  dt1, dt2 , var   =self.gp.GroupByBins (self.merged_df , self.dist_max  ,self.bin_int )

        # Bins distances 
        dist_list = d1.index.to_list() 
        
        # Var list and period  
        lvar    = [var]* len(dist_list)
        lperiod = [ str(dt1) +"_"+str(dt2) for _ in range(len( dist_list ))  ]       
        
        # Nobs 
        nobs=list(dobs )

        # SIGMA FG1
        st1=np.divide( d6 , dobs)
        st2=np.divide( d2 , dobs)**2
        sigma_fg1=np.sqrt(  np.subtract(st1, st2 ))
        del st1 , st2 

        # SIGMA FG2 
        st1=np.divide( d7 ,dobs )
        st2=np.divide( d3 ,dobs )**2
        sigma_fg2=np.sqrt( np.subtract(st1, st2 ))
        del st1, st2

        # SIGMA A1
        st1=np.divide( d8 ,dobs )
        st2=np.divide( d1 ,dobs )**2
        sigma_a1=np.sqrt( np.subtract(st1, st2  )) 
        df_sig =pd.DataFrame({              
                               "dist"     :dist_list   ,
                               "nobs"     :nobs        ,
                               "var"      :lvar        ,
                               "period"   :lperiod     ,
                               "sigma_fg1": sigma_fg1  ,
                               "sigma_fg2": sigma_fg2  ,
                               "sigma_a1" : sigma_a1
                                }).astype( {   "nobs"     :"int32"   ,
                                                "var"     :"category",
                                              "period"    :"category",
                                               "sigma_fg1":"float64" ,
                                               "sigma_fg2":"float64" ,
                                               "sigma_a1" :"float64"
                                                             })

        df_sig = df_sig.reset_index(drop=True).dropna(inplace=False ).reset_index()   


        if inplace ==True:
           return sigma_fg1 , sigma_fg2, sigma_a1  # used inside the class 
        else:
           return df_sig


    def getCor (self,var ,  inplace=None ):
        d1 ,_, _ , _ , _ , _ , _ , _ , dobs , _ ,  dt1, dt2 , var  =self.gp.GroupByBins (self.merged_df , self.dist_max  ,self.bin_int )

        # Bins distances 
        dist_list = d1.index.to_list()

        # Var list and period  
        lvar    = [var]* len(dist_list)
        lperiod = [ str(dt1) +"_"+str(dt2) for _ in range(len( dist_list ))  ]
        
        # Nobs 
        nobs=list(dobs )

        # COV & Sigma 
        cov_hl, cov_drB, cov_drR =self.getCov  ( var , inplace =True )
        sfg1 , sfg2 , sa1        =self.getSig  ( var , inplace =True )


        # H.L and DRZ  CORRELATIONS
        cor_hl  =np.divide( cov_hl , np.multiply( sfg1 , sfg2  ) )
        cor_drB =np.divide( cov_drB, np.multiply( sfg1 , sfg2  ) )
        cor_drR =np.divide( cov_drR, np.multiply( sa1  , sfg2  ) )


        df_cor=pd.DataFrame({ 
                     "dist"     : dist_list    ,
                     "nobs"     : nobs         ,
                     "var"      : lvar         ,
                     "period"   : lperiod      ,    
                     "COR_HL"   : cor_hl       ,
                     "COR_DR-R" : cor_drR      ,
                     "COR_DR-B" : cor_drB
                              } ).astype( {   "nobs":"int32"   ,
                                              "var" :"category",
                                            "period":"category",
                                         "COR_HL"   :"float64" ,
                                         "COR_DR-R" :"float64" ,
                                         "COR_DR-B" :"float64"
                                                             }) 


        df_cor  = df_cor.reset_index(drop=True).dropna(inplace=False ).reset_index()   

        if inplace ==True:
           return cor_hl , cor_drB , cor_drR
        else:
           return df_cor 


    def getStatFrame (self , var ):
        d1 ,_, _ , _ , _ , _ , _ , _ , dobs , _ ,  dt1, dt2 , var  =self.gp.GroupByBins (self.merged_df,  self.dist_max  ,self.bin_int )

        # Bins distances 
        dist_list = d1.index.to_list()
        
        # Var list and period  
        lvar    = [var]* len(dist_list)
        lperiod = [ str(dt1) +"_"+str(dt2) for _ in range(len( dist_list ))  ]
        
        # Nobs 
        nobs=list(dobs )

        # Gather all Statistics  
        cov_hl, cov_drB, cov_drR     =self.getCov  ( var , inplace =True )
        cor_hl, cor_drB, cor_drR     =self.getCor  ( var , inplace =True )
        sigma_fg1,sigma_fg2,sigma_a1 =self.getSig  ( var , inplace =True )
       
        # DataFrame: Contains  the Desroziers , Hollingsworth/Lonnberg   Cov, Sigma and Corr 
        drhl_frame={ 
                     "dist"     :dist_list    ,
                     "nobs"     :nobs         ,
                     "var"      :lvar         ,
                     "period"   :lperiod      ,
                     "COV_HL"   :cov_hl       , 
                     "COV_DR-B" :cov_drB      ,
                     "COV_DR-R" :cov_drR      ,
                     "sigma_FG1":sigma_fg1    ,
                     "sigma_FG2":sigma_fg2    ,
                     "sigma_a1" :sigma_a1     ,
                     "COR_HL"   :cor_hl       , 
                     "COR_DR-R" :cor_drR      ,
                     "COR_DR-B" :cor_drB      
                  }

        # Return statistics DF 
        # Remark that the dataframe is rounded to 4 decimal digits 
        # It's done to allow a consistent comparison with the values given by R code 
        stat_frame =pd.DataFrame (   drhl_frame  ).astype({
                                              "nobs":"int32"   ,     
                                              "var" :"category",      
                                            "period":"category",      
                                         "COV_HL"   :"float64" ,      
                                         "COV_DR-R" :"float64" ,      
                                         "COV_DR-B" :"float64" ,
                                         "sigma_FG1":"float64" ,      
                                         "sigma_FG2":"float64" ,      
                                         "sigma_a1" :"float64" ,   
                                         "COR_HL"   :"float64" ,      
                                         "COR_DR-R" :"float64" ,      
                                         "COR_DR-B" :"float64"      
                                                             }).round(4)



        stat_frame  = stat_frame.dropna(inplace=False ).reset_index(drop=True) 
        return stat_frame      
        # To be plotted   !!!
