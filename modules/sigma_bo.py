# -*- coding: utf-8 -*-
import sys  
import os  
import pandas as pd 
from math import sqrt
from statistics    import stdev , mean 

from  odb         import Odb 
from  config_env  import TuneEnv  


# BASED ON THE PROGRAM ratio.F90  
# IN THE ORIGINAL VERSION  B. Strajnar 2010/10/26

class Predef:
      """
      Class : Returns predefined SIGMAO for t , bt, q and ke 
      """

      def __init__(self , paths , dates , lverb , lwrite):
          self.basedir=paths['BASEDIR'];
          self.rows_path= paths["ROWS_PATH"]
          self.lverb  =lverb ;
          self.lwrite =lwrite;
          # INIT LISTS 
          self.psigma=[]     ;
          self.dates =dates  ;
          self.rabso =2147483647.0     ; # Missing data Indicator  MDI 
          


      def Write2File (self , varname , pname , value ):
          # Output path   ( BASEDIR/out)
          os.makedirs(self.basedir+"/out" , exist_ok=True)
          file_=self.basedir+"/out/"+varname+"_"+pname

          outfile=open(file_  , "w")
          if isinstance ( value , list):
             for i,j in enumerate (value):
                 outfile.write(  str(i+1)+"    "+str(j)+"\n")
             outfile.close()
          else:
             outfile.write(str(value) )





      # Returned params from ReadOdbRows 
      def GetSigmaP(self , param):
          means=[]         
          # Get according to their index 
          if   param=="t" :  idx=0
          elif param=="bt":  idx=1
          elif param=="q" :  idx=2
          elif param=="ke":  idx=3
          else:
               print("Unkown parameter -->",param , ", possible short names : t , bt , q and ke" )
               sys.exit ()
          for  dt in self.dates:
               prows= Odb.ReadOdbRows (self.basedir,  self.rows_path , dt ,  "predef")
               if isinstance( prows , tuple ):
                  if isinstance(  prows[idx], pd.Series): 
                     pred_rows  = prows[idx].dropna().to_list()   # Clean NaN and convert to list  
                     self.psigma= self.psigma +pred_rows                  
                    
               # Get mean for each datetime
               if len( self.psigma) != 0:
                  nobs_p = len( self.psigma ) 
                  dte    = dt[0:8]
                  hh     = dt[8:10]
                  dh     = dte+" "+hh+":00:00"
                  means.append( dh+"    "+str( mean(self.psigma))+"   "+str(nobs_p) )
               else:
                  nobs_p = 0
                  dte= dt[0:8]
                  hh = dt[8:10]
                  dh =dte+" "+hh+":00:00"
                  means.append( dh+"    "+"None"+"    "+str(nobs_p))


          if self.lwrite == True:
             self.Write2File ("so_pred_means_vs_date" ,param , means  )

          if len(self.psigma) != 0:
             so_pred=sum( self.psigma)/len(self.psigma)
             if self.lwrite == True: self.Write2File ("so_pred_mean" ,param ,so_pred  )
          else:
             so_pred=None 
             if self.lwrite == True: self.Write2File ("so_pred_mean" ,param ,so_pred  )
          return so_pred






class Diag:
      """
      Class : 
            # Returns SIGMAO AND SIGMAB diagnostics using the "obs differences" method
            # METHOD : 
            #    HBH^T = d(b,a) * d(b,o)^T 
            #    R     = d(o,a) * d(b,o)^T
      """

      def __init__(self ,paths , dates, lverb , lwrite):
          self.basedir=paths["BASEDIR"]
          self.rows_path = paths["ROWS_PATH"]
          self.lverb   =lverb 
          self.lwrite  =lwrite 

          # INIT LISTS 
          self.sigb    =[]  ;
          self.sigo    =[]  ;
          self.sigb_out=[]  ;
          self.sigo_out=[]  ;
          self.dates =dates ; 
          self.rabso = 2147483647.0 ;  # Missing data 

      def Write2File (self , varname , pname , value ):
          # OUT PATH ( BASEDIR/out)
          os.makedirs(self.basedir+"/out" , exist_ok=True)
          file_=self.basedir+"/out/"+varname+"_"+pname
          outfile=open(file_  , "w")
          if isinstance ( value , list):
             for i,j in enumerate (value):
                 outfile.write(  str(i+1)+"    "+str(j)+"\n")
             outfile.close()
          else:
             outfile.write(str(value) )


      def ComputeSigmab(self,fg_dep , an_dep):
          s=0
          if len(fg_dep )!=0 and len(an_dep) !=0:
             for i in range(len(fg_dep )): 
                  diff=fg_dep[i] - an_dep[i] 
                  s   =s+ diff  *  fg_dep[i]
             sigmab=sqrt(s/len(fg_dep)) 
          else:
             sigmab=None 
          return sigmab 


      def ComputeSigmao(self,fg_dep , an_dep):
          s=0
          if len(fg_dep) != 0 and len(an_dep) != 0:
             for i in range(len(fg_dep)): 
                 s=s + (fg_dep[i]*an_dep[i] )
             sigmao = sqrt( s/len(fg_dep))
          else:
             sigmao = None 
          return sigmao 


      # 0 = t, 1=bt, 2=q, 3=ke  
      def GetSigmaD(self, param): 
          if param=="t" :  idx=0 ;
          if param=="bt":  idx=1 ;
          if param=="q" :  idx=2 ;
          if param=="ke":  idx=3 ;
          ncase=[]

          for  dt in self.dates:
               # Get the pd.Series according to the idx ==> param 
               fg_rows =Odb.ReadOdbRows (self.basedir ,self.rows_path , dt, "fg_diag")
               an_rows =Odb.ReadOdbRows (self.basedir ,self.rows_path , dt, "an_diag")

               # Check if it's pandas Series inside  a tuple  (as expected )
               if isinstance ( fg_rows , tuple  ) and  isinstance ( an_rows , tuple ):
                  if isinstance (fg_rows[idx], pd.Series ) and isinstance (an_rows[idx], pd.Series ) :
                     fg_rows  = fg_rows [idx]
                     an_rows  = an_rows [idx]

                     # Concat an and fg  
                     df = pd.concat([fg_rows , an_rows], axis=1)
               
                     # Clean the df ( align the available values without NaN )
                     df = df.dropna()
                     fg = df.iloc[:, 0].to_list()
                     an = df.iloc[:, 1].to_list()
                  
                     so = self.ComputeSigmao( fg, an ) # Observations
                     sb = self.ComputeSigmab( fg, an ) # Background
                     if so != None and sb != None:
                        self.sigo.append(so  )   
                        self.sigb.append(sb  )   
                        ncase.append(len(fg) )  # len(fg)=len(an)  
               
                     if self.lwrite==True: 
                        if so !=None and sb !=None :   # Available values 
                           dte= dt[0:8]
                           hh = dt[8:10]
                           dh =dte+" "+hh+":00:00"
                           self.sigo_out.append(dh+"    "+str(self.ComputeSigmao( fg, an ))+"   "+str(len(fg) ) )
                           self.sigb_out.append(dh+"    "+str(self.ComputeSigmab( fg, an ))+"   "+str(len(fg) ) )
                        else:                      
                           dte= dt[0:8];  hh = dt[8:10] ;  dh =dte+" "+hh+":00:00"
                           self.sigo_out.append(dh+"    "+"None"+"    "+str(len(fg) ) )
                           self.sigb_out.append(dh+"    "+"None"+"    "+str(len(fg) ) )

          if self.lwrite==True:
             self.Write2File("so_diag_means_vs_date", param , self.sigo_out)
             self.Write2File("sb_diag_means_vs_date", param , self.sigb_out)

          # Mean values 
          if len( self.sigo) !=0 and len(self.sigb ) !=0:             
             if self.lwrite==True:
                self.Write2File("so_diag_mean", param , mean(self.sigo))
                self.Write2File("sb_diag_mean", param , mean(self.sigb))
             return  mean(self.sigb) , mean(self.sigo), sum(ncase)
          else:
             if self.lwrite==True:
                self.Write2File("so_diag_mean", param , self.rabso)
                self.Write2File("sb_diag_mean", param , self.rabso) 
             return None , None , 0




class Ratios:
      def __init__(self,paths,  Nobs, rednmc ,so_pred, so_diag , sb_pred, sb_diag, lwrite ):
          self.basedir= paths["BASEDIR"]
          self.ptot    =  sum(Nobs) 
          self.pt      =  Nobs[0]
          self.pbt     =  Nobs[1]
          self.pq      =  Nobs[2]
          self.pke     =  Nobs[3]
          self.so_tp   = so_pred[0]  ; self.so_td =so_diag[0]  
          self.so_btp  = so_pred[1]  ; self.so_btd=so_diag[1]
          self.so_qp   = so_pred[2]  ; self.so_qd =so_diag[2]
          self.so_kep  = so_pred[3]  ; self.so_ked=so_diag[3]
       
          self.sb_tp   = sb_pred[0]   ; self.sb_td =sb_diag[0]
          self.sb_qp   = sb_pred[1]   ; self.sb_qd =sb_diag[2]  
          self.sb_kep  = sb_pred[2]   ; self.sb_ked=sb_diag[3]
 
          self.rednmc=rednmc  
            
          self.rabso= 2147483647.0
          self.lwrite=lwrite 
           
   
      def Write2File (self , varname , pname , value ):
          # OUT PATH ( BASEDIR/out)
          os.system( "mkdir -p "+ self.basedir+"/out" )
          file_=self.basedir+"/out/"+varname+"_"+pname

          outfile=open(file_  , "w" )
          if isinstance ( value , list):
             for i,j in enumerate (value):
                 outfile.write(  str(i+1)+"    "+str(j)+"\n")
             outfile.close()
          else:
             outfile.write(str(value) )





      def ComputeRatios(self, pred , diag , rednmc,  target  ):         
          if pred != None  and diag != None :
             if target   == "sigmao":
                ratio =diag/pred
                return ratio
             elif target == "sigmab":
                ratio =diag/(pred * float(rednmc))
                return ratio 
          else: 
             ratio =None 
             return ratio


      def RatioSo(self):
          """
          The averaged ratio is computed by weighted sum of 
          the ratio of each observation subset 
          roav=sqrt(roq**2*float(pq)/float(ptot)+rot**2*float(pt)/float(ptot)
                   robt**2*float(pbt)/float(ptot)+roke**2*float(pke)/float(ptot))

          """

          target="sigmao"
          rot = self.ComputeRatios( self.so_tp  , self.so_td   ,self.rednmc , target )          
          robt= self.ComputeRatios( self.so_btp , self.so_btd  ,self.rednmc , target )
          roq = self.ComputeRatios( self.so_qp  , self.so_qd   ,self.rednmc , target )
          roke= self.ComputeRatios( self.so_kep , self.so_ked  ,self.rednmc , target )
   
          if rot  == None : rot =0.
          if roq  == None : roq =0.
          if robt == None : robt=0. 
          if roke == None : roke=0.

          if  self.ptot  != 0:
              rrot  = rot **2* float(self.pt ) /float(self.ptot)
              rrobt = robt**2* float(self.pbt) /float(self.ptot)
              rroq  = roq **2* float(self.pq ) /float(self.ptot)
              rroke = roke**2* float(self.pke) /float(self.ptot)

              roav  = sqrt(  rroq +rrot + rrobt + rroke )     
              return (rot , robt , roq , roke ,roav)
          else:
              print("Number of total observations is equal to zero")
              return None 

          if self.lwrite==True:             
              lines= "ro_t   : "+str("%.4f" % rot)  +"  |   Nobs_t   : "+str(int(self.pt))   +"\n" \
                     "ro_bt  : "+str("%.4f" % robt) +"  |   Nobs_bt  : "+str(int(self.pbt))  +"\n" \
                     "ro_q   : "+str("%.4f" % roq)  +"  |   Nobs_q   : "+str(int(self.pq) )  +"\n" \
                     "ro_ke  : "+str("%.4f" % roke) +"  |   Nobs_ke  : "+str(int(self.pke))  +"\n" \
                     "\n" \
                     "ro_avg : "+str("%.4f" % roav) +"  |   Nobs_tot : "+str(int(self.ptot)) +"\n"
              self.Write2File( "ratios" ,"so" , lines )
          #return rot , robt , roq , roke ,roav 
 



      def RatioSb(self):
          """
               sb_*d: diagnosed  sigmab
               sb_*p: predefined sigmab
               rb*  : tuning ratio for sigmab  for each parameter 
               rbav : weighted average ratio for sigmab  

               rb=sbd/(sbp*rednmc)
               rbav=sqrt(rbq**2*pq/ptot+rbt**2*pt/ptot+rbke**2*pke/ptot)
          """

          target="sigmab"
          rbt  = self.ComputeRatios( self.sb_tp  , self.sb_td    ,self.rednmc , target )
          rbq  = self.ComputeRatios( self.sb_qp  , self.sb_qd    ,self.rednmc , target )
          rbke = self.ComputeRatios( self.sb_kep , self.sb_ked   ,self.rednmc , target )
          
          # AVERAGE 
          if rbt  == None : rbt =0.
          if rbq  == None : rbq =0.
          if rbke == None : rbke=0.

          if self.ptot != 0:              
             rrbt =rbt**2 *(self.pt  / self.ptot )
             rrbq =rbq**2 *(self.pq  / self.ptot )
             rrbke=rbke**2*(self.pke / self.ptot )
             rbav = sqrt( rrbt  + rrbq  + rrbke )

             if self.lwrite == True:
                lines=  "rb_t   : "+str("%.4f" % rbt)  +"  |   Nobs_t   : "+str(int(self.pt))   +"\n" \
                        "rb_q   : "+str("%.4f" % rbq)  +"  |   Nobs_q   : "+str(int(self.pq) )  +"\n" \
                        "rb_ke  : "+str("%.4f" % rbke) +"  |   Nobs_ke  : "+str(int(self.pke))  +"\n" \
                        "\n" \
                        "rb_avg : "+str("%.4f" % rbav) +"  |   Nobs_tot : "+str(int(self.ptot)) +"\n"
                self.Write2File( "ratios" ,"sb" , lines )

             return  (rbt , rbq , rbke , rbav)
          else:
             print( "Number of total innovations is equal to zero" )
             return None 

