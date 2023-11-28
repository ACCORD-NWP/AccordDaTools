# -*- coding: utf-8 -*-
import os  
import sys  
sys.path.append("./modules")
from   math import  sqrt, pi   
import numpy as np  
import readgsa  



class GSA:
      """
      Class : CALL A FORTRAN ROUTINE readgsa.so  
              READ THE BACKGROUND COVARIANCES OF :
              TEMPERATURE ,Q SPECIFIC ,VORTICIY AND DIVERGENCE  

            : RETURNS VERTICAL AVERAGED SIGMA AND MEAN OF SIGMA 
              OF T,Q,VOR,DIV and Wind 
      """
     
      dpar={2:"Temperature"      ,
            3:"Specific humidity",
            4:"Vorticity"        ,
            5:"Divergence"       }          
      def __init__ (self,paths , cfile , nsmax , nflev , deltax , lverb ,lwrite):
          self.path  =paths  
          self.cfile =cfile 
          self.nsmax =int  (nsmax)
          self.nlev  =int  (nflev)
          self.deltax=float(deltax)
          self.lverb =bool(lverb)
          self.write =bool(lwrite)
          
          return None  

      def Write2File (self , varname , pname , value ):
          # OUT PATH ( BASEDIR/out)
          os.system( "mkdir -p "+ self.path["BASEDIR"]+"/out" )
          file_=self.path["BASEDIR"]+"/out/"+varname+"_"+pname

          outfile=open(file_  , "w")
          if isinstance ( value , list):
             for i,j in enumerate (value):
                 outfile.write(  str(i+1)+"    "+str(j)+"\n")
             outfile.close()
          else:
             outfile.write(str(value) )



      def ComputeStd(self, pcov):
          var=np.zeros((self.nlev ,self.nlev))
          std=[]
          for i in range(0,self.nlev):
             for j in range(0,self.nlev):
                var[i,j]= sum(  pcov[i,j,:]  )
        
          for i in range(self.nlev):
             std.append(sqrt(var[i,i]))
             mstd=sum( std )/len(std)
          return std , mstd

      def GetGSA( self, kpar ):
          nlev=None 
          if kpar == 2: 
             nlev=self.nlev + 1 
          else: nlev=self.nlev 
          cov,kret = readgsa.readcov( nlev, self.nsmax , kpar,self.cfile,self.lverb)
          if kret != 0:  
             print( "FAILED TO READ MATRICES FOR PARAMETER :" , dpar[kpar] )
          else:
             return cov  

      def GetSigmaB(self, ipar):
          tcov=self.GetGSA (2)   # TEMPERATURE  
          qcov=self.GetGSA (3)   # SPECIFIC Q 
          vcov=self.GetGSA (4)   # VORTICITY 
          dcov=self.GetGSA (5)   # DIVERGENCE 
        
          if ipar == 2:
             tstd_ver , mean_av_t=self.ComputeStd( tcov )
             if self.write == True: 
                self.Write2File ( "sigmab_profile", "t" ,  tstd_ver  )
                self.Write2File ( "sigmab_mean"   , "t" ,  mean_av_t )
             return tstd_ver , mean_av_t
          if ipar == 3:
             qstd_ver , mean_av_q=self.ComputeStd( qcov )
             if self.write == True: 
                self.Write2File ( "sigmab_profile", "q" ,  qstd_ver )
                self.Write2File ( "sigmab_mean"   , "q" ,  mean_av_q )
             return qstd_ver , mean_av_q
          if ipar == 4:
             vstd_ver , mean_av_v=self.ComputeStd( vcov )
             if self.write == True: 
                self.Write2File ( "sigmab_profile", "vor" , vstd_ver )
                self.Write2File ( "sigmab_mean"   , "vor" , mean_av_v) 
             return vstd_ver , mean_av_v
          if ipar == 5:
             dstd_ver , mean_av_d=self.ComputeStd( dcov )
             if self.write == True: 
                self.Write2File ( "sigmab_profile", "div" , dstd_ver )
                self.Write2File ( "sigmab_mean"   , "div" , mean_av_d)
             return dstd_ver , mean_av_d

          # COMPUTE WIND SPEED FROM VORTICITY AND DIV 
          # FOTRAN k is from 1 ..NSMAX  
          # PYTHON k is from 0 ..NSMAX-1

          # INIT 
          covuv=np.zeros((vcov.shape[0],vcov.shape[1],vcov.shape[2])) 

          # PREPARE LAGGED INDICES 
          zidx =[i for i in range(1,self.nsmax +1)  ]

          for i in range(0,self.nlev):    
             for j in range(0,self.nlev):
                for k in range(0,self.nsmax):
                    ik=zidx[k]     
                    z=-(pi*ik /((2*self.nsmax+3)*self.deltax*1000.0))**2
                    if i ==j:
                       covuv[i,j,k] = ((dcov[i,j,ik]+vcov[i,j,ik])/ ( -2.0*z ))

          uvstd_ver , mean_av_uv=self.ComputeStd( covuv ) 
          if ipar == 999:                   # SET 999 FOR UV (NO KPAR VALUE IN stabal FILE )
             if self.write == True: 
                self.Write2File ( "sigmab_profile", "uv" , uvstd_ver    )
                self.Write2File ( "sigmab_mean"   , "uv" , mean_av_uv   )
             return uvstd_ver , mean_av_uv
