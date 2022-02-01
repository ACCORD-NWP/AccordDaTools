
#########################################
# 	SETTINGS
#########################################
#january 2022. Jana. Some cleaning from sat data.
#This script can be used for gnssztd, radar, or any conv obs.And ascat. 
#Be CAREFUL: Fill the path input in read0 and also the name of the file

# Set satellite/sensor ID
mylsat  = c(223)
mylsens = c(3)

# Spatial separation bin for OmG
bin_int = 20 		# [km] : binning interval for pairs
bin_max_dist = 200 	# [km] : maximal distance for the bin_int

# Time separation distance 
time_btw_omg = 60	# [+/- min] : time_btw_omg = time between OmG/OmA in pairs
dist=20

#########################################
#	START SCRIPT
#########################################
# Load R-packages
library(sp)

# INPUTS
exp=""
yymmddnt="XXYYMMDDNTXX"

lDint = c(0, 1, seq(bin_int, bin_max_dist, bin_int))
cDint = c(0, seq(bin_int, bin_max_dist, bin_int))

# SET VARIABLES
#read="/scratch/ms/es/mdy/HARM_DiagTool/gnss_obs"
#save="/scratch/ms/es/mdy/HARM_DiagTool/gnss_obs"

read0="/.../...PATHINPUT"
read= paste(read0,"/",exp, sep="")
save= read

date = strptime(as.character(yymmddnt), format="%Y%m%d%H")
#lab_input.x = c("satid", "press", "sensor", "lat", "lon", "an_depar", "tdiff", "fg_depar", "obsvalue")
lab_input.x = c("satid","lat","lon","an_depar","fg_depar","obsvalue")


# READ DATA
#file = paste(read,"/",exp,"/ccma_gnss_", yymmddnt, sep="")
#file = paste(read,"/",exp,"/ccma_mon_", yymmddnt, sep="")
file = paste(read,"/ccma_mon_", yymmddnt, sep="")
#------------------------------------
# 	RUN PREPARATION
#------------------------------------
if (file.exists(file)){ 

  #rdata = read.table(paste(file,sep=""), col.names=lab_input.x)
  data = read.table(paste(file,sep=""), col.names=lab_input.x)
  newdata = data.frame()

  # DATA SELECTION
  #data = subset(rdata, abs(tdiff) <= time_btw_omg)

  #------------------------------------
  # START DISTANCE CALCULATION
  #------------------------------------
  

        # Distances between satellite pixels
  	# matrix(a,b) : diagonal(0 at points), non-diagonal(distance btw pixels)
  	# Distance is determined by spDists

        latlon  <- SpatialPoints(coords = data[,c("lon","lat")], proj4string=CRS("+proj=longlat +datum=WGS84"))
        matdist <- spDists(latlon, latlon, longlat=T)

        print(paste("----> LatLon"))

        tabdist = as.table(matdist)

        nobs = length(data$obsvalue)

  	colnames(tabdist) = 1:nobs
  	rownames(tabdist) = 1:nobs

  	rdfnewdata = as.data.frame(tabdist)
  	names(rdfnewdata) = c("n1","n2","dist")

        #dfnewdata = subset(rdfnewdata, dist>0 & dist<=bin_max_dist)
        dfnewdata = subset(rdfnewdata, dist<=bin_max_dist)

        print(paste("----> Data Table"))

        dfnewdata$OA1 = data[dfnewdata[,"n1"], "an_depar"]
        dfnewdata$OA2 = data[dfnewdata[,"n2"], "an_depar"]
        dfnewdata$FG1 = data[dfnewdata[,"n1"], "fg_depar"]
        dfnewdata$FG2 = data[dfnewdata[,"n2"], "fg_depar"]
                 
	newdata = rbind(newdata, dfnewdata)

  
  snewdata = subset(newdata, dist<=bin_max_dist)
  ldist = cut(snewdata$dist, lDint, labels=cDint, include.lowest=TRUE)

  print(unique(ldist))

  attach(snewdata)

  ###########################################
  # 		SAVE SCORES
  ###########################################
  # Save scores for each day 
  # --> less demaning for memory
  # --> to speed-up the diagnostic 
  #==============================================================
  # COV(XY) = E[X*Yt] - E[X]*E[Yt] 
  # save: 
  # sum[X]; sum[Yt]; sum[X*Yt]; num[X]
  #
  # COV(OA1,FG2) = E[OA1*FG2] - E[OA1]*E[FG2]
  # OA1 = an_depar
  # FG2 = fg_depar
  # save: 
  # sum[OA1]; sum[FG2] ;sum[OA1*FG2]; num[FG2]
  #==============================================================
  # STD(X) = sqrt( E[X^2] - (E[X])^2 )
  # save: 
  # sum[FG1]   ; sum[FG2]   ; sum[OA1]
  # sum[FG1^2] ; sum[FG2^2] ; sum[OA1^2]
  #==============================================================
  # COVARIANCE, CORRELATIONS
  StatTab        = aggregate(list(Asum1   = OA1)		, list(dist=ldist), FUN=sum    )
  StatTab$FGsum1 = aggregate(list(FGSUM1  = FG1)		, list(dist=ldist), FUN=sum    )$FGSUM1
  StatTab$FGsum2 = aggregate(list(FGSUM2  = FG2)		, list(dist=ldist), FUN=sum    )$FGSUM2
  StatTab$AFGsqr = aggregate(list(AFGSQR  = OA1*FG2)		, list(dist=ldist), FUN=sum    )$AFGSQR
  StatTab$FGsqr  = aggregate(list(FGSQR   = FG1*FG2)		, list(dist=ldist), FUN=sum    )$FGSQR
  StatTab$num    = aggregate(list(NOBS    = FG2)		, list(dist=ldist), FUN=length )$NOBS

  # STD
  StatTab$FGsqr1 = aggregate(list(FGSQR1  = FG1*FG1)		, list(dist=ldist), FUN=sum )$FGSQR1
  StatTab$FGsqr2 = aggregate(list(FGSQR2  = FG2*FG2)		, list(dist=ldist), FUN=sum )$FGSQR2
  StatTab$Asqr1  = aggregate(list(ASQR1   = OA1*OA1)		, list(dist=ldist), FUN=sum )$ASQR1

  detach(snewdata)

  # Additional informations
  StatTab$date   = date
  StatTab$dist   = as.numeric(as.character(StatTab[,"dist"]))

  # SAVE STATISTICS
  #save(StatTab, file=paste(save,"/",exp,"/stat_sats_",yymmddnt,".Rdat",sep=""))
  save(StatTab, file=paste(save,"/stat_sats_",yymmddnt,".Rdat",sep=""))

}else{

  print(paste("File",file,"does not exist!!"))

}

rm(list = ls())
