#This script can be used for gnssztd, radar, or any conv obs.
#Be CAREFUL: Fill the path  in read0 and also the name of the ouput file at line 75 and 109, and the obs at line 93 and 122

# Load packages
source("get_statistics.R")
source("get_satinfo.R")
source("visualisation.R")
source("read_odb.R")

# Inputs
lexp = c("XXEXPXX")
cexp = c("XXCEXPXX")
sdate=""
edate=""
cyc=""
exp=""

# Set binning interval (if NULL than default)
#read="/scratch/ms/es/mdy/HARM_DiagTool/gnss_obs"
read0="/AccordDATools/scripts"
read= paste(read0,"/",exp, sep="")


dist_interval = NULL   # [km]
max_distance  = NULL   # [km]

# -----------------------------------------
#             DEFINITION
# -----------------------------------------
cyc.txt = paste(cyc,"hours")

# -----------------------------------------
# 		READ DATA 
# -----------------------------------------
collEXP = paste(lexp, collapse="_")
dbname = paste("source_sats_", collEXP, sep="")

# Read data from ODB
dfData = read.ODB(sdate, edate, cycle=cyc.txt, dbname, rerun=FALSE)

# -----------------------------------------
# 		VISUALISATION
# -----------------------------------------
daterange = c(as.POSIXct(min(dfData$date),tz="MEST"),as.POSIXct(max(dfData$date),tz="MEST"))

        
   # if (nrow(msSENS) > 0)
   # {
      ###########################################################
      # 	PREPARE MEAN STATISTICS
      ###########################################################
      if ( !is.null(max_distance) & !is.null(dist_interval) )
      {
        print(paste("New separation bin", dist_interval, "km up to", max_distance,"km", sep=""))
        lDint = seq(0, max_distance, dist_interval)
        cDint = seq(dist_interval/2, max_distance-(dist_interval/2), dist_interval)

        ldist = cut(dfData$dist, lDint, labels=cDint, include.lowest=FALSE)
        monSENS = prepare_diag_cov(dfData, list(DIST=ldist, EXP=dfData$Exp))
        monSENS$DIST = as.numeric(as.character(monSENS[,"DIST"]))

      }else{

        # OUT: COV.DR (Desroziers), COV.HL (Hollingsworth/Lonnberg)  DIST, EXP
        monSENS = prepare_diag_cov(dfData, list(DIST=dfData$dist, EXP=dfData$Exp))
      }

      ###########################################################
      #            HORIZONTAL PLOTS
      ###########################################################
      #-----------------------------
      # 	COVARIANCE
      #-----------------------------
      ##pdf(paste("diag_cov_",isat,"_",isens,".pdf",sep=""))
      #pdf(paste("diag_cov_gnss_ztd",".pdf",sep=""))
      png(paste("diag_cov_OBS",".png",sep=""))

      
      #for (ichan in plchan)
      #{
        #mondata   = subset(monSENS,  CHAN==ichan)
         mondata = monSENS
	 
        attach(mondata)
        csat=1
	csens=2
	ichan=3
        # plot.horizontal(data, x, y)
        #plot.horiz(     mondata, c("DIST"), c("COV.DR.B", "COV.HL"), legx = c("Desroziers.B", "FG_depar"),
        #                main=paste(csat," / ",csens, " / Channel ", ichan, sep=""),
        #               xlab="Distance [km]", ylab=expression("Covariance [" ~ K^2 ~ "]"),
        #               LCOL.EXP=c("black"), LLWD=c(2,2), LLTY=c(2,1), LPCH=c(18,20), INCL0=TRUE, FIRST=2)
        plot.horiz(     mondata, c("DIST"), c("COV.DR.B", "COV.HL"), legx = c("Desroziers.B", "FG_depar"),
                        main=paste(" OBS ", sep=""),
                       xlab="Distance [km]", ylab=expression("Covariance [" ~ K^2 ~ "]"),
                       LCOL.EXP=c("black"), LLWD=c(2,2), LLTY=c(2,1), LPCH=c(18,20), INCL0=TRUE, FIRST=2)


        detach(mondata)
        
      #} # ichan

      dev.off()

      #-----------------------------
      # 	CORRELATIONS
      #-----------------------------
      ##pdf(paste("diag_cor_",isat,"_",isens,".pdf",sep=""))
      #pdf(paste("diag_cor_gnss_ztd",".pdf",sep=""))
       png(paste("diag_cor_gnss_ztd",".png",sep=""))
     
        #mondata   = subset(monSENS,  CHAN==ichan & DIST>0)
         mondata   = monSENS
	 
        attach(mondata)

        # plot.horizontal(data, x, y)
       #plot.horiz(     mondata, c("DIST"), c("COR.DR.R"), legx = c("Desroziers.R"),
       #               main=paste(csat," / ",csens, " / Channel ", ichan, sep=""),
       #                xlab="Distance [km]", ylab="Correlation", ylim=c(0,1),
       #                LCOL.EXP=c("black"), LLWD=c(2), LLTY=c(2), LPCH=c(20), INCL0=TRUE)
        plot.horiz(     mondata, c("DIST"), c("COR.DR.R"), legx = c("Desroziers.R"),
                       main=paste(" GNSS ZTD ", sep=""),
                        xlab="Distance [km]", ylab="Correlation", ylim=c(0,1),
                        LCOL.EXP=c("black"), LLWD=c(2), LLTY=c(2), LPCH=c(20), INCL0=TRUE)


        detach(mondata)

      

      dev.off()

    #}else{

      #print(paste("Data for ", csat,"/", csens," are not available.", sep=""))
      print(paste("Data are not available.", sep=""))

    #} # test obsnum 
#  } # isensor
#} # isat
