#This script can be used for ascat obs.
#Be CAREFUL: Fill the path  in read0 and also the name of the ouput file , and the obs 

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

# Set binning interval (if NULL than default)
read0="/../$OBS"
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
lsat      = unique(dfData$sat)

for ( isat in lsat )
{ 
  msSAT  = subset(dfData,  sat==isat)
  lsens = unique(msSAT$sens)

  for ( isens in lsens )
  {

    # Get satellite channel selection for the instrument
    channels = get.satInfo(isens)[[1]]

    msSENS  = subset(msSAT, sat==isat & sens==isens & chan%in%channels)

    plchan   = unique(msSENS$chan)
    nchan    = length(plchan)

    csat  = get.satName(isat, isens)[1]
    csens = get.satName(isat, isens)[2]

    print(paste("Plotting ", csat, "/", csens, sep=""))

    if (nrow(msSENS) > 0)
    {
      ###########################################################
      # 	PREPARE MEAN STATISTICS
      ###########################################################
      if ( !is.null(max_distance) & !is.null(dist_interval) )
      {
        print(paste("New separation bin", dist_interval, "km up to", max_distance,"km", sep=""))
        lDint = seq(0, max_distance, dist_interval)
        cDint = seq(dist_interval/2, max_distance-(dist_interval/2), dist_interval)

        ldist = cut(msSENS$dist, lDint, labels=cDint, include.lowest=FALSE)
        monSENS = prepare_diag_cov(msSENS, list(CHAN=msSENS$chan, DIST=ldist, EXP=msSENS$Exp))
        monSENS$DIST = as.numeric(as.character(monSENS[,"DIST"]))

      }else{

        # OUT: COV.DR (Desroziers), COV.HL (Hollingsworth/Lonnberg) ~ CHAN, DIST, EXP
        monSENS = prepare_diag_cov(msSENS, list(CHAN=msSENS$chan, DIST=msSENS$dist, EXP=msSENS$Exp))
      }

      ###########################################################
      #            HORIZONTAL PLOTS
      ###########################################################
      #-----------------------------
      # 	COVARIANCE
      #-----------------------------
      pdf(paste("diag_cov_",isat,"_",isens,".pdf",sep=""))

      for (ichan in plchan)
      {
        mondata   = subset(monSENS,  CHAN==ichan)

        attach(mondata)

        # plot.horizontal(data, x, y)
        plot.horiz(     mondata, c("DIST"), c("COV.DR.B", "COV.HL"), legx = c("Desroziers.B", "FG_depar"),
                        main=paste(csat," / ",csens, " / Channel ", ichan, sep=""),
                        xlab="Distance [km]", ylab=expression("Covariance [" ~ K^2 ~ "]"),
                        LCOL.EXP=c("black"), LLWD=c(2,2), LLTY=c(2,1), LPCH=c(18,20), INCL0=TRUE, FIRST=2)

        detach(mondata)
        
      } # ichan

      dev.off()

      #-----------------------------
      # 	CORRELATIONS
      #-----------------------------
      pdf(paste("diag_cor_",isat,"_",isens,".pdf",sep=""))

      for (ichan in plchan)
      {
        mondata   = subset(monSENS,  CHAN==ichan & DIST>0)

        attach(mondata)

        # plot.horizontal(data, x, y)
        plot.horiz(     mondata, c("DIST"), c("COR.DR.R"), legx = c("Desroziers.R"),
                        main=paste(csat," / ",csens, " / Channel ", ichan, sep=""),
                        xlab="Distance [km]", ylab="Correlation", ylim=c(0,1),
                        LCOL.EXP=c("black"), LLWD=c(2), LLTY=c(2), LPCH=c(20), INCL0=TRUE)

        detach(mondata)

      } # ichan

      dev.off()

    }else{

      print(paste("Data for ", csat,"/", csens," are not available.", sep=""))

    } # test obsnum 
  } # isensor
} # isat
