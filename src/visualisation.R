set.visual <- function()
{
    col.exp= c("black", "red3", "blue3", "green3", "yellow3")

    list.setting = list(col.exp)
    return(list.setting)
}


plot.vert <- function(data, cx, cy, CNT = NULL, YREV = FALSE, LCOL.EXP = col.exp, LLWD = 2, LLTY = 1, LPCH = NA, legx = cx,  ...)
{

    # PROZATIM PRO JEDNU STATISTIKU
    #col.exp= set.visual()[[1]]

    cexp = unique(data$EXP)
    nexp = length(cexp)
    nobs = data$NOBS

    nstat = length(cx)

    x     = data[,cx]
    y     = unique(data[,cy])
    nx    = dim(data)[1]
    ny    = length(y)

    if (YREV==TRUE){ YLIM = rev(range(1:ny)) }else{ YLIM = range(1:ny) }

    par(mfrow=c(1,2), oma=c(1,1,1,1), mar=c(4,4,3,1))

    plot(range(x,na.rm=TRUE), range(1:ny), ylim=YLIM, type="n", axes=F, ...)

    axis(2, at=c(1:ny), labels=y, las=2, cex=.1, tck=1, lwd=.5, col="grey", lty=3)
    #axis(1)
    axis(1, at=pretty(as.numeric(unlist(x))), labels=chartr(".", ",", as.character(pretty(as.numeric(unlist(x))))))
    box()

    for (iexp in 1:nexp)
    { 
       for (istat in 1:nstat)
       {
         lines( subset(data, data$EXP==cexp[iexp])[,cx[istat]], c(1:ny), col=LCOL.EXP[istat], lwd=LLWD[istat], lty=LLTY[istat] )
         if (sum(!(is.na(LPCH)))>0){ points(subset(data, data$EXP==cexp[iexp])[,cx[istat]], c(1:ny), col=LCOL.EXP[iexp], pch=LPCH[istat]) }
       }
    }

    #----------------------------
    # PLOT NUMBER OF OBSERVATION
    #----------------------------
    par(mar=c(4,1,3,4))

    plot(range(c(0,nobs/1000)), range(1:ny), ylim=YLIM, type="n", axes=F, xlab=expression("Počet měření [" ~ x10^3 ~ "]"), ylab="", main=paste("NT:", CNT, "UTC"))

    axis(1)
    axis(2, at=c(1:ny), labels=FALSE, las=2, cex=.1, tck=1, lwd=.5, col="grey", lty=3)
    box()

    for (iexp in 1:nexp){ lines(subset(data, data$EXP==cexp[iexp])[,"NOBS"]/1000, c(1:ny), col="blue", lty=2, lwd=2) }

    legend("bottomleft", legx, col=LCOL.EXP, lty=LLTY[1:nstat], lwd=LLWD[1:nstat], pch=LPCH[1:nstat], bg="white", cex=.8)

}

plot.horiz <- function(data, cx, cy, CNT = NULL, YREV = FALSE, LCOL.EXP = col.exp, LLWD = 2, LLTY = 1, LPCH = NA, INCL0 = FALSE, legx = cy, FIRST=1, ...)
{
    # PROZATIM PRO JEDNU STATISTIKU
    col.exp = set.visual()[[1]]

    cexp = unique(data$EXP)
    nexp = length(cexp)
    nobs = data$NOBS
    
    nstat = length(cy)

    x     = unique(data[,cx])
    y     = data[,cy]
    nx    = length(x)
    ny    = dim(data)[1]

    if (INCL0==TRUE){ yran = range(c(y,0),na.rm=T) }else{ yran = range(y,na.rm=T) }
    if (cx=="DATE"){ xran = c(as.POSIXct(min(x),tz="MEST"),as.POSIXct(max(x),tz="MEST")) ; cycl="6 hours" }else{ xran = range(x) }

    par(mfrow=c(2,1), oma=c(1,1,1,1), mar=c(4,4.2,3,1))

    plot(xran, yran, type="n", axes=F, ...)

    if (cx=="DATE"){ 	axis.POSIXct(1, at=seq(xran[1], xran[2], by=cycl), format="%m%d%H", las=2, cex.axis=.8, tck=1, lwd=.5, col="grey", lty=3) }else{
      			axis(1, at = x, cex=1, tck=1, lwd=.8, col="grey", lty=3) }

    axis(2, las=2, cex=1, tck=1, lwd=1, col="grey", lty=3)
    box()

    for (iexp in 1:nexp)
    {
       for (istat in 1:nstat)
       {
         lines(x[FIRST:nx], subset(data, data$EXP==cexp[iexp])[FIRST:ny,cy[istat]], col=LCOL.EXP[iexp], lwd=LLWD[istat], lty=LLTY[istat] )
         if (sum(!(is.na(LPCH)))>0){ points(x, subset(data, data$EXP==cexp[iexp])[,cy[istat]], col=LCOL.EXP[iexp], pch=LPCH[istat]) }
       }
    }

    abline(h=0   , lty=2, col="grey")

    legend("topright", legx, col="black", lty=LLTY[1:nstat], lwd=LLWD[1:nstat], pch=LPCH[1:nstat], bg="white", cex=.8)

    #----------------------------
    # PLOT NUMBER OF OBSERVATION
    #----------------------------
    par(mar=c(4,4.2,1,1))

    visdata = rbind(subset(data, data$EXP==cexp[1])[,"NOBS"]/100)

    # BARPLOT
    barplot(	visdata, 
		axes=F, col=LCOL.EXP[1:nexp], space=c(1,1), ylab=expression("Observation number [" ~ x10^2 ~ "]"), xlab="", 
		legend.text = cexp, args.legend = list(x = "topright", bg = "white"), cex.names = x )

    axis(2, las=2, cex=1, tck=1, lwd=1, col="grey", lty=3)
    box()
}
