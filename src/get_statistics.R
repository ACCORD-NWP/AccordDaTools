prepare_mean_stat <- function(data, lvar)
{
    rransum   = aggregate(list(ANSUM   = data$ansum  ), by=lvar, FUN=sum )
    rrfgsum   = aggregate(list(FGSUM   = data$fgsum  ), by=lvar, FUN=sum )
    rrfgncsum = aggregate(list(FGNCSUM = data$fgncsum), by=lvar, FUN=sum )
    rrfgsqr   = aggregate(list(FGSQR   = data$fgsqr  ), by=lvar, FUN=sum )
    rrnum     = aggregate(list(NOBS    = data$num    ), by=lvar, FUN=sum )

    # Prepare statistics for plotting
    out       = rrnum
    out$BSFG  = rrfgsum$FGSUM/rrnum$NOBS
    out$BSFGNC= rrfgncsum$FGNCSUM/rrnum$NOBS
    out$RMSFG = sqrt(rrfgsqr$FGSQR/rrnum$NOBS)
    out$SDFG  = sqrt((out$RMSFG)^2 - (out$BSFG)^2)

    return(out)
}

prepare_diag_cov <- function(data, lvar)
{
    # COV 
    ag1 = aggregate(list(ASUM1  = data$Asum1 ), by=lvar, FUN=sum )
    ag2 = aggregate(list(FGSUM1 = data$FGsum1), by=lvar, FUN=sum )
    ag3 = aggregate(list(FGSUM2 = data$FGsum2), by=lvar, FUN=sum )
    ag4 = aggregate(list(AFGSQR = data$AFGsqr), by=lvar, FUN=sum )
    ag5 = aggregate(list(FGSQR  = data$FGsqr ), by=lvar, FUN=sum )
    ag6 = aggregate(list(NOBS   = data$num   ), by=lvar, FUN=sum )
    # STD
    ag7 = aggregate(list(FGSQR1 = data$FGsqr1   ), by=lvar, FUN=sum )
    ag8 = aggregate(list(FGSQR2 = data$FGsqr2   ), by=lvar, FUN=sum )
    ag9 = aggregate(list(ASQR1  = data$Asqr1    ), by=lvar, FUN=sum )

    # Prepare statistics for plotting
    #---------------------------------
    # Covariance
    cov          = ag6
    cov$COV.HL   = (ag5$FGSQR/ag6$NOBS) - ((ag2$FGSUM1 * ag3$FGSUM2)/(ag6$NOBS)^2)
    cov$COV.DR.B = cov$COV.HL - ((ag4$AFGSQR/ag6$NOBS) - ((ag1$ASUM1 * ag3$FGSUM2)/(ag6$NOBS)^2))
    cov$COV.DR.R = (ag4$AFGSQR/ag6$NOBS) - ((ag1$ASUM1 * ag3$FGSUM2)/(ag6$NOBS)^2)

    # Standard deviation 
    cov$sigmaFG1 = sqrt( (ag7$FGSQR1/ag6$NOBS) - (ag2$FGSUM1/ag6$NOBS)^2 )
    cov$sigmaFG2 = sqrt( (ag8$FGSQR2/ag6$NOBS) - (ag3$FGSUM2/ag6$NOBS)^2 )
    cov$sigmaA1  = sqrt( (ag9$ASQR1/ag6$NOBS) - (ag1$ASUM1/ag6$NOBS)^2 )

    # Correlation
    cov$COR.HL = cov$COV.HL/(cov$sigmaFG1 * cov$sigmaFG2)
    cov$COR.DR.R = cov$COV.DR.R/(cov$sigmaA1 * cov$sigmaFG2)
    cov$COR.DR.B = cov$COV.DR.B/(cov$sigmaFG1 * cov$sigmaFG2)

    return(cov)
}

prepare_obs_error <- function(data, lvar)
{
    rransum   = aggregate(list(ANSUM   = data$ansum  ), by=lvar, FUN=sum )
    rrfgsum   = aggregate(list(FGSUM   = data$fgsum  ), by=lvar, FUN=sum )
    rrfgsqr   = aggregate(list(FGSQR   = data$fgsqr  ), by=lvar, FUN=sum )
    rrafsqr   = aggregate(list(AFSQR   = data$afsqr  ), by=lvar, FUN=sum )
    rrnum     = aggregate(list(NOBS    = data$num    ), by=lvar, FUN=sum )

    # Prepare statistics for plotting
    out       = rrnum

    # STD OF FG.DEP
    out$BSAN  = rransum$ANSUM/rrnum$NOBS
    out$BSFG  = rrfgsum$FGSUM/rrnum$NOBS
    out$RMSFG = sqrt(rrfgsqr$FGSQR/rrnum$NOBS)
    out$SDFG  = sqrt((out$RMSFG)^2 - (out$BSFG)^2)

    # STD OF DEROZIERS
    out$SDDER = sqrt((rrafsqr$AFSQR/rrnum$NOBS) - (out$BSFG * out$BSAN))

    return(out)
}
