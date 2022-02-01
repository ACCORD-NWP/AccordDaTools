read.ODB <- function(sdate, edate, cycle, dbname, rerun=FALSE)
{

  # Time initialization
  days <- seq(from=strptime(sdate, format="%Y%m%d%H"), to=strptime(edate, format="%Y%m%d%H"), by=cycle)

  # Save collected date to database
  database=paste(read, "/", dbname, "_", sdate, "_", edate, ".Rdat", sep="")

  if (file.exists(database) & rerun==FALSE)
  {
    print(paste("Reading file from ", database, " ..."))
    load(database)

  }else{
    
    dfData = data.frame()

    # READ STATISTICS
    for ( i in seq_along(days) )
    {
      idays = format(days[i],"%Y%m%d%H")

      for (j in seq_along(lexp))
      {
        iexp = lexp[j]

        subDir=paste(read,"/", iexp, "/stat_sats_", idays,".Rdat", sep="")

        print(paste("Read file", subDir,"..."))

        if (file.exists(subDir))
        {
          load(subDir)

          StatTab$Exp = lexp[j]

          # Merging data frames
          dfData = rbind(dfData, StatTab)

        }else{
           print(paste("Files", subDir,"does not exist."))
        }

      } # iexp
    } # idays

    save(dfData, file=database)
  } #fi exist

  return(dfData)
}
