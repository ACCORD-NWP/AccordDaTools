get.satName <- function(sats.in, sens.in)
{
        # NAMES DEFINITON
        satID  = c(1)
        satName  = c("gnss")
	
        sensID = c(0)
        sensName = c("ztd")

        names(satName)     = as.character(satID)
        names(sensName)    = as.character(sensID)

        return(list(satName[[as.character(sats.in)]], sensName[[as.character(sens.in)]]))
}

get.satInfo <- function(sensor.in)
{
        #----------------------------------------
        # CHANNEL SELECTION
        #----------------------------------------
        selChan = list()
        selChan[["0"]]  = c(0)

        outSelChan = selChan[[as.character(sensor.in)]]

        return(list(outSelChan))
}
