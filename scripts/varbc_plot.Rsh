#!/usr/bin/env Rscript
############################################################

args <- commandArgs(trailingOnly = TRUE)

if ( length(args) != 6 ) {print("Incorrect number of arguments"); quit()}

sat <- as.numeric(args[1])
sensor <- as.numeric(args[2])
channel <- as.numeric(args[3])
hh <- as.character(args[4])
exp <- as.character(args[5])
indir <- as.character(args[6])

if (sat == 206 ) {
     satb = "NOAA15" 
} else if (sat == 207 ) {
     satb = "NOAA16"
} else if (sat == 209 ) {
     satb = "NOAA18"
} else if (sat == 223 ) {
     satb = "NOAA19"
} else if (sat ==   3 ) {
     satb = "METOP-B"
} else if (sat ==   4 ) {
     satb = "METOP-A"
} else if (sat ==   5 ) {
     satb = "METOP-C"
} else if (sat ==   70 ) {
     satb = "METEOSAT-11"
} else {
     satb = "UNKNOWN"
}

if (sensor == 3 ) {
     sensorb = "AMSU-A" 
} else if (sensor == 4 ) {
     sensorb = "AMSU-B" 
} else if (sensor == 15 ) {
     sensorb = "MHS" 
} else if (sensor == 29 ) {
     sensorb = "SEVIRI" 
} else {
     sensorb = "UNKNOWN"
}

# name of the input file (for instance VARBC_209_3_9_03)
file_varbc=sprintf("%s/VARBC_%s_%s_%s_%s",indir,sat,sensor,channel,hh)

if (!file.exists(file_varbc)) { stop("the file does not exist") }

x <- read.table(file_varbc,fill=TRUE)
    
# General description of file columns:
# x[,1:2] year, month, day, hour
# x[,3:5] sat, sensor, channel
# x[,6] ndata
# x[,7] number of predictors 
# the number of predictors is always the same
#DEBUG print (x)

npred=x[1,7]

# parameter estimates 'params'
# first and last value for 'params'
n1=8; n2=n1+npred-1
# x[,n1:n2] parameter estimates 'param0'

# parameter backgroud 'param0'
# first and last value for 'param0'
n11=n1+npred
n22=n2+npred

# name of first and last predictors
n111=n11+npred
n222=n22+npred
# the number of the predictors is always the same
predic=as.character(x[1,n111:n222])

# color of predictors, if npred > 5 add more colors
col_pred=c("black","red","magenta","blue","cyan")

# time evolution of parameters 

#DEBUG print (x[,1])
#DEBUG print (x[,n1:n2])

####################################################################3

# 2 plots in a figure: 
# 1st time evolution of the varbc coefficents
# 2nd time evolution of the number of observations

file.png=sprintf("varbc_%s_%s_%s_%s.png",sat,sensor,channel,hh)
png(filename=file.png,width=720,height=720)

par(mfrow=c(2,1))
#par(mfrow=c(3,1))

# 1st Plot 
matplot(x[,1],x[,n1:n2],pch=19,col=col_pred,lwd=3,lty=1,xlab="day",ylab="bias parameter estimates")
matlines(x[,1],x[,n1:n2],type="l",col=col_pred,lwd=3,lty=1)

# to plot param0, uncomment next line 
#matplot(n,x[,n11:n22],type="l",col=col_pred,lwd=3,lty=2,ylab="bias parameter estimates",add=T)

#legend("topleft",predic,col=col_pred,lwd=3,lty=1)
legend("top",predic,col=col_pred,lwd=3,lty=1)
title(sprintf("%s     Sat= %s (%s)  Sensor= %s (%s)   Channel= %s  HH= %s UTC",exp,satb,sat,sensorb,sensor,channel,hh))
grid()

# 2nd Plot
plot(x[,1],x[,6],type="h",xlab="day",ylab="number of active observations",ylim=c(0,900))

# mean data number
mean_num_obs=round(mean(x[,6]),digits=0)
title(sprintf("Mean number of observations:  %s",mean_num_obs))


dev.off()

