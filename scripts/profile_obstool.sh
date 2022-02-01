#!/bin/bash

#---------------------
# Experiment Setting
#---------------------
#lEXP="AIBr_fr"
lEXP="AIBr_fr2"
cEXP="oper"
#Be careful: also write exp name in Rscrits/preparing_gnss.R and monitoring_gnss.R
#---------------------
# Setting Date
#---------------------
sdate=2019021100
edate=2019022821
#sdate=2019031712
#edate=2019032606
cycle=3

#Observation
OBS=gnss_ztd
#-----------------------------------
# Data preparation or Visualisation
#-----------------------------------
#Preparation
LPREP=TRUE
LMON=FALSE
#or visualization
#LPREP=FALSE
#LMON=TRUE
