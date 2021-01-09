library(data.table)
library(tictoc)
library(ggplot2)
library(dplyr)
library(signal)
library(roll)
library(pracma)
library(plotly)
source("peakdet.R")
library(rootSolve)
library(scales)
library(tidyr)
library(qpcR)
library(plyr)



BoatdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU", caption = "Select Boat File")))
PelvisdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU", caption = "Select Pelvis File")))
ThoraxdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU", caption = "Select Thorax File")))


# t = length(BoatdfAl$time_s)-1
# SampleRateTS <- matrix(0, ncol = 1, nrow = t)
# for (i in 1:t) {
#   SampleRateTS[i,] <- 1/(BoatdfAl$time_s[i+1]-BoatdfAl$time_s[i])
# }
# SampleRateB <- median(SampleRateTS)
# 
# 
# t = length(PelvisdfAl$time_s)-1
# SampleRateTS <- matrix(0, ncol = 1, nrow = t)
# for (i in 1:t) {
#   SampleRateTS[i,] <- 1/(PelvisdfAl$time_s[i+1]-PelvisdfAl$time_s[i])
# }
# SampleRateP <- median(SampleRateTS)
# 
# t = length(ThoraxdfAl$time_s)-1
# SampleRateTS <- matrix(0, ncol = 1, nrow = t)
# for (i in 1:t) {
#   SampleRateTS[i,] <- 1/(ThoraxdfAl$time_s[i+1]-ThoraxdfAl$time_s[i])
# }
# SampleRateT <- median(SampleRateTS)
# 
# 
# 
# # irregular time points at which data was sampled
# t <- BoatdfAl$time_s
# # measurements 
# y <- BoatdfAl[,4]
# 
# f <- approxfun(t,y)
# 
# # get interpolated values for time points 5, 20, 35, 50
# output <- f(seq(from=3,to=7000,by=0.01))




columns = colnames(BoatdfAl)


BoatDS = NULL

for (i in 1:ncol(BoatdfAl)){
  xvalues = BoatdfAl$time_s
  yvalues = unlist(BoatdfAl[i])
  output_x_vals <- seq(3,7000,by=1/225)
  
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  BoatDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


BoatDS2 <- as.data.frame(BoatDS)

colnames(BoatDS2) <-  columns


PelvisDS = NULL
for (i in 1:ncol(PelvisdfAl)){
  xvalues = PelvisdfAl$time_s
  yvalues = unlist(PelvisdfAl[i])
  output_x_vals <- seq(3,7000,by=1/225)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  PelvisDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


PelvisDS2 <- as.data.frame(PelvisDS)

colnames(PelvisDS2) <-  columns

ThoraxDS = NULL
for (i in 1:ncol(ThoraxdfAl)){
  xvalues = ThoraxdfAl$time_s
  yvalues = unlist(ThoraxdfAl[i])
  output_x_vals <- seq(3,7000,by=1/225)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  ThoraxDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


ThoraxDS2 <- as.data.frame(ThoraxDS)

colnames(ThoraxDS2) <-  columns

##############################################

#############################################


# indexmatchPelvis <- findInterval(BoatdfAl[1,1], PelvisdfAl[,1])
# 
# indexmatchThorax <- findInterval(BoatdfAl[1,1], ThoraxdfAl[,1])
# 
# PelvisdfAl2 <-  PelvisdfAl[indexmatchPelvis:(indexmatchPelvis+nrow(BoatdfAl)-1),]
# ThoraxdfAl2 <-  ThoraxdfAl[indexmatchThorax:(indexmatchThorax+nrow(BoatdfAl)-1),]
# 
# t = length(BoatdfAl$time_s)-1
# SampleRateTS <- matrix(0, ncol = 1, nrow = t)
# for (i in 1:t) {
#   SampleRateTS[i,] <- 1/(BoatdfAl$time_s[i+1]-BoatdfAl$time_s[i])
# }
# SampleRate <- median(SampleRateTS)
# 
# timeinterval = 1/SampleRate
# 
# 
# PelvisdfAl2 = subset(PelvisdfAl2, select = -c(time_s, unix_timestamp_microsec) )
# ThoraxdfAl2 = subset(ThoraxdfAl2, select = -c(time_s, unix_timestamp_microsec) )
# BoatdfAl = subset(BoatdfAl, select = -c(time_s, unix_timestamp_microsec) )
# 
# PelvisdfAl2 = subset(PelvisdfAl2, select = -c(time_s) )
# ThoraxdfAl2 = subset(ThoraxdfAl2, select = -c(time_s) )
# BoatdfAl = subset(BoatdfAl, select = -c(time_s) )
# 
# 
# BoatdfAl$time_s <-  BoatdfAl$time_s-BoatdfAl$time_s[1]
# 
# a = NULL
# 
# for (i in 2:length(BoatdfAl[,1])){
#   
#   BoatdfAl$time_s[i] <- timeinterval*(i-1)
#   
# }
# rm(time_s)
# time_s = seq(0,(length(BoatdfAl[,1])-1)*timeinterval,timeinterval)
# 
# rm(a)
# 
# BoatdfAl$time_s <-  time_s
# PelvisdfAl2$time_s <-  time_s
# ThoraxdfAl2$time_s <-  time_s


write.csv(BoatDS2, "C:/Users/aaron.beach/CaptureU/Boataligned.csv")
write.csv(PelvisDS2, "C:/Users/aaron.beach/CaptureU/Pelvisaligned.csv")
write.csv(ThoraxDS2, "C:/Users/aaron.beach/CaptureU/Thoraxaligned.csv")

