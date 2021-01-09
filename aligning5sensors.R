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



S1lowdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S1 low File")))
S1highdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S1 high File")))
S2lowdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S2 low File")))
S2highdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S2 high File")))
S3lowdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S3 low File")))
S3highdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S3 high File")))
S4lowdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S4 low File")))
S4highdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S4 high File")))
S5lowdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S5 low File")))
S5highdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Javelin/1", caption = "Select S5 high File")))

t = length(S2dfAl$time_s)-1
SampleRateTS <- matrix(0, ncol = 1, nrow = t)
for (i in 1:t) {
 SampleRateTS[i,] <- 1/(S2dfAl$time_s[i+1]-S2dfAl$time_s[i])
}
SampleRate <- median(SampleRateTS)

# SampleRateB <- median(SampleRateTS)
# 
# 
# t = length(S2dfAl$time_s)-1
# SampleRateTS <- matrix(0, ncol = 1, nrow = t)
# for (i in 1:t) {
#   SampleRateTS[i,] <- 1/(S2dfAl$time_s[i+1]-S2dfAl$time_s[i])
# }
# SampleRateP <- median(SampleRateTS)
# 
# t = length(S3dfAl$time_s)-1
# SampleRateTS <- matrix(0, ncol = 1, nrow = t)
# for (i in 1:t) {
#   SampleRateTS[i,] <- 1/(S3dfAl$time_s[i+1]-S3dfAl$time_s[i])
# }
# SampleRateT <- median(SampleRateTS)
# 
# 
# 
# # irregular time points at which data was sampled
# t <- S1dfAl$time_s
# # measurements 
# y <- S1dfAl[,4]
# 
# f <- approxfun(t,y)
# 
# # get interpolated values for time points 5, 20, 35, 50
# output <- f(seq(from=3,to=7000,by=0.01))




columns = colnames(S1lowdfAl)
columns2 = c("unix_timestamp_microsec", "time_s", "highg_ax_m.s.s", "highg_ay_m.s.s", "highg_az_m.s.s")  


S1lowDS = NULL

for (i in 1:ncol(S1lowdfAl)){
  xvalues = S1lowdfAl$time_s
  yvalues = unlist(S1lowdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S1lowDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S1lowDS2 <- as.data.frame(S1lowDS)

colnames(S1lowDS2) <-  columns


S1highDS = NULL
for (i in 1:ncol(S1highdfAl)){
  xvalues = S1highdfAl$time_s
  yvalues = unlist(S1highdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S1highDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S1highDS2 <- as.data.frame(S1highDS)

colnames(S1highDS2) <-  columns2


AlignedS1 = cbind(S1lowDS2, S1highDS2[,3:5])



S2lowDS = NULL
for (i in 1:ncol(S2lowdfAl)){
  xvalues = S2lowdfAl$time_s
  yvalues = unlist(S2lowdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S2lowDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S2lowDS2 <- as.data.frame(S2lowDS)

colnames(S2lowDS2) <-  columns

S2highDS = NULL
for (i in 1:ncol(S2highdfAl)){
  xvalues = S2highdfAl$time_s
  yvalues = unlist(S2highdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S2highDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S2highDS2 <- as.data.frame(S2highDS)

colnames(S2highDS2) <-  columns2


AlignedS2 = cbind(S2lowDS2, S2highDS2[,3:5])
###
S3lowDS = NULL
for (i in 1:ncol(S3lowdfAl)){
  xvalues = S3lowdfAl$time_s
  yvalues = unlist(S3lowdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S3lowDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S3lowDS2 <- as.data.frame(S3lowDS)

colnames(S3lowDS2) <-  columns

###
S3highDS = NULL
for (i in 1:ncol(S3highdfAl)){
  xvalues = S3highdfAl$time_s
  yvalues = unlist(S3highdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S3highDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S3highDS2 <- as.data.frame(S3highDS)

colnames(S3highDS2) <-  columns2

AlignedS3 = cbind(S3lowDS2, S3highDS2[,3:5])

###
S4lowDS = NULL
for (i in 1:ncol(S4lowdfAl)){
  xvalues = S4lowdfAl$time_s
  yvalues = unlist(S4lowdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S4lowDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S4lowDS2 <- as.data.frame(S4lowDS)

colnames(S4lowDS2) <-  columns

###
S4highDS = NULL
for (i in 1:ncol(S4highdfAl)){
  xvalues = S4highdfAl$time_s
  yvalues = unlist(S4highdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S4highDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S4highDS2 <- as.data.frame(S4highDS)

colnames(S4highDS2) <-  columns2

AlignedS4 = cbind(S4lowDS2, S4highDS2[,3:5])

###
S5lowDS = NULL
for (i in 1:ncol(S5lowdfAl)){
  xvalues = S5lowdfAl$time_s
  yvalues = unlist(S5lowdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S5lowDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S5lowDS2 <- as.data.frame(S5lowDS)

colnames(S5lowDS2) <-  columns

###
S5highDS = NULL
for (i in 1:ncol(S5highdfAl)){
  xvalues = S5highdfAl$time_s
  yvalues = unlist(S5highdfAl[i])
  output_x_vals <- seq(6,4880,by=1/1600)
  #
  # compute the interpolated values; this would be done for each input time series
  #
  #interp_output[[,i]]<- 
  
  S5highDS[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
}


S5highDS2 <- as.data.frame(S5highDS)

colnames(S5highDS2) <-  columns2

AlignedS5 = cbind(S5lowDS2, S5highDS2[,3:5])

##############################################

#############################################


# indexmatchS2 <- findInterval(S1dfAl[1,1], S2dfAl[,1])
# 
# indexmatchS3 <- findInterval(S1dfAl[1,1], S3dfAl[,1])
# 
# S2dfAl2 <-  S2dfAl[indexmatchS2:(indexmatchS2+nrow(S1dfAl)-1),]
# S3dfAl2 <-  S3dfAl[indexmatchS3:(indexmatchS3+nrow(S1dfAl)-1),]
# 
# t = length(S1dfAl$time_s)-1
# SampleRateTS <- matrix(0, ncol = 1, nrow = t)
# for (i in 1:t) {
#   SampleRateTS[i,] <- 1/(S1dfAl$time_s[i+1]-S1dfAl$time_s[i])
# }
# SampleRate <- median(SampleRateTS)
# 
# timeinterval = 1/SampleRate
# 
# 
# S2dfAl2 = subset(S2dfAl2, select = -c(time_s, unix_timestamp_microsec) )
# S3dfAl2 = subset(S3dfAl2, select = -c(time_s, unix_timestamp_microsec) )
# S1dfAl = subset(S1dfAl, select = -c(time_s, unix_timestamp_microsec) )
# 
# S2dfAl2 = subset(S2dfAl2, select = -c(time_s) )
# S3dfAl2 = subset(S3dfAl2, select = -c(time_s) )
# S1dfAl = subset(S1dfAl, select = -c(time_s) )
# 
# 
# S1dfAl$time_s <-  S1dfAl$time_s-S1dfAl$time_s[1]
# 
# a = NULL
# 
# for (i in 2:length(S1dfAl[,1])){
#   
#   S1dfAl$time_s[i] <- timeinterval*(i-1)
#   
# }
# rm(time_s)
# time_s = seq(0,(length(S1dfAl[,1])-1)*timeinterval,timeinterval)
# 
# rm(a)
# 
# S1dfAl$time_s <-  time_s
# S2dfAl2$time_s <-  time_s
# S3dfAl2$time_s <-  time_s


write.csv(AlignedS1, "C:/Users/aaron.beach/CaptureU/S1aligned.csv")
write.csv(AlignedS2, "C:/Users/aaron.beach/CaptureU/S2aligned.csv")
write.csv(AlignedS3, "C:/Users/aaron.beach/CaptureU/S3aligned.csv")
write.csv(AlignedS4, "C:/Users/aaron.beach/CaptureU/S4aligned.csv")
write.csv(AlignedS5, "C:/Users/aaron.beach/CaptureU/S5aligned.csv")
