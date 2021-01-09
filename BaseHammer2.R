#fin peaks, find zero crossings, then from range peak1 to peak2, what is the last zero crossing

rm(list = ls())
if(!is.null(dev.list())) dev.off()

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
library(RSpincalc)
library(biomechanics)
library(mFilter)
library(IRISSeismic)


# listToDF <- function(aa){
#   sapply(aa, "length<-", max(lengths(aa)))
# }

FootdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Alex/a", caption = "Select Foot File")))
PelvisdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Alex/a", caption = "Select Pelvis File")))
ThoraxdfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Alex/a", caption = "Select Thorax File")))
HanddfAl <- data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Alex/a", caption = "Select Hand File")))

Ballvelocity <-  data.frame(fread(choose.files(default = "C:/Users/aaron.beach/CaptureU/Alex DTE 14092020/a", caption = "Select Ball Velocity File")))

#############################
t = length(PelvisdfAl$time_s)-1
SampleRateTS <- matrix(0, ncol = 1, nrow = t)
for (i in 1:t) {
  SampleRateTS[i,] <- 1/(PelvisdfAl$time_s[i+1]-PelvisdfAl$time_s[i])
}
SampleRate <- median(SampleRateTS)

################
FootdfAl$time_s<- FootdfAl$time_s - FootdfAl$time_s[1]
PelvisdfAl$time_s<- PelvisdfAl$time_s - PelvisdfAl$time_s[1]
ThoraxdfAl$time_s<- ThoraxdfAl$time_s - ThoraxdfAl$time_s[1]
HanddfAl$time_s<- HanddfAl$time_s - HanddfAl$time_s[1]

FootdfAl$ResAcc = sqrt((FootdfAl$"highg_ax_m.s.s"^2) + (FootdfAl$"highg_ay_m.s.s"^2) + (FootdfAl$"highg_az_m.s.s"^2))-9.8
HanddfAl$ResAcc = sqrt((HanddfAl$"highg_ax_m.s.s"^2) + (HanddfAl$"highg_ay_m.s.s"^2) + (HanddfAl$"highg_az_m.s.s"^2))-9.8
#ThoraxdfAl$ResAcc = sqrt((ThoraxdfAl$"highg_ax_m.s.s"^2) + (ThoraxdfAl$"highg_ay_m.s.s"^2) + (ThoraxdfAl$"highg_az_m.s.s"^2))-9.8

#################
PelvisdfAl$gx_deg.s = PelvisdfAl$gx_deg.s*57.2958
ThoraxdfAl$gx_deg.s = ThoraxdfAl$gx_deg.s*57.2958
HanddfAl$gx_deg.s = HanddfAl$gx_deg.s*57.2958
# PelvisdfAl$z_deg2 = PelvisdfAl$z_deg-FootdfAl$z_deg+10
# ThoraxdfAl$z_deg2 = ThoraxdfAl$z_deg-FootdfAl$z_deg+8

PelvisdfAl$gy_deg.s = PelvisdfAl$gy_deg.s*57.2958
ThoraxdfAl$gy_deg.s = ThoraxdfAl$gy_deg.s*57.2958
HanddfAl$gy_deg.s = HanddfAl$gy_deg.s*57.2958

PelvisdfAl$gz_deg.s = PelvisdfAl$gy_deg.s*57.2958
ThoraxdfAl$gz_deg.s = ThoraxdfAl$gy_deg.s*57.2958
HanddfAl$gz_deg.s = HanddfAl$gy_deg.s*57.2958




############# FILTERING #############

# spectrum(Ballvelocity$BallVelocity, method = "ar")
#  residual_analysis(Ballvelocity$BallVelocity, cutoff_range = c(0, 10), sample_freq = 100,
#                    final_order = 4, interval = 0.5)
# 
#  bfBV <- butter(4, 5/50, type="low")
#  
#  Ballvelocity$BallVelocityFilt <- filtfilt(bfBV, Ballvelocity$BallVelocity)
#  plot(Ballvelocity$BallVelocity, type = 'l')
#  plot(Ballvelocity$BallVelocityFilt, type = 'l')
 


#spectrum(FootdfAl$ResAccfilt, method = "ar")
#residual_analysis(FootdfAl$ResAccfilt, cutoff_range = c(0, 800), sample_freq = 1600,
#                  final_order = 4, interval = 1)
 
#spectrum(ThoraxdfAl$highg_ax_m.s.s, method = "ar")
#residual_analysis(HanddfAl$ResAcc, cutoff_range = c(0, 20), sample_freq = 1600,
#                 final_order = 4, interval = 1)
bfF <- butter(4, 20/800, type="low")
bfTacc <- butter(4, 20/800, type="low")
bfH <- butter(4, 6/800, type="low")


FootdfAl$ResAccfilt <- filtfilt(bfF, FootdfAl$ResAcc)
HanddfAl$ResAccfilt <- filtfilt(bfH, HanddfAl$ResAcc)

##ThoraxdfAl$ResAccfilt <- filtfilt(bfTacc, ThoraxdfAl$ResAcc)
#ThoraxdfAl$Xaccfilt <- filtfilt(bfTacc, ThoraxdfAl$highg_ax_m.s.s)

#plot <- ggplot() + geom_line(aes(x=FootdfAl$V1, y=(FootdfAl$ResAcc))) + geom_line(aes(x=FootdfAl$V1, y=(FootdfAl$ResAccfilt), colour = "red"))
#plot <- ggplot() + geom_line(aes(x=HanddfAl$V1, y=(HanddfAl$ResAcc))) + geom_line(aes(x=HanddfAl$V1, y=(HanddfAl$ResAccfilt), colour = "red"))

#ggplotly(plot)



#ev <-ggplot() + geom_line(aes(x=PelvisdfAl$V1, y=(ThoraxdfAl$highg_ax_m.s.s)))+ geom_line(aes(x=PelvisdfAl$V1, y=(FootdfAl$ResAccfilt)))
#ev

#spectrum(PelvisdfAl$"gx_deg.s", method = "ar")
#residual_analysis(PelvisdfAl$"gx_deg.s", cutoff_range = c(0, 20), sample_freq = 1600,
#                   final_order = 4, interval = 0.5)
bfP <- butter(4, 7/800, type="low")

PelvisdfAl$gyroxfilt <- filtfilt(bfP, PelvisdfAl$"gx_deg.s")
PelvisdfAl$gyroyfilt <- filtfilt(bfP, PelvisdfAl$"gy_deg.s")
PelvisdfAl$gyrozfilt <- filtfilt(bfP, PelvisdfAl$"gz_deg.s")

plot <- ggplot() + geom_line(aes(x=PelvisdfAl$V1, y=(PelvisdfAl$"gx_deg.s"))) + geom_line(aes(x=PelvisdfAl$V1, y=(PelvisdfAl$gyroxfilt), colour = "red"))
ggplotly(plot)
#spectrum(ThoraxdfAl$"gx_deg.s", method = "ar")
residual_analysis(ThoraxdfAl$"gx_deg.s", cutoff_range = c(0, 40), sample_freq = 1600,
                   final_order = 4, interval = 0.5)


bfT <- butter(4, 7/800, type="low")

ThoraxdfAl$gyroxfilt <- filtfilt(bfT, ThoraxdfAl$"gx_deg.s")
ThoraxdfAl$gyroyfilt <- filtfilt(bfT, ThoraxdfAl$"gy_deg.s")
ThoraxdfAl$gyrozfilt <- filtfilt(bfT, ThoraxdfAl$"gz_deg.s")

plot <- ggplot() + geom_line(aes(x=ThoraxdfAl$V1, y=(ThoraxdfAl$"gx_deg.s"))) + geom_line(aes(x=ThoraxdfAl$V1, y=(ThoraxdfAl$gyroxfilt), colour = "red"))
ggplotly(plot)
#ggplot() + geom_line(aes(x=PelvisdfAl$V1, y=(PelvisdfAl$gyroxfilt)))


bfH <- butter(4, 5/800, type="low")
HanddfAl$gyroxfilt <- filtfilt(bfH, HanddfAl$"gx_deg.s")
HanddfAl$gyroyfilt <- filtfilt(bfH, HanddfAl$"gy_deg.s")
HanddfAl$gyrozfilt <- filtfilt(bfH, HanddfAl$"gz_deg.s")

plot <- ggplot() + geom_line(aes(x=HanddfAl$V1, y=(HanddfAl$"gx_deg.s"))) + geom_line(aes(x=HanddfAl$V1, y=(HanddfAl$gyroxfilt), colour = "red"))
ggplotly(plot)


#ggplot() + geom_line(aes(x=HanddfAl$V1, y=(HanddfAl$ResAcc)))


# plot(ThoraxdfAl$"gx_deg.s", type = 'l')
# plot(ThoraxdfAl$gyroxfilt, type = 'l')
# 
# plot(PelvisdfAl$"gx_deg.s", type = 'l')
# plot(PelvisdfAl$gyroxfilt, type = 'l')
# 
# plot(FootdfAl$ResAcc, type = 'l')
# plot(FootdfAl$ResAccfilt, type = 'l')


#spectrum(PelvisdfAl$z_deg2, method = "ar")
# residual_analysis(PelvisdfAl$z_deg, cutoff_range = c(0, 20), sample_freq = 1600,
#                   final_order = 4, interval = 0.5)

bfPA <- butter(4, 6/800, type="low")
PelvisdfAl$z_degfilt <- filtfilt(bfPA, PelvisdfAl$z_deg)

# plot(PelvisdfAl$z_deg, type = 'l')

#spectrum(ThoraxdfAl$z_deg2, method = "ar")

# residual_analysis(ThoraxdfAl$z_deg, cutoff_range = c(0, 20), sample_freq = 1600,
#                   final_order = 4, interval = 0.5)

bfTA <- butter(4, 6/800, type="low")

ThoraxdfAl$z_degfilt <- filtfilt(bfTA, ThoraxdfAl$z_deg)
# plot(ThoraxdfAl$y_deg, type = 'l')

#spectrum(ThoraxdfAl$z_degfilt, method = "ar")





# Set the sample period to run Stroke Event Detection to analyse Performance
# TStart <- 0
# TEnd <- 10
# DStart <- as.integer(TStart * SampleRate)
# DEnd <- as.integer(TEnd * SampleRate)


# FootdfAl$frames <- c(1:length(FootdfAl$time_s))
# 
# 
# FootdfAlTrim = FootdfAl[DStart:DEnd,]

# Calculate the Start & End of each Stroke Cycle
# minpeakdistance of 900 allows for a SR49

#######CREATE EVENTS FROM  ACC PEAKS#########
 Event <- findpeaks((FootdfAl$ResAccfilt), minpeakheight = 100, minpeakdistance = 100)
 ev <-  ggplot() + geom_line(aes(x=FootdfAl$V1, y=(FootdfAl$ResAccfilt))) + geom_point(aes(x=Event[,2], y=Event[,1]))
 ev
 Event <- Event[,2]

  
# EventTh <- findpeaks((ThoraxdfAl$Xaccfilt), minpeakheight = 15, minpeakdistance = 500)
# ev <-  ggplot() + geom_line(aes(x=ThoraxdfAl$V1, y=(ThoraxdfAl$Xaccfilt))) + geom_point(aes(x=EventTh[,2], y=EventTh[,1]))
# ev
# EventTh <- EventTh[,2]

############Plot to find events
# ev <-  ggplot() + geom_line(aes(x=ThoraxdfAl$V1, y=(ThoraxdfAl$highg_ax_m.s.s)))
# ggplotly(ev)
# Event = c(9914, 10951, 11750, 12607)

EventRelease <- findpeaks((HanddfAl$ResAccfilt), minpeakheight = 90, minpeakdistance = 1000)

ev <-  ggplot() + geom_line(aes(x=HanddfAl$V1, y=(HanddfAl$ResAccfilt))) + geom_point(aes(x=EventRelease[,2], y=EventRelease[,1]))
ev


Release <- Event2[1] + 1.99

#Event <- Event[,2]
EventRelease2 <- EventRelease[,2]
Event <- sort(Event, decreasing = FALSE)
Event <-  Event[1:4]
Event2 <- FootdfAl$time_s[Event]
Release <- FootdfAl$time_s[EventRelease2]


Event2 = c(Event2,Release)


PelvisGyrox = NULL
ThoraxGyrox = NULL
PelvisGyroy = NULL
ThoraxGyroy = NULL

PelvisGyrox[[1]] = PelvisdfAl$gyroxfilt[Event[1]:Event[2]]
PelvisGyrox[[2]] = PelvisdfAl$gyroxfilt[Event[2]:Event[3]]
PelvisGyrox[[3]] = PelvisdfAl$gyroxfilt[Event[3]:Event[4]]
PelvisGyrox[[4]] = PelvisdfAl$gyroxfilt[Event[4]:NROW(PelvisdfAl$gyroxfilt)]

ThoraxGyrox[[1]] = ThoraxdfAl$gyroxfilt[Event[1]:Event[2]]
ThoraxGyrox[[2]] = ThoraxdfAl$gyroxfilt[Event[2]:Event[3]]
ThoraxGyrox[[3]] = ThoraxdfAl$gyroxfilt[Event[3]:Event[4]]
ThoraxGyrox[[4]] = ThoraxdfAl$gyroxfilt[Event[4]:NROW(ThoraxdfAl$gyroxfilt)]

HandGyrox[[1]] = HanddfAl$gyroxfilt[Event[1]:Event[2]]
HandGyrox[[2]] = HanddfAl$gyroxfilt[Event[2]:Event[3]]
HandGyrox[[3]] = HanddfAl$gyroxfilt[Event[3]:Event[4]]
HandGyrox[[4]] = HanddfAl$gyroxfilt[Event[4]:NROW(HanddfAl$gyroxfilt)]

PelvisGyroy[[1]] = PelvisdfAl$gyroyfilt[Event[1]:Event[2]]
PelvisGyroy[[2]] = PelvisdfAl$gyroyfilt[Event[2]:Event[3]]
PelvisGyroy[[3]] = PelvisdfAl$gyroyfilt[Event[3]:Event[4]]
PelvisGyroy[[4]] = PelvisdfAl$gyroyfilt[Event[4]:NROW(PelvisdfAl$gyroyfilt)]

ThoraxGyroy[[1]] = ThoraxdfAl$gyroyfilt[Event[1]:Event[2]]
ThoraxGyroy[[2]] = ThoraxdfAl$gyroyfilt[Event[2]:Event[3]]
ThoraxGyroy[[3]] = ThoraxdfAl$gyroyfilt[Event[3]:Event[4]]
ThoraxGyroy[[4]] = ThoraxdfAl$gyroyfilt[Event[4]:NROW(ThoraxdfAl$gyroyfilt)]

HandGyroy[[1]] = HanddfAl$gyroyfilt[Event[1]:Event[2]]
HandGyroy[[2]] = HanddfAl$gyroyfilt[Event[2]:Event[3]]
HandGyroy[[3]] = HanddfAl$gyroyfilt[Event[3]:Event[4]]
HandGyroy[[4]] = HanddfAl$gyroyfilt[Event[4]:NROW(HanddfAl$gyroyfilt)]

#plot(PelvisGyrox[[3]])

#############OLD#############



# gyrev <-  ggplot() + geom_line(aes(x=PelvisdfAl$V1, y=(PelvisdfAl$gx_deg.s))) + geom_point(aes(x=Event[,2], y=Event[,1]))
# ggplotly(gyrev)


# Find peaks gyro
# EventGyropos <- findpeaks((PelvisdfAl$gx_deg.s), minpeakheight = 50, minpeakdistance = 500)
# EventGyropos <- EventGyropos[order(Event[,2],decreasing = FALSE),]
# EventGyropos[,2:4] <- EventGyropos[,2:4]
# 
# EventGyroneg <- findpeaks((-PelvisdfAl$gx_deg.s), minpeakheight = 50, minpeakdistance = 500)
# EventGyroneg <- EventGyroneg[order(Event[,2],decreasing = FALSE),]
# EventGyroneg[,2:4] <- EventGyroneg[,2:4]


########## CREATE CYCLES FROM EVENT TO EVENT ########
#if(which.min(FootdfAl$AccFilt[1:Event[1]]) >0){
#  Event <- Event[-1]
#}

#ev <-  ggplot() + geom_line(aes(x=FootdfAl$V1, y=(FootdfAl$AccFilt))) + geom_point(aes(x=Event, y=c(1)))
#ggplotly(ev)

# zero <- NULL
# cycle <- NULL
# cycle[1] = tail((which(FootdfAl$AccFilt[1:Event[1]] <0)), n=1)
# 
# 
# for (r in 1:(length(Event))){
#   t = r+1
#   # framer = Event[r,2] - FootdfAl$V1[1]
#   # framet = Event[t,2] - FootdfAl$V1[1]
#   
#   tryCatch((cycle[t] = tail((which(FootdfAl$AccFilt[Event[r]:Event[t]] <0)), n=1) + Event[r]),error = function(e) print(NA))
# }




#evB <-  ggplot() + geom_line(aes(x=FootdfAl$V1, y=(FootdfAl$AccFilt))) + geom_vline(xintercept=cycle, colour = "red")
#ggplotly(evB)

# evPG <-  ggplot() + geom_line(aes(x=PelvisdfAl$time_s, y=(PelvisdfAl$gyroxfilt))) + geom_vline(xintercept=Event2, colour = "red")
# ggplotly(evPG)


#evP <-  ggplot() + geom_line(aes(x=PelvisdfAl$V1, y=(PelvisdfAl$z_degfilt))) + geom_vline(xintercept=cycle, colour = "red")
#ggplotly(evP)

# evTG <-  ggplot() + geom_line(aes(x=ThoraxdfAl$time_s, y=(ThoraxdfAl$gyroxfilt))) + geom_vline(xintercept=Event2, colour = "red")
# ggplotly(evTG)

#evT <-  ggplot() + geom_line(aes(x=ThoraxdfAl$V1, y=(ThoraxdfAl$z_degfilt))) + geom_vline(xintercept=Event, colour = "red")
#ggplotly(evT)



###########################
yaxisrange = range(PelvisdfAl$gyroxfilt,ThoraxdfAl$gyroxfilt)
xaxisrange = length(PelvisdfAl$gyroxfilt)


fig <- plot_ly(x = PelvisdfAl$time_s) %>%
  add_trace(y = PelvisdfAl$gyroxfilt, type = 'scatter', mode = 'lines',
            line = list(color = 'rgba(250, 0, 0, 1)'),
            showlegend = TRUE, name = 'Pelvis Rotation')%>% 
  add_trace(y = ThoraxdfAl$gyroxfilt, type = 'scatter', mode = 'lines', line = list(color = 'green'),
            showlegend = TRUE, name = 'Thorax Rotation')%>% 
  add_trace(y = HanddfAl$ResAccfilt, type = 'scatter', mode = 'lines', yaxis = "y2", line = list(color = 'blue'),
            showlegend = TRUE, name = 'Hand Acceleration')#%>%
  # add_trace(x=Ballvelocity$Time, y = Ballvelocity$BallVelocity, type = 'scatter', mode = 'lines', yaxis = "y2", line = list(color = 'purple'),
  #           showlegend = TRUE, name = 'Ball Velocity')

# line <- list(
#   type = "line",
#   line = list(color = "black", dash = 'dot'),
#   xref = "x",
#   yref = "y"
# )
# line2 <- list(
#   type = "line",
#   line = list(color = "red", dash = 'dot'),
#   xref = "x",
#   yref = "y"
# )

# lines <- list()
# for (i in c(1,2,3,4)){
#   line[["x0"]] <- Event2[i]
#   line[["x1"]] <- Event2[i]
#   line[["y0"]] <- yaxisrange[1]
#   line[["y1"]] <- yaxisrange[2]
#   lines <- c(lines, list(line))
# }
# 
# lines2 <- list()
# line2[["x0"]] <- Event2[5]
# line2[["x1"]] <- Event2[5]
# line2[["y0"]] <- yaxisrange[1]
# line2[["y1"]] <- yaxisrange[2]
# lines2 <- c(lines2, list(line2))

fig <-  layout(fig, xaxis = list(title = "Time (s)",
                                 showgrid = TRUE,
                                 showline = FALSE,
                                 showticklabels = TRUE,
                                 ticks = 'outside',
                                 zeroline = TRUE),
               yaxis = list(title = "Rotational velocity (deg/s)",
                            showgrid = TRUE,
                            showline = FALSE,
                            showticklabels = TRUE,
                            ticks = 'outside',
                            range = c(-400,1200),
                            zeroline = TRUE),
               yaxis2 = list(title = "Velocity (m/s)",
                             overlaying = "y",
                             side = "right",
                             showgrid = FALSE,
                             showline = FALSE,
                             showticklabels = TRUE,
                             ticks = 'inside',
                             zeroline = TRUE,
                             #range = c(5,15),
                             color = 'rgba(122, 39, 160, 1)'),
               margin = list(l = 0, r = 25, b = 0, t = 0),
               legend = list(x =0.1, y = 0.8, font = list(size = 24)), shapes = c(lines)#,
               #annotations = list(text = c("Foot Strikes"),  x = c(Event2[3]), y = c(-400),showarrow=FALSE,xanchor = c('right'), font = list(size = 24))
               ) %>%
  config(displayModeBar = T)


fig

########################

yaxisrange = range(PelvisdfAl$gyroxfilt,ThoraxdfAl$gyroxfilt)
xaxisrange = length(PelvisdfAl$gyroxfilt)
yaxisrange2 = range(HanddfAl$ResAccfilt)


fig <- plot_ly(x = PelvisdfAl$time_s) %>%
  add_trace(y = PelvisdfAl$gyroxfilt, type = 'scatter', mode = 'lines',
            line = list(color = 'rgba(250, 0, 0, 1)'),
            showlegend = TRUE, name = 'Pelvis Rotation')%>% 
  add_trace(y = ThoraxdfAl$gyroxfilt, type = 'scatter', mode = 'lines', line = list(color = 'green'),
            showlegend = TRUE, name = 'Thorax Rotation')%>% 
   add_trace(y = HanddfAl$ResAccfilt, type = 'scatter', mode = 'lines', yaxis = "y2", line = list(color = 'blue'),
             showlegend = TRUE, name = 'Hand Acceleration')#%>%
#add_trace(x=Ballvelocity$Time, y = Ballvelocity$BallVelocity, type = 'scatter', mode = 'lines', yaxis = "y2", line = list(color = 'purple'),
#          showlegend = TRUE, name = 'Ball Velocity')

line <- list(
  type = "line",
  line = list(color = "black", dash = 'dot'),
  xref = "x",
  yref = "y"
)
line2 <- list(
  type = "line",
  line = list(color = "red", dash = 'dot'),
  xref = "x",
  yref = "y"
)

lines <- list()
for (i in c(1,2,3,4)){
  line[["x0"]] <- Event2[i]
  line[["x1"]] <- Event2[i]
  line[["y0"]] <- yaxisrange[1]
  line[["y1"]] <- yaxisrange[2]
  lines <- c(lines, list(line))
}

lines2 <- list()
line2[["x0"]] <- Event2[5]
line2[["x1"]] <- Event2[5]
line2[["y0"]] <- yaxisrange[1]
line2[["y1"]] <- yaxisrange[2]
lines2 <- c(lines2, list(line2))

fig <-  layout(fig, xaxis = list(title = "Time (s)",
                                 showgrid = TRUE,
                                 showline = FALSE,
                                 showticklabels = TRUE,
                                 ticks = 'outside',
                                 zeroline = TRUE),
               yaxis = list(title = "Rotational velocity (deg/s)",
                            showgrid = TRUE,
                            showline = FALSE,
                            showticklabels = TRUE,
                            ticks = 'outside',
                            range = yaxisrange,
                            zeroline = TRUE),
               yaxis2 = list(title = "Velocity (m/s)",
                             overlaying = "y",
                             side = "right",
                             showgrid = FALSE,
                             showline = FALSE,
                             showticklabels = TRUE,
                             ticks = 'inside',
                             zeroline = TRUE,
                             range = yaxisrange2,
                             color = 'rgba(122, 39, 160, 1)'),
               margin = list(l = 0, r = 40, b = 0, t = 0),
               legend = list(x =0.1, y = 0.8, font = list(size = 24)), shapes = c(lines, lines2),
               annotations = list(text = c("Foot Strikes", "Release"),  x = c(Event2[3], Event2[5]), y = c(-400,-400),showarrow=FALSE,xanchor = c('right', 'left'), font = list(size = 24))
               ) %>%
  config(displayModeBar = T)


fig
# alldf = data.frame(PelvisdfAl$gyroxfilt,ThoraxdfAl$gyroxfilt)
# write.csv(alldf, "C:/Users/aaron.beach/CaptureU/Alex Sunday Event/Combined.csv")

##################################

yaxisrange = range(PelvisdfAl$gyroyfilt,ThoraxdfAl$gyroyfilt)
xaxisrange = length(PelvisdfAl$gyroyfilt)


fig <- plot_ly(x = PelvisdfAl$time_s) %>%
  add_trace(y = PelvisdfAl$gyroyfilt, type = 'scatter', mode = 'lines',
            line = list(color = 'rgba(250, 0, 0, 1)'),
            showlegend = TRUE, name = 'Pelvis Rotation')%>% 
  add_trace(y = ThoraxdfAl$gyroyfilt, type = 'scatter', mode = 'lines', line = list(color = 'green'),
            showlegend = TRUE, name = 'Thorax Rotation')

line <- list(
  type = "line",
  line = list(color = "black", dash = 'dot'),
  xref = "x",
  yref = "y"
)

lines <- list()
for (i in c(1,2,3,4)){
  line[["x0"]] <- Event2[i]
  line[["x1"]] <- Event2[i]
  line[["y0"]] <- yaxisrange[1]
  line[["y1"]] <- yaxisrange[2]
  lines <- c(lines, list(line))
}

fig <-  layout(fig, xaxis = list(title = "Time (s)",
                                 showgrid = TRUE,
                                 showline = FALSE,
                                 showticklabels = TRUE,
                                 ticks = 'outside',
                                 zeroline = TRUE),
               yaxis = list(title = "Rotational velocity (deg/s)",
                            showgrid = TRUE,
                            showline = FALSE,
                            showticklabels = TRUE,
                            ticks = 'outside',
                            zeroline = TRUE), margin = list(l = 0, r = 1, b = 0, t = 0),
               legend = list(x =0.1, y = 0.8), shapes = lines,
               annotations = list(text = "Foot Strikes",  x = Event2[3], y = -300,showarrow=FALSE, font = list(size = 24, color = "red"))) %>%
  config(displayModeBar = T)


fig


###################
# yaxisrange = range(PelvisdfAl$gyrozfilt)
# xaxisrange = length(PelvisdfAl$gyrozfilt)
# 
# 
# fig <- plot_ly(x = PelvisdfAl$time_s) %>%
#   add_trace(y = PelvisdfAl$gyrozfilt, type = 'scatter', mode = 'lines',
#             line = list(color = 'rgba(250, 0, 0, 1)'),
#             showlegend = TRUE, name = 'Pelvis Rotation')%>% 
#   add_trace(y = ThoraxdfAl$gyrozfilt, type = 'scatter', mode = 'lines', line = list(color = 'green'),
#             showlegend = TRUE, name = 'Thorax Rotation')
# 
# line <- list(
#   type = "line",
#   line = list(color = "black", dash = 'dot'),
#   xref = "x",
#   yref = "y"
# )
# 
# lines <- list()
# for (i in c(1,2,3,4)){
#   line[["x0"]] <- Event2[i]
#   line[["x1"]] <- Event2[i]
#   line[["y0"]] <- yaxisrange[1]
#   line[["y1"]] <- yaxisrange[2]
#   lines <- c(lines, list(line))
# }
# 
# fig <-  layout(fig, xaxis = list(title = "Time (s)",
#                                  showgrid = TRUE,
#                                  showline = FALSE,
#                                  showticklabels = TRUE,
#                                  ticks = 'outside',
#                                  zeroline = TRUE),
#                yaxis = list(title = "Rotational velocity (deg/s)",
#                             showgrid = TRUE,
#                             showline = FALSE,
#                             showticklabels = TRUE,
#                             ticks = 'outside',
#                             zeroline = TRUE), margin = list(l = 0, r = 1, b = 0, t = 0),
#                legend = list(x =0.1, y = 0.8), shapes = lines,
#                annotations = list(text = "Foot Strikes",  x = Event2[3], y = -10,showarrow=FALSE, font = list(size = 24, color = "red"))) %>%
#   config(displayModeBar = T)
# 
# 
# fig

# #####################
# fig <- plot_ly(x = PelvisdfAl$time_s) %>%
#   add_trace(y = PelvisdfAl$z_deg, type = 'scatter', mode = 'lines',
#             line = list(color = 'rgba(250, 0, 0, 1)'),
#             showlegend = TRUE, name = 'Pelvis Rotation')%>% 
#   add_trace(y = ThoraxdfAl$z_deg, type = 'scatter', mode = 'lines', line = list(color = 'green'),
#             showlegend = TRUE, name = 'Thorax Rotation')
# 
# line <- list(
#   type = "line",
#   line = list(color = "black", dash = 'dot'),
#   xref = "x",
#   yref = "y"
# )
# 
# lines <- list()
# for (i in c(1,2,3,4)){
#   line[["x0"]] <- Event2[i]
#   line[["x1"]] <- Event2[i]
#   line[["y0"]] <- yaxisrange[1]
#   line[["y1"]] <- yaxisrange[2]
#   lines <- c(lines, list(line))
# }
# 
# fig <-  layout(fig, xaxis = list(title = "Time (s)",
#                                  showgrid = TRUE,
#                                  showline = FALSE,
#                                  showticklabels = TRUE,
#                                  ticks = 'outside',
#                                  zeroline = TRUE),
#                yaxis = list(title = "Rotational velocity (deg/s)",
#                             showgrid = TRUE,
#                             showline = FALSE,
#                             showticklabels = TRUE,
#                             ticks = 'outside',
#                             zeroline = TRUE), margin = list(l = 0, r = 1, b = 0, t = 0),
#                legend = list(x =0.1, y = 0.8), shapes = lines,
#                annotations = list(text = "Foot Strikes",  x = Event2[3], y = -400,showarrow=FALSE, font = list(size = 24, color = "red"))) %>%
#   config(displayModeBar = T)
# 
# 
# fig
# 
# 
# 
# #######################
# for (i in 1:length(ThoraxdfAl$z_deg)){
#   if (ThoraxdfAl$z_deg[i] <0){
#     ThoraxdfAl$z_deg[i] = 360+ThoraxdfAl$z_deg[i]
#   }
# }
# 
# fig <- plot_ly(
#   ThoraxdfAl,
#   type = 'scatterpolar',
#   mode = 'lines'
# ) 
# fig <- fig %>%
#   add_trace(
#     r = ThoraxdfAl$time_s,
#     theta = ThoraxdfAl$z_deg,
#     name = 'Figure8',
#     line = list(
#       color = 'peru'
#     )
#   )
# fig
# 
# rm(Q)
# rm(Q2)
# Q <-  data.table(ThoraxdfAl$qx, ThoraxdfAl$qy, ThoraxdfAl$qz, ThoraxdfAl$qr)
# Q <- data.matrix(Q)
# Q2 <- data.frame(Q2EA(Q, EulerOrder='ZYX'))
# plot(Q2$X3)
