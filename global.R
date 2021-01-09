#fin peaks, find zero crossings, then from range peak1 to peak2, what is the last zero crossing







library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)

library(data.table)
library(tictoc)
library(ggplot2)
library(plyr)
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


# listToDF <- function(aa){
#   sapply(aa, "length<-", max(lengths(aa)))
# }




