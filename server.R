options(shiny.maxRequestSize=2000*1024^2)
server <- function(input, output) {
  
  observeEvent(input$goButton, {
    
    ### IMPORT FILES ######
    BoatdfAl <- fread(input$BoatdfAl$datapath, stringsAsFactors = F)
    
    PelvisdfAl <- fread(input$PelvisdfAl$datapath, stringsAsFactors = F)
    
    ThoraxdfAl <- fread(input$ThoraxdfAl$datapath, stringsAsFactors = F)
    
    MinimaxdfAl <- fread(input$MinimaxdfAl$datapath, stringsAsFactors = F)
    
    t = length(BoatdfAl$time_s)-1
    SampleRateTS <- matrix(0, ncol = 1, nrow = t)
    for (i in 1:t) {
      SampleRateTS[i,] <- 1/(BoatdfAl$time_s[i+1]-BoatdfAl$time_s[i])
    }
    SampleRate <- median(SampleRateTS)
    
    
    
    start = SampleRate*60
    end = SampleRate*90
    
    BoatdfAl <- BoatdfAl[start:end,]
    PelvisdfAl <- PelvisdfAl[start:end,]
    ThoraxdfAl <- ThoraxdfAl[start:end,]
    
    
    row.names(BoatdfAl)<-1:nrow(BoatdfAl)
    row.names(PelvisdfAl)<-1:nrow(PelvisdfAl)
    row.names(ThoraxdfAl)<-1:nrow(ThoraxdfAl)
    
    BoatdfAl$V1<-1:nrow(BoatdfAl)
    PelvisdfAl$V1<-1:nrow(PelvisdfAl)
    ThoraxdfAl$V1<-1:nrow(ThoraxdfAl)
    
    PelvisdfAl$gx_deg.s = PelvisdfAl$gx_deg.s#*57.2958
    ThoraxdfAl$gx_deg.s = ThoraxdfAl$gx_deg.s#*57.2958
    PelvisdfAl$z_deg2 = PelvisdfAl$z_deg-BoatdfAl$z_deg+10
    ThoraxdfAl$z_deg2 = ThoraxdfAl$z_deg-BoatdfAl$z_deg+8
    
    ########OLD########
    # x1 = PelvisdfAl$`time_s`
    # y1 =PelvisdfAl$gx_deg.s
    # p <- plot_ly(x = ~x1, y = ~y1, mode = 'lines', source = "source")
    # p
    # 
    # 
    # x1 = PelvisdfAl$`time_s`
    # y1 =PelvisdfAl$z_deg
    # p <- plot_ly(x = ~x1, y = ~y1, mode = 'lines', source = "source")
    # p
    # 
    # x1 = BoatdfAl$`time_s`
    # y1 =BoatdfAl$z_deg
    # p <- plot_ly(x = ~x1, y = ~y1, mode = 'lines', source = "source")
    # p
    
    #BoatdfAltrim <- BoatdfAl %>% dplyr::filter(between(time_s, 650, 850))
    #BoatdfAltrim2 <- BoatdfAl%>% dplyr::filter(between(time_s, 750, 775))
    #BoatdfLGtrim <- BoatdfLG%>% dplyr::filter(between(time_s, 650, 850))
    #BoatdfLGtrim2 <- BoatdfLG%>% dplyr::filter(between(time_s, 750, 775))
    
    # PelvisdfAltrim <- PelvisdfAl%>% dplyr::filter(between(time_s, 650, 850))
    # PelvisdfAltrim2 <- PelvisdfAl%>% dplyr::filter(between(time_s, 750, 775))
    # PelvisdfAlcal <- PelvisdfAl%>% dplyr::filter(between(time_s, 0, 100))
    # PelvisdfLGtrim <- PelvisdfLG%>% dplyr::filter(between(time_s, 650, 850))
    # PelvisdfLGtrim2 <- PelvisdfLG%>% dplyr::filter(between(time_s, 750, 775))
    # PelvisdfALGcal <- PelvisdfLG%>% dplyr::filter(between(time_s, 0, 100))
    # 
    # 
    # ThoraxdfAltrim <- ThoraxdfAl%>% dplyr::filter(between(time_s, 650, 850))
    # ThoraxdfAltrim2 <- ThoraxdfAl%>% dplyr::filter(between(time_s, 750, 775))
    # ThoraxdfAlcal <- ThoraxdfAl%>% dplyr::filter(between(time_s, 0, 100))
    # 
    # ThoraxdfLGtrim <- ThoraxdfLG%>% dplyr::filter(between(time_s, 650, 850))
    # ThoraxdfLGtrim2 <- ThoraxdfLG%>% dplyr::filter(between(time_s, 750, 775))
    # ThoraxdfLGcal <- ThoraxdfLG%>% dplyr::filter(between(time_s, 0, 100))
    
    
    ######## FILTER BOAT ACC DATA ########
    
    #############OLD#############
    
    #BoatdfAl$ax <-  BoatdfAltrim$"ax_m.s.s"
    #BoatdfAl$AccFilt <- NULL
    
    #Boat.OffSet = roll::roll_sd(as.matrix(BoatdfAl$AccFilt), 1000)
    #BoatdfAl$AccFilt2 = BoatdfAl$AccFilt - BoatdfAl$AccFilt[which.min(Boat.OffSet)]
    
    #BoatdfAltrim <- BoatdfAl %>% dplyr::filter(between(time_s, 1600, 1610))
    
    ######CALCULTE SAMPLE RATE#######
    t = length(BoatdfAl$time_s)-1
    SampleRateTS <- matrix(0, ncol = 1, nrow = t)
    for (i in 1:t) {
      SampleRateTS[i,] <- 1/(BoatdfAl$time_s[i+1]-BoatdfAl$time_s[i])
    }
    SampleRate <- median(SampleRateTS)
    
    #############OLD#############
    
    #spectrum(BoatdfAl$"ax_m.s.s", method = "ar")
    
    
    bf <- butter(4, 0.1, type="low")
    BoatdfAl$AccFilt <- filtfilt(bf, BoatdfAl$"ax_m.s.s")
    
    
    #spectrum(PelvisdfAl$"gx_deg.s", method = "ar")
    
    bf <- butter(4, 0.1, type="low")
    PelvisdfAl$gyroxfilt <- filtfilt(bf, PelvisdfAl$"gx_deg.s")
    
    #spectrum(ThoraxdfAl$"gx_deg.s", method = "ar")
    
    bf <- butter(4, 0.1, type="low")
    ThoraxdfAl$gyroxfilt <- filtfilt(bf, ThoraxdfAl$"gx_deg.s")
    
    #spectrum(PelvisdfAl$z_deg2, method = "ar")
    
    bf <- butter(4, 0.1, type="low")
    PelvisdfAl$z_degfilt <- filtfilt(bf, PelvisdfAl$z_deg2)
    
    #spectrum(ThoraxdfAl$z_deg2, method = "ar")
    
    bf <- butter(4, 0.1, type="low")
    ThoraxdfAl$z_degfilt <- filtfilt(bf, ThoraxdfAl$z_deg2)
    #spectrum(ThoraxdfAl$z_degfilt, method = "ar")
    
    
    
    
    
    # Set the sample period to run Stroke Event Detection to analyse Performance
    # TStart <- 0
    # TEnd <- 10
    # DStart <- as.integer(TStart * SampleRate)
    # DEnd <- as.integer(TEnd * SampleRate)
    
    
    # BoatdfAl$frames <- c(1:length(BoatdfAl$time_s))
    # 
    # 
    # BoatdfAlTrim = BoatdfAl[DStart:DEnd,]
    
    # Calculate the Start & End of each Stroke Cycle
    # minpeakdistance of 900 allows for a SR49
    
    #######CREATE EVENTS FROM  ACC PEAKS#########
    Event <- findpeaks((BoatdfAl$AccFilt), minpeakheight = 1, minpeakdistance = 50)
    #ev <-  ggplot() + geom_line(aes(x=BoatdfAl$V1, y=(BoatdfAl$AccFilt))) + geom_point(aes(x=Event[,2], y=Event[,1]))
    #ggplotly(ev)
    
    
    Event <- Event[,2]
    Event <- sort(Event, decreasing = FALSE)
    
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
    if(which.min(BoatdfAl$AccFilt[1:Event[1]]) >0){
      Event <- Event[-1]
    }
    
    #ev <-  ggplot() + geom_line(aes(x=BoatdfAl$V1, y=(BoatdfAl$AccFilt))) + geom_point(aes(x=Event, y=c(1)))
    #ggplotly(ev)
    
    zero <- NULL
    cycle <- NULL
    cycle[1] = tail((which(BoatdfAl$AccFilt[1:Event[1]] <0)), n=1)
    
    
    for (r in 1:(length(Event))){
      t = r+1
      # framer = Event[r,2] - BoatdfAl$V1[1]
      # framet = Event[t,2] - BoatdfAl$V1[1]
      
      tryCatch((cycle[t] = tail((which(BoatdfAl$AccFilt[Event[r]:Event[t]] <0)), n=1) + Event[r]),error = function(e) print(NA))
    }
    
    
    
    
    #evB <-  ggplot() + geom_line(aes(x=BoatdfAl$V1, y=(BoatdfAl$AccFilt))) + geom_vline(xintercept=cycle, colour = "red")
    #ggplotly(evB)
    
    #evPG <-  ggplot() + geom_line(aes(x=PelvisdfAl$V1, y=(PelvisdfAl$gyroxfilt))) + geom_vline(xintercept=cycle, colour = "red")
    #ggplotly(evPG)
    
    
    #evP <-  ggplot() + geom_line(aes(x=PelvisdfAl$V1, y=(PelvisdfAl$z_degfilt))) + geom_vline(xintercept=cycle, colour = "red")
    #ggplotly(evP)
    
    #evTG <-  ggplot() + geom_line(aes(x=ThoraxdfAl$V1, y=(ThoraxdfAl$gyroxfilt))) + geom_vline(xintercept=Event, colour = "red")
    #ggplotly(evTG)
    
    #evT <-  ggplot() + geom_line(aes(x=ThoraxdfAl$V1, y=(ThoraxdfAl$z_degfilt))) + geom_vline(xintercept=Event, colour = "red")
    #ggplotly(evT)
    BoatAcc = NULL
    BoatAcc1 = NULL
    BoatAcc2 = NULL
    PelvisGyro = NULL
    PelvisGyro1 = NULL
    PelvisGyro2 = NULL
    ThoraxGyro = NULL
    ThoraxGyro1 = NULL
    ThoraxGyro2 = NULL
    PelvisRot = NULL
    PelvisRot1 = NULL
    PelvisRot2 = NULL
    ThoraxRot = NULL
    ThoraxRot1 = NULL
    ThoraxRot2 = NULL
    
    for (c in 1:(length(Event))){
      t = c+1
      tryCatch((BoatAcc[[c]] = BoatdfAl$AccFilt[cycle[c]:cycle[t]]), error = function(e) print(NA))
      tryCatch((PelvisGyro[[c]] = PelvisdfAl$gyroxfilt[cycle[c]:cycle[t]]),error = function(e) print(NA))
      tryCatch((ThoraxGyro[[c]] = ThoraxdfAl$gyroxfilt[cycle[c]:cycle[t]]),error = function(e) print(NA))
      tryCatch((PelvisRot[[c]] = PelvisdfAl$z_degfilt[cycle[c]:cycle[t]]),error = function(e) print(NA))
      tryCatch((ThoraxRot[[c]] = ThoraxdfAl$z_degfilt[cycle[c]:cycle[t]]),error = function(e) print(NA))
      
    }
    
    
    if (max(ThoraxRot[[1]]) >0){
      Side1 <- "Left"
      
    }else{
      Side1 <- "Right"
      
    }
    
    
    
    for (i in seq(2,length(BoatAcc),2)){
      k = i-1
      tryCatch((BoatAcc2[[i-1]] = BoatAcc[[i]]), error = function(e) print(NA))
      tryCatch((BoatAcc1[[i-1]] = BoatAcc[[k]]), error = function(e) print(NA))
    }
    
    BoatAcc1 <- plyr::compact(BoatAcc1)
    BoatAcc2 <- plyr::compact(BoatAcc2)
    
    if (Side1 == "Left"){
      BoatAccLeft <- BoatAcc1
      BoatAccRight <- BoatAcc2
    } else {
      BoatAccRight <- BoatAcc1
      BoatAccLeft <- BoatAcc2
    }
    
    
    
    for (i in seq(2,length(PelvisGyro),2)){
      k = i-1
      tryCatch((PelvisGyro2[[i-1]] = PelvisGyro[[i]]), error = function(e) print(NA))
      tryCatch((PelvisGyro1[[i-1]] = PelvisGyro[[k]]), error = function(e) print(NA))
    }
    
    PelvisGyro1 <- plyr::compact(PelvisGyro1)
    PelvisGyro2 <- plyr::compact(PelvisGyro2)
    
    if (Side1 == "Left"){
      PelvisGyroLeft <- PelvisGyro1
      PelvisGyroRight <- PelvisGyro2
    } else {
      PelvisGyroRight <- PelvisGyro1
      PelvisGyroLeft <- PelvisGyro2
    }
    
    for (i in seq(2,length(ThoraxGyro),2)){
      k = i-1
      tryCatch((ThoraxGyro2[[i-1]] = ThoraxGyro[[i]]), error = function(e) print(NA))
      tryCatch((ThoraxGyro1[[i-1]] = ThoraxGyro[[k]]), error = function(e) print(NA))
    }
    
    
    ThoraxGyro1 <- plyr::compact(ThoraxGyro1)
    ThoraxGyro2 <- plyr::compact(ThoraxGyro2)
    
    if (Side1 == "Left"){
      ThoraxGyroLeft <- ThoraxGyro1
      ThoraxGyroRight <- ThoraxGyro2
    } else {
      ThoraxGyroRight <- ThoraxGyro1
      ThoraxGyroLeft <- ThoraxGyro2
    }
    
    for (i in seq(2,length(PelvisRot),2)){
      k = i-1
      tryCatch((PelvisRot2[[i-1]] = PelvisRot[[i]]), error = function(e) print(NA))
      tryCatch((PelvisRot1[[i-1]] = PelvisRot[[k]]), error = function(e) print(NA))
    }
    
    PelvisRot1 <- plyr::compact(PelvisRot1)
    PelvisRot2 <- plyr::compact(PelvisRot2)
    
    if (Side1 == "Left"){
      PelvisRotLeft <- PelvisRot1
      PelvisRotRight <- PelvisRot2
    } else {
      PelvisRotRight <- PelvisRot1
      PelvisRotLeft <- PelvisRot2
    }
    
    for (i in seq(2,length(ThoraxRot),2)){
      k = i-1
      tryCatch((ThoraxRot2[[i-1]] = ThoraxRot[[i]]), error = function(e) print(NA))
      tryCatch((ThoraxRot1[[i-1]] = ThoraxRot[[k]]), error = function(e) print(NA))
    }
    
    ThoraxRot1 <- plyr::compact(ThoraxRot1)
    ThoraxRot2 <- plyr::compact(ThoraxRot2)
    
    if (Side1 == "Left"){
      ThoraxRotLeft <- ThoraxRot1
      ThoraxRotRight <- ThoraxRot2
    } else {
      ThoraxRotRight <- ThoraxRot1
      ThoraxRotLeft <- ThoraxRot2
    }
    
    
    #############OLD#############
    
    
    Minimaxdf2 <-  subset(MinimaxdfAl, `Time`>=60 & `Time`<=90)
    
    stks1 = which(Minimaxdf2$Stk == 1)
    stks2 = which(Minimaxdf2$Stk == 2)
    stks = c(stks1, stks2)
    stks = sort(stks)
    
    MinimaxdfVelocity = NULL
    for (i in 2:length(stks)){
      k = i-1
      
      MinimaxdfVelocity[[k]] = Minimaxdf2[stks[k]:stks[i],VAcc]
      
    }
    
    
    MinimaxdfVelocity1=NULL
    MinimaxdfVelocity2=NULL
    
    for (i in seq(2,length(MinimaxdfVelocity),2)){
      k = i-1
      tryCatch((MinimaxdfVelocity2[[i-1]] = MinimaxdfVelocity[[i]]), error = function(e) print(NA))
      tryCatch((MinimaxdfVelocity1[[i-1]] = MinimaxdfVelocity[[k]]), error = function(e) print(NA))
      
      
      #tryCatch((PelvisGyro[[i]] = PelvisGyro[[i]]*-1), error = function(e) print(NA))
    }
    
    MinimaxdfVelocity1 <- plyr::compact(MinimaxdfVelocity1)
    MinimaxdfVelocity2 <- plyr::compact(MinimaxdfVelocity2)
    
    
    MinimaxdfRoll = NULL
    for (i in 2:length(stks)){
      k = i-1
      MinimaxdfRoll[[k]] = Minimaxdf2[stks[k]:stks[i],"Roll"]
      
    }
    
    
    
    if (max(MinimaxdfRoll[[1]]) >2){
      side = "right"
      MinimaxdfVelocityRight <- MinimaxdfVelocity1
      MinimaxdfVelocityLeft <- MinimaxdfVelocity2
      
    }else{
      side = "left"
      MinimaxdfVelocityLeft <- MinimaxdfVelocity1
      MinimaxdfVelocityRight <- MinimaxdfVelocity2
      
    }
    
    MinimaxdfVelocityRight <- plyr::compact(MinimaxdfVelocity1)
    MinimaxdfVelocityLeft <- plyr::compact(MinimaxdfVelocity2)
    
    
    
    
    MinimaxdfVelocityRightNorm = NULL
    for (i in 1:length(MinimaxdfVelocityRight)){
      xvalues = 1:length(MinimaxdfVelocityRight[[i]])
      yvalues = MinimaxdfVelocityRight[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      MinimaxdfVelocityRightNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    MinimaxdfVelocityLeftNorm = NULL
    for (i in 1:length(MinimaxdfVelocityLeft)){
      xvalues = 1:length(MinimaxdfVelocityLeft[[i]])
      yvalues = MinimaxdfVelocityLeft[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      MinimaxdfVelocityLeftNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    
    
    MinimaxdfVelocityRightNormCombined = do.call(cbind, MinimaxdfVelocityRightNorm)
    MinimaxdfVelocityRightNormMean = rowMeans(MinimaxdfVelocityRightNormCombined)
    MinimaxdfVelocityRightNormSD = apply(MinimaxdfVelocityRightNormCombined[,-1], 1, sd)
    MinimaxdfVelocityRightNormPEB = MinimaxdfVelocityRightNormMean + MinimaxdfVelocityRightNormSD
    MinimaxdfVelocityRightNormNEB = MinimaxdfVelocityRightNormMean - MinimaxdfVelocityRightNormSD
    
    MinimaxdfVelocityRightNormCombined2 = as.data.frame(MinimaxdfVelocityRightNormCombined)
    MinimaxdfVelocityRightNormCombined2$perc = c(0:100)
    MinimaxdfVelocityRightNormCombined2 <- melt(MinimaxdfVelocityRightNormCombined2, id.vars="perc")
    
    MinimaxdfVelocityRightNormMeanMax <- round(max(MinimaxdfVelocityRightNormMean),2)
    MinimaxdfVelocityRightNormMeanMaxPC <- which.max(MinimaxdfVelocityRightNormMean)-1
    
    
    
    output$MinimaxVelRight<- renderPlotly({
      
      fig <- plot_ly(MinimaxdfVelocityRightNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Minimax Velocity (m/s)"))
      
      fig
    })
    
    
    MinimaxdfVelocityLeftNormCombined = do.call(cbind, MinimaxdfVelocityLeftNorm)
    MinimaxdfVelocityLeftNormMean = rowMeans(MinimaxdfVelocityLeftNormCombined)
    MinimaxdfVelocityLeftNormSD = apply(MinimaxdfVelocityLeftNormCombined[,-1], 1, sd)
    MinimaxdfVelocityLeftNormPEB = MinimaxdfVelocityLeftNormMean + MinimaxdfVelocityLeftNormSD
    MinimaxdfVelocityLeftNormNEB = MinimaxdfVelocityLeftNormMean - MinimaxdfVelocityLeftNormSD
    
    MinimaxdfVelocityLeftNormCombined2 = as.data.frame(MinimaxdfVelocityLeftNormCombined)
    MinimaxdfVelocityLeftNormCombined2$perc = c(0:100)
    MinimaxdfVelocityLeftNormCombined2 <- melt(MinimaxdfVelocityLeftNormCombined2, id.vars="perc")
    
    
    MinimaxdfVelocityLeftNormMeanMax <- round(max(MinimaxdfVelocityLeftNormMean),2)
    MinimaxdfVelocityLeftNormMeanMaxPC <- which.max(MinimaxdfVelocityLeftNormMean)-1
    
    output$MinimaxVelLeft<- renderPlotly({
      
      fig <- plot_ly(MinimaxdfVelocityLeftNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Minimax Velocity (m/s)"))
      
      fig
    })
    
    
    output$MinimaxVelCombined<- renderPlotly({
      
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = MinimaxdfVelocityRightNormMean, type = 'scatter', mode = 'lines',
                  line = list(color = 'rgba(250, 0, 0, 1)'),
                  showlegend = TRUE, name = 'Right Stroke Minimax Velocity')%>% 
        add_trace(y = MinimaxdfVelocityRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityLeftNormMean, type = 'scatter', mode = 'lines', line = list(color = 'green'),
                  showlegend = TRUE, name = 'Left Stroke Minimax Velocity')%>% 
        add_trace(y = MinimaxdfVelocityLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Minimax Velocity (m/s)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        legend = list(x = 0.5, y = 0.9)) %>%
        config(displayModeBar = F)
      
      
      fig
      
    })
    
    #Boat Acc Validation
    # rm(BoatAcccombined)
    # BoatAcccombined <- listToDF(BoatAcc)
    # BoatAcccombined2 <- melt(BoatAcccombined)
    # 
    # 
    # q <-  ggplot(BoatAcccombined2) + geom_line(aes(Var1,value, colour = Var2))+ theme(legend.position = "none")
    # ggplotly(q)
    # 
    # plot(BoatAcc[[86]])
    
    
    ###NORMALISE BOAT ACC DATA AND PLOT #############
    
    # BoatAccNorm = NULL
    # 
    # for (i in 1:length(BoatAcc)){
    #   xvalues = 1:length(BoatAcc[[i]])
    #   yvalues = BoatAcc[[i]]
    #   output_x_vals <- seq(1,length(xvalues),length.out=101)
    #   #
    #   # compute the interpolated values; this would be done for each input time series
    #   #
    #   #interp_output[[,i]]<-
    # 
    #   BoatAccNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    # }
    # 
    # 
    # 
    # BoatAccNormCombined = do.call(cbind, BoatAccNorm)
    # BoatAccNormMean = rowMeans(BoatAccNormCombined)
    # BoatAccNormSD = apply(BoatAccNormCombined[,-1], 1, sd)
    # BoatAccNormPEB = BoatAccNormMean + BoatAccNormSD
    # BoatAccNormNEB = BoatAccNormMean - BoatAccNormSD
    # 
    # BoatAccNormCombined2 = as.data.frame(BoatAccNormCombined)
    # BoatAccNormCombined2$perc = c(0:100)
    # BoatAccNormCombined2 <- melt(BoatAccNormCombined2, id.vars="perc")
    # 
    # 
    # 
    # output$BoatAcc<- renderPlotly({
    # 
    #   q <-  ggplot(BoatAccNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none")
    #   scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Boat Acceleration (m/s/s)")
    #   ggplotly(q)
    # })
    # BoatAccNormMeanSD = cbind.data.frame(BoatAccNormMean,BoatAccNormPEB,BoatAccNormNEB)
    #BoatAccNormMeanSD <-  BoatAccNormMeanSD
    
    # outputBoatAccMean<- renderPlotly({
    # 
    #   q <- ggplot(BoatAccNormMeanSD) + geom_line(aes(c(0:100),BoatAccNormMean))+ geom_line(aes(c(0:100),BoatAccNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),BoatAccNormNEB),linetype = "dashed")
    #   scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Boat Acceleration (m/s/s)")
    #   ggplotly(q)
    # })
    
    
    
    ####################################
    #Boat normalised and plot
    
    BoatAccRightNorm = NULL
    
    for (i in 1:length(BoatAccRight)){
      xvalues = 1:length(BoatAccRight[[i]])
      yvalues = BoatAccRight[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      BoatAccRightNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    
    
    BoatAccRightNormCombined = do.call(cbind, BoatAccRightNorm)
    BoatAccRightNormMean = rowMeans(BoatAccRightNormCombined)
    BoatAccRightNormSD = apply(BoatAccRightNormCombined[,-1], 1, sd)
    BoatAccRightNormPEB = BoatAccRightNormMean + BoatAccRightNormSD
    BoatAccRightNormNEB = BoatAccRightNormMean - BoatAccRightNormSD
    
    BoatAccRightNormCombined2 = as.data.frame(BoatAccRightNormCombined)
    BoatAccRightNormCombined2$perc = c(0:100)
    BoatAccRightNormCombined2 <- melt(BoatAccRightNormCombined2, id.vars="perc")
    
    BoatAccRightNormMeanSD = cbind.data.frame(BoatAccRightNormMean,BoatAccRightNormPEB,BoatAccRightNormNEB)
    
    BoatAccRightNormMeanMax <- round(max(BoatAccRightNormMean),2)
    BoatAccRightNormMeanMaxPC <- which.max(BoatAccRightNormMean)-1
    
    output$BoatAccRight<- renderPlotly({
      
      fig <- plot_ly(BoatAccRightNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Boat Acceleration (m/s/s)"))
      
      fig
    })
    
    
    
    
    
    
    #BoatAccRightNormMeanSD <-  BoatAccRightNormMeanSD
    
    # outputBoatAccRightMean<- renderPlotly({
    #   
    #   q <- ggplot(BoatAccRightNormMeanSD) + geom_line(aes(c(0:100),BoatAccRightNormMean))+ geom_line(aes(c(0:100),BoatAccRightNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),BoatAccRightNormNEB),linetype = "dashed")
    #   scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Boat Acceleration (m/s/s)")
    #   ggplotly(q)
    # })
    
    
    ####################################
    #Boat normalised and plot
    
    BoatAccLeftNorm = NULL
    
    for (i in 1:length(BoatAccLeft)){
      xvalues = 1:length(BoatAccLeft[[i]])
      yvalues = BoatAccLeft[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      BoatAccLeftNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    
    
    BoatAccLeftNormCombined = do.call(cbind, BoatAccLeftNorm)
    BoatAccLeftNormMean = rowMeans(BoatAccLeftNormCombined)
    BoatAccLeftNormSD = apply(BoatAccLeftNormCombined[,-1], 1, sd)
    BoatAccLeftNormPEB = BoatAccLeftNormMean + BoatAccLeftNormSD
    BoatAccLeftNormNEB = BoatAccLeftNormMean - BoatAccLeftNormSD
    
    BoatAccLeftNormCombined2 = as.data.frame(BoatAccLeftNormCombined)
    BoatAccLeftNormCombined2$perc = c(0:100)
    BoatAccLeftNormCombined2 <- melt(BoatAccLeftNormCombined2, id.vars="perc")
    
    BoatAccLeftNormMeanSD = cbind.data.frame(BoatAccLeftNormMean,BoatAccLeftNormPEB,BoatAccLeftNormNEB)
    
    BoatAccLeftNormMeanMax <- round(max(BoatAccLeftNormMean),2)
    BoatAccLeftNormMeanMaxPC <- which.max(BoatAccLeftNormMean)-1
    
    output$BoatAccLeft<- renderPlotly({
      
      fig <- plot_ly(BoatAccLeftNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Boat Acceleration (m/s/s)"))
      
      fig
    })
    #BoatAccLeftNormMeanSD <-  BoatAccLeftNormMeanSD
    
    
    #######################
    
    
    output$BoatAccCombined<- renderPlotly({
      
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = BoatAccRightNormMean, type = 'scatter', mode = 'lines',
                  line = list(color = 'rgba(250, 0, 0, 1)'),
                  showlegend = TRUE, name = 'Right Stroke Boat Acceleration')%>% 
        add_trace(y = BoatAccRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = BoatAccRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = BoatAccLeftNormMean, type = 'scatter', mode = 'lines', line = list(color = 'green'),
                  showlegend = TRUE, name = 'Left Stroke Boat Acceleration')%>% 
        add_trace(y = BoatAccLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = BoatAccLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Boat Acceleration (m/s/s)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        legend = list(x = 0.5, y = 0.9)) %>%
        config(displayModeBar = F)
      
      
      fig
      
    })
    
    # q <-  ggplot(BoatAccNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none")
    # ggplotly(q)
    
    ###NORMALISE PELVIS DATA AND PLOT #############
    
    
    
    # PelvisGyroNorm = NULL
    # for (i in 1:length(PelvisGyro)){
    #   xvalues = 1:length(PelvisGyro[[i]])
    #   yvalues = PelvisGyro[[i]]
    #   output_x_vals <- seq(1,length(xvalues),length.out=101)
    #   #
    #   # compute the interpolated values; this would be done for each input time series
    #   #
    #   #interp_output[[,i]]<-
    # 
    #   PelvisGyroNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    # }
    # 
    # 
    # PelvisGyroNormCombined = do.call(cbind, PelvisGyroNorm)
    # PelvisGyroNormMean = (rowMeans(PelvisGyroNormCombined))
    # PelvisGyroNormSD = (apply(PelvisGyroNormCombined[,-1], 1, sd))
    # PelvisGyroNormPEB = (PelvisGyroNormMean + PelvisGyroNormSD)
    # PelvisGyroNormNEB = (PelvisGyroNormMean - PelvisGyroNormSD)
    # 
    # if (PelvisGyroNormMean[1] > 0){
    #   sign = 1
    # }else{
    #   sign = -1
    # }
    # 
    # PelvisGyroNormMean <- PelvisGyroNormMean*sign
    # PelvisGyroNormSD <-  PelvisGyroNormSD*sign
    # PelvisGyroNormPEB <-  PelvisGyroNormPEB*sign
    # PelvisGyroNormNEB <-  PelvisGyroNormNEB*sign
    # 
    # 
    # PelvisGyroNormCombined2 = (as.data.frame(PelvisGyroNormCombined))*sign
    # PelvisGyroNormCombined2$perc = c(0:100)
    # PelvisGyroNormCombined2 <- melt(PelvisGyroNormCombined2, id.vars="perc")
    # 
    # 
    # 
    # output$PelvisGyro<- renderPlotly({
    #   
    #   q <-  ggplot(PelvisGyroNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none") +
    #     scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Rotation Velocity (deg/s)")
    #   ggplotly(q)
    # })
    # PelvisGyroNormMeanSD = cbind.data.frame(PelvisGyroNormMean,PelvisGyroNormPEB,PelvisGyroNormNEB)
    
    # output$PelvisGyroMean<- renderPlotly({
    #   
    #   q <-  ggplot(PelvisGyroNormMeanSD) + geom_line(aes(c(0:100),PelvisGyroNormMean))+ geom_line(aes(c(0:100),PelvisGyroNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),PelvisGyroNormNEB),linetype = "dashed")+
    #     scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Rotation Velocity (deg/s)")
    #   ggplotly(q)
    # })
    # 
    #####################
    
    PelvisGyroRightNorm = NULL
    for (i in 1:length(PelvisGyroRight)){
      xvalues = 1:length(PelvisGyroRight[[i]])
      yvalues = PelvisGyroRight[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      PelvisGyroRightNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    
    PelvisGyroRightNormCombined = do.call(cbind, PelvisGyroRightNorm)
    PelvisGyroRightNormMean = (rowMeans(PelvisGyroRightNormCombined))
    PelvisGyroRightNormSD = (apply(PelvisGyroRightNormCombined[,-1], 1, sd))
    PelvisGyroRightNormPEB = (PelvisGyroRightNormMean + PelvisGyroRightNormSD)
    PelvisGyroRightNormNEB = (PelvisGyroRightNormMean - PelvisGyroRightNormSD)
    
    if (PelvisGyroRightNormMean[1] > 0){
      sign = 1
    }else{
      sign = -1
    }
    
    PelvisGyroRightNormMean <- PelvisGyroRightNormMean*sign
    PelvisGyroRightNormSD <-  PelvisGyroRightNormSD*sign
    PelvisGyroRightNormPEB <-  PelvisGyroRightNormPEB*sign
    PelvisGyroRightNormNEB <-  PelvisGyroRightNormNEB*sign
    
    
    PelvisGyroRightNormCombined2 = (as.data.frame(PelvisGyroRightNormCombined))*sign
    PelvisGyroRightNormCombined2$perc = c(0:100)
    PelvisGyroRightNormCombined2 <- melt(PelvisGyroRightNormCombined2, id.vars="perc")
    
    PelvisGyroRightNormMeanSD = cbind.data.frame(PelvisGyroRightNormMean,PelvisGyroRightNormPEB,PelvisGyroRightNormNEB)
    
    PelvisGyroRightNormMeanMax <- round(max(PelvisGyroRightNormMean),2)
    PelvisGyroRightNormMeanMaxPC <- which.max(PelvisGyroRightNormMean)-1
    
    output$PelvisGyroRight<- renderPlotly({
      
      fig <- plot_ly(PelvisGyroRightNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Rotation Velocity (deg/s)"))
      
      fig
      
    })
    
    
    # output$PelvisGyroRightMean<- renderPlotly({
    #   
    #   q <-  ggplot(PelvisGyroRightNormMeanSD) + geom_line(aes(c(0:100),PelvisGyroRightNormMean))+ geom_line(aes(c(0:100),PelvisGyroRightNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),PelvisGyroRightNormNEB),linetype = "dashed")+
    #     scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Rotation Velocity (deg/s)")
    #   ggplotly(q)
    # })
    
    
    
    ##########################
    PelvisGyroLeftNorm = NULL
    for (i in 1:length(PelvisGyroLeft)){
      xvalues = 1:length(PelvisGyroLeft[[i]])
      yvalues = PelvisGyroLeft[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      PelvisGyroLeftNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    
    PelvisGyroLeftNormCombined = do.call(cbind, PelvisGyroLeftNorm)
    PelvisGyroLeftNormMean = (rowMeans(PelvisGyroLeftNormCombined))
    PelvisGyroLeftNormSD = (apply(PelvisGyroLeftNormCombined[,-1], 1, sd))
    PelvisGyroLeftNormPEB = (PelvisGyroLeftNormMean + PelvisGyroLeftNormSD)
    PelvisGyroLeftNormNEB = (PelvisGyroLeftNormMean - PelvisGyroLeftNormSD)
    
    if (PelvisGyroLeftNormMean[1] > 0){
      sign = 1
    }else{
      sign = -1
    }
    
    PelvisGyroLeftNormMean <- PelvisGyroLeftNormMean*sign
    PelvisGyroLeftNormSD <-  PelvisGyroLeftNormSD*sign
    PelvisGyroLeftNormPEB <-  PelvisGyroLeftNormPEB*sign
    PelvisGyroLeftNormNEB <-  PelvisGyroLeftNormNEB*sign
    
    
    PelvisGyroLeftNormCombined2 = (as.data.frame(PelvisGyroLeftNormCombined))*sign
    PelvisGyroLeftNormCombined2$perc = c(0:100)
    PelvisGyroLeftNormCombined2 <- melt(PelvisGyroLeftNormCombined2, id.vars="perc")
    
    PelvisGyroLeftNormMeanMax <- round(max(PelvisGyroLeftNormMean),2)
    PelvisGyroLeftNormMeanMaxPC <- which.max(PelvisGyroLeftNormMean)-1
    
    output$PelvisGyroLeft<- renderPlotly({
      
      fig <- plot_ly(PelvisGyroLeftNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Rotation Velocity (deg/s)"))
      
      fig
    })
    
    # output$PelvisGyroLeftMean<- renderPlotly({
    #   
    #   q <-  ggplot(PelvisGyroLeftNormMeanSD) + geom_line(aes(c(0:100),PelvisGyroLeftNormMean))+ geom_line(aes(c(0:100),PelvisGyroLeftNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),PelvisGyroLeftNormNEB),linetype = "dashed")+
    #     scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Rotation Velocity (deg/s)")
    #   ggplotly(q)
    # })
    
    
    
    output$PelvisGyroCombined<- renderPlotly({
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = PelvisGyroRightNormMean, type = 'scatter', mode = 'lines',
                  line = list(color = 'rgba(250, 0, 0, 1)'),
                  showlegend = TRUE, name = 'Right Stroke Pelvis Rotation')%>% 
        add_trace(y = PelvisGyroRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisGyroRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisGyroLeftNormMean, type = 'scatter', mode = 'lines', line = list(color = 'green'),
                  showlegend = TRUE, name = 'Left Stroke Pelvis Rotation')%>% 
        add_trace(y = PelvisGyroLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisGyroLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Rotation Velocity (m/s)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        legend = list(x = 0.5, y = 0.9)) %>%
        config(displayModeBar = F)
      
      
      fig
      
      
      
      
    })
    
    
    
    ###NORMALISE THORAX DATA AND PLOT #############
    
    
    # ThoraxGyroNorm = NULL
    # for (i in 1:length(ThoraxGyro)){
    #   xvalues = 1:length(ThoraxGyro[[i]])
    #   yvalues = ThoraxGyro[[i]]
    #   output_x_vals <- seq(1,length(xvalues),length.out=101)
    #   #
    #   # compute the interpolated values; this would be done for each input time series
    #   #
    #   #interp_output[[,i]]<-
    # 
    #   ThoraxGyroNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    # }
    # 
    # ThoraxGyroNormCombined = do.call(cbind, ThoraxGyroNorm)
    # 
    # ThoraxGyroNormMean = (rowMeans(ThoraxGyroNormCombined))
    # ThoraxGyroNormSD = (apply(ThoraxGyroNormCombined[,-1], 1, sd))
    # ThoraxGyroNormPEB = (ThoraxGyroNormMean + ThoraxGyroNormSD)
    # ThoraxGyroNormNEB = (ThoraxGyroNormMean - ThoraxGyroNormSD)
    # 
    # if (ThoraxGyroNormMean[1] > 0){
    #   sign = 1
    # }else{
    #   sign = -1
    # }
    # 
    # ThoraxGyroNormMean <- ThoraxGyroNormMean*sign
    # ThoraxGyroNormSD <-  ThoraxGyroNormSD*sign
    # ThoraxGyroNormPEB <-  ThoraxGyroNormPEB*sign
    # ThoraxGyroNormNEB <-  ThoraxGyroNormNEB*sign
    # 
    # 
    # ThoraxGyroNormCombined2 = (as.data.frame(ThoraxGyroNormCombined))*sign
    # ThoraxGyroNormCombined2$perc = c(0:100)
    # ThoraxGyroNormCombined2 <- melt(ThoraxGyroNormCombined2, id.vars="perc")
    # 
    # output$ThoraxGyro<- renderPlotly({
    #   q <-  ggplot(ThoraxGyroNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none")+
    #     scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Rotation Velocity (deg/s)")
    #   ggplotly(q)
    # })
    # 
    # 
    # ThoraxGyroNormMeanSD = cbind.data.frame(ThoraxGyroNormMean,ThoraxGyroNormPEB,ThoraxGyroNormNEB)
    # output$ThoraxGyroMean<- renderPlotly({
    #   q <-  ggplot(ThoraxGyroNormMeanSD) + geom_line(aes(c(0:100),ThoraxGyroNormMean))+ geom_line(aes(c(0:100),ThoraxGyroNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),ThoraxGyroNormNEB),linetype = "dashed")+
    #     scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Rotation Velocity (deg/s)")
    #   ggplotly(q)
    # 
    # 
    # })
    
    ##########################################
    #Thorax normalised and plot
    
    ThoraxGyroRightNorm = NULL
    for (i in 1:length(ThoraxGyroRight)){
      xvalues = 1:length(ThoraxGyroRight[[i]])
      yvalues = ThoraxGyroRight[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      ThoraxGyroRightNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    ThoraxGyroRightNormCombined = do.call(cbind, ThoraxGyroRightNorm)
    
    ThoraxGyroRightNormMean = (rowMeans(ThoraxGyroRightNormCombined))
    ThoraxGyroRightNormSD = (apply(ThoraxGyroRightNormCombined[,-1], 1, sd))
    ThoraxGyroRightNormPEB = (ThoraxGyroRightNormMean + ThoraxGyroRightNormSD)
    ThoraxGyroRightNormNEB = (ThoraxGyroRightNormMean - ThoraxGyroRightNormSD)
    
    if (ThoraxGyroRightNormMean[1] > 0){
      sign = 1
    }else{
      sign = -1
    }
    
    ThoraxGyroRightNormMean <- ThoraxGyroRightNormMean*sign
    ThoraxGyroRightNormSD <-  ThoraxGyroRightNormSD*sign
    ThoraxGyroRightNormPEB <-  ThoraxGyroRightNormPEB*sign
    ThoraxGyroRightNormNEB <-  ThoraxGyroRightNormNEB*sign
    
    
    ThoraxGyroRightNormCombined2 = (as.data.frame(ThoraxGyroRightNormCombined))*sign
    ThoraxGyroRightNormCombined2$perc = c(0:100)
    ThoraxGyroRightNormCombined2 <- melt(ThoraxGyroRightNormCombined2, id.vars="perc")
    
    ThoraxGyroRightNormMeanMax <- round(max(ThoraxGyroRightNormMean),2)
    ThoraxGyroRightNormMeanMaxPC <- which.max(ThoraxGyroRightNormMean)-1
    
    
    output$ThoraxGyroRight<- renderPlotly({
      
      
      fig <- plot_ly(ThoraxGyroRightNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Rotation Velocity (deg/s)"))
      
      fig
    })
    
    
    # ThoraxGyroRightNormMeanSD = cbind.data.frame(ThoraxGyroRightNormMean,ThoraxGyroRightNormPEB,ThoraxGyroRightNormNEB)
    # output$ThoraxGyroRightMean<- renderPlotly({
    #   q <-  ggplot(ThoraxGyroRightNormMeanSD) + geom_line(aes(c(0:100),ThoraxGyroRightNormMean))+ geom_line(aes(c(0:100),ThoraxGyroRightNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),ThoraxGyroRightNormNEB),linetype = "dashed")+
    #     scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Rotation Velocity (deg/s)")
    #   ggplotly(q)
    #   
    #   
    # })
    
    
    ##########################################
    
    ThoraxGyroLeftNorm = NULL
    for (i in 1:length(ThoraxGyroLeft)){
      xvalues = 1:length(ThoraxGyroLeft[[i]])
      yvalues = ThoraxGyroLeft[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      ThoraxGyroLeftNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    ThoraxGyroLeftNormCombined = do.call(cbind, ThoraxGyroLeftNorm)
    
    ThoraxGyroLeftNormMean = (rowMeans(ThoraxGyroLeftNormCombined))
    ThoraxGyroLeftNormSD = (apply(ThoraxGyroLeftNormCombined[,-1], 1, sd))
    ThoraxGyroLeftNormPEB = (ThoraxGyroLeftNormMean + ThoraxGyroLeftNormSD)
    ThoraxGyroLeftNormNEB = (ThoraxGyroLeftNormMean - ThoraxGyroLeftNormSD)
    
    if (ThoraxGyroLeftNormMean[1] > 0){
      sign = 1
    }else{
      sign = -1
    }
    
    ThoraxGyroLeftNormMean <- ThoraxGyroLeftNormMean*sign
    ThoraxGyroLeftNormSD <-  ThoraxGyroLeftNormSD*sign
    ThoraxGyroLeftNormPEB <-  ThoraxGyroLeftNormPEB*sign
    ThoraxGyroLeftNormNEB <-  ThoraxGyroLeftNormNEB*sign
    
    
    ThoraxGyroLeftNormCombined2 = (as.data.frame(ThoraxGyroLeftNormCombined))*sign
    ThoraxGyroLeftNormCombined2$perc = c(0:100)
    ThoraxGyroLeftNormCombined2 <- melt(ThoraxGyroLeftNormCombined2, id.vars="perc")
    
    ThoraxGyroLeftNormMeanMax <- round(max(ThoraxGyroLeftNormMean),2)
    ThoraxGyroLeftNormMeanMaxPC <- which.max(ThoraxGyroLeftNormMean)-1
    
    
    output$ThoraxGyroLeft<- renderPlotly({
      
      fig <- plot_ly(ThoraxGyroLeftNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Rotation Velocity (deg/s)"))
      
      fig
    })
    
    
    # ThoraxGyroLeftNormMeanSD = cbind.data.frame(ThoraxGyroLeftNormMean,ThoraxGyroLeftNormPEB,ThoraxGyroLeftNormNEB)
    # output$ThoraxGyroLeftMean<- renderPlotly({
    #   q <-  ggplot(ThoraxGyroLeftNormMeanSD) + geom_line(aes(c(0:100),ThoraxGyroLeftNormMean))+ geom_line(aes(c(0:100),ThoraxGyroLeftNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),ThoraxGyroLeftNormNEB),linetype = "dashed")+
    #     scale_x_continuous(name="% of stroke")+
    #     scale_y_continuous(name="Rotation Velocity (deg/s)")
    #   ggplotly(q)
    #   
    #   
    # })
    
    output$ThoraxGyroCombined<- renderPlotly({
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = ThoraxGyroRightNormMean, type = 'scatter', mode = 'lines',
                  line = list(color = 'rgba(250, 0, 0, 1)'),
                  showlegend = TRUE, name = 'Right Stroke Thorax Rotation')%>% 
        add_trace(y = ThoraxGyroRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxGyroRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxGyroLeftNormMean, type = 'scatter', mode = 'lines', line = list(color = 'green'),
                  showlegend = TRUE, name = 'Left Stroke Thorax Rotation')%>% 
        add_trace(y = ThoraxGyroLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxGyroLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Rotation Velocity (m/s)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        legend = list(x = 0.5, y = 0.9)) %>%
        config(displayModeBar = F)
      
      
      fig
    })
    
    ###CREATE COMBINED PLOT OF MEANS #############
    
    # output$Sequence<- renderPlotly({
    # q <- ggplot() + geom_line(aes(c(0:100),BoatAccNormMean*100,colour = "BoatAcc"))+ geom_line(aes(c(0:100),BoatAccNormPEB*100,colour = "BoatAcc"), linetype = "dashed")+ geom_line(aes(c(0:100),BoatAccNormNEB*100, colour = "BoatAcc"),linetype = "dashed") +
    #   geom_line(aes(c(0:100),ThoraxGyroNormMean,colour = "Thorax"))+ geom_line(aes(c(0:100),ThoraxGyroNormPEB,colour = "Thorax"), linetype = "dashed")+ geom_line(aes(c(0:100),ThoraxGyroNormNEB, colour = "Thorax"),linetype = "dashed") +
    #   geom_line(aes(c(0:100),PelvisGyroNormMean, colour = "Pelvis"))+ geom_line(aes(c(0:100),PelvisGyroNormPEB, colour = "Pelvis"), linetype = "dashed")+ geom_line(aes(c(0:100),PelvisGyroNormNEB, colour = "Pelvis"),linetype = "dashed") +
    #   scale_color_manual( values = c('BoatAcc' = 'black','Thorax' = 'red', 'Pelvis' = 'blue')) + labs(color = 'Y series')+
    #   scale_x_continuous(name="% of stroke")+
    #   scale_y_continuous(name="Rotation Velocity (deg/s)")
    #   ggplotly(q)
    # })
    
    
    output$SequenceRight<- renderPlotly({
      
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = BoatAccRightNormMean*50, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  line = list(color = 'black', dash = 'dot'),
                  showlegend = TRUE, name = 'Boat Acceleration')%>% 
        add_trace(y = BoatAccRightNormPEB*50, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(0,0,0,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = BoatAccRightNormNEB*50, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(0,0,0,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityRightNormMean, type = 'scatter', mode = 'lines',yaxis = "y2",
                  line = list(color = 'rgba(122, 39, 160, 1)', dash = 'dot'),
                  showlegend = TRUE, name = 'Minimax Velocity')%>% 
        add_trace(y = MinimaxdfVelocityRightNormPEB, type = 'scatter', mode = 'lines',yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(122, 39, 160, 0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityRightNormNEB, type = 'scatter', mode = 'lines',yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(122, 39, 160,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxGyroRightNormMean, type = 'scatter', mode = 'lines', line = list(color = 'rgba(21, 0, 250, 1)'),
                  showlegend = TRUE, name = 'Thorax Rotation')%>% 
        add_trace(y = ThoraxGyroRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(21, 0, 250, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxGyroRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(21, 0, 250, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisGyroRightNormMean, type = 'scatter', mode = 'lines',line = list(color = 'rgba(255, 117, 31,1)'),
                  showlegend = TRUE, name = 'Pelvis Rotation')%>% 
        add_trace(y = PelvisGyroRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 117, 31, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisGyroRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 117, 31, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Rotation Velocity (m/s)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE,
                                     range = c(-50,200)
                        ),
                        yaxis2 = list(title = "Velocity (m/s)",
                                      overlaying = "y",
                                      side = "right",
                                      showgrid = FALSE,
                                      showline = FALSE,
                                      showticklabels = TRUE,
                                      ticks = 'outside',
                                      zeroline = TRUE,
                                      range = c(3,4),
                                      color = 'rgba(122, 39, 160, 1)'),
                        margin = list(l = 0, r = 50, b = 0, t = 0),
                        legend = list(orientation = 'h', x =0, y = -0.2)) %>%
        config(displayModeBar = F)
      
      
      fig
      
      
      
      
    })
    
    output$SequenceLeft<- renderPlotly({
      
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = BoatAccLeftNormMean*50, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  line = list(color = 'black',dash = 'dot'),
                  showlegend = TRUE, name = 'Boat Acceleration')%>% 
        add_trace(y = BoatAccLeftNormPEB*50, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(0,0,0,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = BoatAccLeftNormNEB*50, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(0,0,0,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityLeftNormMean, type = 'scatter', mode = 'lines',yaxis = "y2",
                  line = list(color = 'rgba(122, 39, 160, 1)',dash = 'dot'),
                  showlegend = TRUE, name = 'Minimax Velocity')%>% 
        add_trace(y = MinimaxdfVelocityLeftNormPEB, type = 'scatter', mode = 'lines',yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(122, 39, 160, 0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityLeftNormNEB, type = 'scatter', mode = 'lines',yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(122, 39, 160,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxGyroLeftNormMean, type = 'scatter', mode = 'lines', line = list(color = 'rgba(21, 0, 250, 1)'),
                  showlegend = TRUE, name = 'Thorax Rotation')%>% 
        add_trace(y = ThoraxGyroLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(21, 0, 250, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxGyroLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(21, 0, 250, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisGyroLeftNormMean, type = 'scatter', mode = 'lines',line = list(color = 'rgba(255, 117, 31,1)'),
                  showlegend = TRUE, name = 'Pelvis Rotation')%>% 
        add_trace(y = PelvisGyroLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 117, 31,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisGyroLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 117, 31,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Rotation Velocity (m/s)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE,
                                     range = c(-50,200)),
                        yaxis2 = list(title = "Stroke Rate (spm)",
                                      overlaying = "y",
                                      side = "right",
                                      showgrid = FALSE,
                                      showline = FALSE,
                                      showticklabels = TRUE,
                                      ticks = 'outside',
                                      zeroline = FALSE,
                                      range = c(3,4),
                                      color = 'rgba(122, 39, 160, 1)'),
                        margin = list(l = 0, r = 50, b = 0, t = 0),
                        legend = list(orientation = 'h', x =0, y = -0.2)) %>%
        config(displayModeBar = F)
      
      
      fig
    })
    
    
    SequencedataRight <- data.frame(MinimaxdfVelocityRightNormMeanMax, MinimaxdfVelocityRightNormMeanMaxPC, PelvisGyroRightNormMeanMax,  PelvisGyroRightNormMeanMaxPC, ThoraxGyroRightNormMeanMax, ThoraxGyroRightNormMeanMaxPC)
    colnames(SequencedataRight) <- c("Minimax Velocity Max (m/s)", "%", "Pelvis Rotation Velocity Max (deg/s)",  "%", "Thorax Rotation Velocity Max (deg/s)", "%")
    SequencedataLeft <- data.frame(MinimaxdfVelocityLeftNormMeanMax, MinimaxdfVelocityLeftNormMeanMaxPC, PelvisGyroLeftNormMeanMax,  PelvisGyroLeftNormMeanMaxPC, ThoraxGyroLeftNormMeanMax, ThoraxGyroLeftNormMeanMaxPC)
    colnames(SequencedataLeft) <- c("Minimax Velocity Max (m/s)", "%", "Pelvis Rotation Velocity Max (deg/s)",  "%", "Thorax Rotation Velocity Max (deg/s)", "%")
    
    Sequencedata <- rbind(SequencedataRight,SequencedataLeft)
    
    
    output$table_VelSequence <- renderDataTable({ 
      
      
      datatable(Sequencedata,  rownames = c("Right", "Left"), options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', ordering=F))%>% formatStyle(
        columns = c(3,4),
        #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
        valueColumns = 0,
        target = 'cell',
        backgroundColor = 'rgba(112, 165, 255, 1)')
    })
    
    ###NORMALISE PELVIS ORIENTATION DATA AND PLOT #############
    
    
    
    # PelvisRotNorm = NULL
    # for (i in 1:length(PelvisRot)){
    #   xvalues = 1:length(PelvisRot[[i]])
    #   yvalues = PelvisRot[[i]]
    #   output_x_vals <- seq(1,length(xvalues),length.out=101)
    #   #
    #   # compute the interpolated values; this would be done for each input time series
    #   #
    #   #interp_output[[,i]]<- 
    #   
    #   PelvisRotNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    # }
    # 
    # PelvisRotNormCombined = do.call(cbind, PelvisRotNorm)
    # PelvisRotNormMean = rowMeans(PelvisRotNormCombined)
    # PelvisRotNormSD = apply(PelvisRotNormCombined[,-1], 1, sd)
    # PelvisRotNormPEB = PelvisRotNormMean + PelvisRotNormSD
    # PelvisRotNormNEB = PelvisRotNormMean - PelvisRotNormSD
    # 
    # 
    # 
    # 
    # PelvisRotNormCombined2 = as.data.frame(PelvisRotNormCombined)
    # PelvisRotNormCombined2$perc = c(0:100)
    # PelvisRotNormCombined2 <- melt(PelvisRotNormCombined2, id.vars="perc")
    # 
    # PelvisRotNormMeanSD = cbind.data.frame(PelvisRotNormMean,PelvisRotNormPEB,PelvisRotNormNEB)
    
    # 
    # ggplot(PelvisRotNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none")
    # 
    # 
    # ggplot(PelvisRotNormMeanSD) + geom_line(aes(c(0:100),PelvisRotNormMean))+ geom_line(aes(c(0:100),PelvisRotNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),PelvisRotNormNEB),linetype = "dashed")
    
    
    
    
    ####################################
    ###NORMALISE PELVIS RIGHT ORIENTATION DATA AND PLOT #############
    
    
    PelvisRotRightNorm = NULL
    for (i in 1:length(PelvisRotRight)){
      xvalues = 1:length(PelvisRotRight[[i]])
      yvalues = PelvisRotRight[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      PelvisRotRightNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    PelvisRotRightNormCombined = do.call(cbind, PelvisRotRightNorm)
    PelvisRotRightNormMean = rowMeans(PelvisRotRightNormCombined)
    PelvisRotRightNormSD = apply(PelvisRotRightNormCombined[,-1], 1, sd)
    PelvisRotRightNormPEB = PelvisRotRightNormMean + PelvisRotRightNormSD
    PelvisRotRightNormNEB = PelvisRotRightNormMean - PelvisRotRightNormSD
    
    if (PelvisRotRightNormMean[1] < 0){
      sign = 1
    }else{
      sign = -1
    }
    
    
    PelvisRotRightNormMean <- PelvisRotRightNormMean*sign
    PelvisRotRightNormSD <-  PelvisRotRightNormSD*sign
    PelvisRotRightNormPEB <-  PelvisRotRightNormPEB*sign
    PelvisRotRightNormNEB <-  PelvisRotRightNormNEB*sign
    
    
    PelvisRotRightNormCombined2 = (as.data.frame(PelvisRotRightNormCombined))*sign
    PelvisRotRightNormCombined2$perc = c(0:100)
    PelvisRotRightNormCombined2 <- melt(PelvisRotRightNormCombined2, id.vars="perc")
    
    PelvisRotRightNormMeanMax <- round(max(PelvisRotRightNormMean),2)
    PelvisRotRightNormMeanMaxPC <- which.max(PelvisRotRightNormMean)-1
    
    
    # ggplot(PelvisRotRightNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none")
    # 
    # 
    # ggplot(PelvisRotRightNormMeanSD) + geom_line(aes(c(0:100),PelvisRotRightNormMean))+ geom_line(aes(c(0:100),PelvisRotRightNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),PelvisRotRightNormNEB),linetype = "dashed")
    
    output$PelvisRotRight<- renderPlotly({
      
      fig <- plot_ly(PelvisRotRightNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Rotation Angle (deg)"))
      
      fig
      
    })
    
    
    ###NORMALISE PELVIS LEFT ORIENTATION DATA AND PLOT #############
    
    
    
    PelvisRotLeftNorm = NULL
    for (i in 1:length(PelvisRotLeft)){
      xvalues = 1:length(PelvisRotLeft[[i]])
      yvalues = PelvisRotLeft[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      PelvisRotLeftNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    PelvisRotLeftNormCombined = do.call(cbind, PelvisRotLeftNorm)
    PelvisRotLeftNormMean = rowMeans(PelvisRotLeftNormCombined)
    PelvisRotLeftNormSD = apply(PelvisRotLeftNormCombined[,-1], 1, sd)
    PelvisRotLeftNormPEB = PelvisRotLeftNormMean + PelvisRotLeftNormSD
    PelvisRotLeftNormNEB = PelvisRotLeftNormMean - PelvisRotLeftNormSD
    
    if (PelvisRotLeftNormMean[1] < 0){
      sign = 1
    }else{
      sign = -1
    }
    
    PelvisRotLeftNormMean <- PelvisRotLeftNormMean*sign
    PelvisRotLeftNormSD <-  PelvisRotLeftNormSD*sign
    PelvisRotLeftNormPEB <-  PelvisRotLeftNormPEB*sign
    PelvisRotLeftNormNEB <-  PelvisRotLeftNormNEB*sign
    
    PelvisRotLeftNormCombined2 = (as.data.frame(PelvisRotLeftNormCombined))*sign
    PelvisRotLeftNormCombined2$perc = c(0:100)
    PelvisRotLeftNormCombined2 <- melt(PelvisRotLeftNormCombined2, id.vars="perc")
    
    PelvisRotLeftNormMeanMax <- round(max(PelvisRotLeftNormMean),2)
    PelvisRotLeftNormMeanMaxPC <- which.max(PelvisRotLeftNormMean)-1 
    
    # ggplot(PelvisRotLeftNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none")+
    #   scale_y_reverse()
    # 
    # 
    # ggplot(PelvisRotLeftNormMeanSD) + geom_line(aes(c(0:100),PelvisRotLeftNormMean))+ geom_line(aes(c(0:100),PelvisRotLeftNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),PelvisRotLeftNormNEB),linetype = "dashed") +
    #   scale_y_reverse()
    
    
    
    
    
    
    output$PelvisRotLeft<- renderPlotly({
      
      fig <- plot_ly(PelvisRotLeftNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Rotation Angle (deg)"))
      
      fig
      
    })
    
    output$PelvisRotCombined<- renderPlotly({
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = PelvisRotRightNormMean, type = 'scatter', mode = 'lines',
                  line = list(color = 'rgba(250, 0, 0, 1)'),
                  showlegend = TRUE, name = 'Right Stroke Pelvis Rotation')%>% 
        add_trace(y = PelvisRotRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisRotRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisRotLeftNormMean, type = 'scatter', mode = 'lines', line = list(color = 'green'),
                  showlegend = TRUE, name = 'Left Stroke Pelvis Rotation')%>% 
        add_trace(y = PelvisRotLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisRotLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Rotation Angle (deg)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        legend = list(x = 0.5, y = 0.2)) %>%
        config(displayModeBar = F)
      
      
      fig
      
      
      
      
    })
    
    ###NORMALISE THORAX ORIENTATION DATA AND PLOT #############
    
    # ThoraxRotNorm = NULL
    # for (i in 1:length(ThoraxRot)){
    #   xvalues = 1:length(ThoraxRot[[i]])
    #   yvalues = ThoraxRot[[i]]
    #   output_x_vals <- seq(1,length(xvalues),length.out=101)
    #   #
    #   # compute the interpolated values; this would be done for each input time series
    #   #
    #   #interp_output[[,i]]<- 
    #   
    #   ThoraxRotNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    # }
    # 
    # ThoraxRotNormCombined = do.call(cbind, ThoraxRotNorm)
    # ThoraxRotNormMean = rowMeans(ThoraxRotNormCombined)
    # ThoraxRotNormSD = apply(ThoraxRotNormCombined[,-1], 1, sd)
    # ThoraxRotNormPEB = ThoraxRotNormMean + ThoraxRotNormSD
    # ThoraxRotNormNEB = ThoraxRotNormMean - ThoraxRotNormSD
    # 
    # ThoraxRotNormCombined2 = as.data.frame(ThoraxRotNormCombined)
    # ThoraxRotNormCombined2$perc = c(0:100)
    # ThoraxRotNormCombined2 <- melt(ThoraxRotNormCombined2, id.vars="perc")
    # ThoraxRotNormMeanSD = cbind.data.frame(ThoraxRotNormMean,ThoraxRotNormPEB,ThoraxRotNormNEB)
    
    
    # ggplot(ThoraxRotNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none")
    # 
    # 
    # ggplot(ThoraxRotNormMeanSD) + geom_line(aes(c(0:100),ThoraxRotNormMean))+ geom_line(aes(c(0:100),ThoraxRotNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),ThoraxRotNormNEB),linetype = "dashed")
    
    
    ###NORMALISE THORAX RIGHT ORIENTATION DATA AND PLOT #############
    
    ThoraxRotRightNorm = NULL
    for (i in 1:length(ThoraxRotRight)){
      xvalues = 1:length(ThoraxRotRight[[i]])
      yvalues = ThoraxRotRight[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      ThoraxRotRightNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    ThoraxRotRightNormCombined = do.call(cbind, ThoraxRotRightNorm)
    ThoraxRotRightNormMean = rowMeans(ThoraxRotRightNormCombined)
    ThoraxRotRightNormSD = apply(ThoraxRotRightNormCombined[,-1], 1, sd)
    ThoraxRotRightNormPEB = ThoraxRotRightNormMean + ThoraxRotRightNormSD
    ThoraxRotRightNormNEB = ThoraxRotRightNormMean - ThoraxRotRightNormSD
    
    if (ThoraxRotRightNormMean[1] < 0){
      sign = 1
    }else{
      sign = -1
    }
    
    ThoraxRotRightNormMean <- ThoraxRotRightNormMean*sign
    ThoraxRotRightNormSD <-  ThoraxRotRightNormSD*sign
    ThoraxRotRightNormPEB <-  ThoraxRotRightNormPEB*sign
    ThoraxRotRightNormNEB <-  ThoraxRotRightNormNEB*sign
    
    ThoraxRotRightNormCombined2 = (as.data.frame(ThoraxRotRightNormCombined))*sign
    ThoraxRotRightNormCombined2$perc = c(0:100)
    ThoraxRotRightNormCombined2 <- melt(ThoraxRotRightNormCombined2, id.vars="perc")
    
    ThoraxRotRightNormMeanMax <- round(max(ThoraxRotRightNormMean),2)
    ThoraxRotRightNormMeanMaxPC <- which.max(ThoraxRotRightNormMean)-1 
    
    # ggplot(ThoraxRotRightNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none")
    # 
    # 
    # ggplot(ThoraxRotRightNormMeanSD) + geom_line(aes(c(0:100),ThoraxRotRightNormMean))+ geom_line(aes(c(0:100),ThoraxRotRightNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),ThoraxRotRightNormNEB),linetype = "dashed")
    
    
    output$ThoraxRotRight<- renderPlotly({
      
      fig <- plot_ly(ThoraxRotRightNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Rotation Angle (deg)"))
      
      
      fig
      
    })
    
    ###NORMALISE THORAX LEFT ORIENTATION DATA AND PLOT #############
    
    ThoraxRotLeftNorm = NULL
    for (i in 1:length(ThoraxRotLeft)){
      xvalues = 1:length(ThoraxRotLeft[[i]])
      yvalues = ThoraxRotLeft[[i]]
      output_x_vals <- seq(1,length(xvalues),length.out=101)
      #
      # compute the interpolated values; this would be done for each input time series
      #
      #interp_output[[,i]]<- 
      
      ThoraxRotLeftNorm[[i]] <- approx(x=xvalues, y=yvalues, xout=output_x_vals)$y
    }
    
    ThoraxRotLeftNormCombined = do.call(cbind, ThoraxRotLeftNorm)
    ThoraxRotLeftNormMean = rowMeans(ThoraxRotLeftNormCombined)
    ThoraxRotLeftNormSD = apply(ThoraxRotLeftNormCombined[,-1], 1, sd)
    ThoraxRotLeftNormPEB = ThoraxRotLeftNormMean + ThoraxRotLeftNormSD
    ThoraxRotLeftNormNEB = ThoraxRotLeftNormMean - ThoraxRotLeftNormSD
    
    if (ThoraxRotLeftNormMean[1] < 0){
      sign = 1
    }else{
      sign = -1
    }
    
    ThoraxRotLeftNormMean <- ThoraxRotLeftNormMean*sign
    ThoraxRotLeftNormSD <-  ThoraxRotLeftNormSD*sign
    ThoraxRotLeftNormPEB <-  ThoraxRotLeftNormPEB*sign
    ThoraxRotLeftNormNEB <-  ThoraxRotLeftNormNEB*sign
    
    ThoraxRotLeftNormCombined2 = (as.data.frame(ThoraxRotLeftNormCombined))*sign
    ThoraxRotLeftNormCombined2$perc = c(0:100)
    ThoraxRotLeftNormCombined2 <- melt(ThoraxRotLeftNormCombined2, id.vars="perc")
    
    ThoraxRotLeftNormMeanMax <- round(max(ThoraxRotLeftNormMean),2)
    ThoraxRotLeftNormMeanMaxPC <- which.max(ThoraxRotLeftNormMean) -1   
    
    
    # ggplot(ThoraxRotLeftNormCombined2) + geom_line(aes(perc,value, colour = variable))+ theme(legend.position = "none")+
    #   scale_y_reverse()
    # 
    # 
    # ggplot(ThoraxRotLeftNormMeanSD) + geom_line(aes(c(0:100),ThoraxRotLeftNormMean))+ geom_line(aes(c(0:100),ThoraxRotLeftNormPEB), linetype = "dashed")+ geom_line(aes(c(0:100),ThoraxRotLeftNormNEB),linetype = "dashed")+
    #   scale_y_reverse()
    
    output$ThoraxRotLeft<- renderPlotly({
      
      fig <- plot_ly(ThoraxRotLeftNormCombined2, x = ~perc, y = ~value, showlegend = FALSE) %>% add_lines(color = ~ordered(variable))  %>% 
        layout(xaxis = list(title = "% of Stroke"), yaxis = list(title = "Rotation Angle (deg)"))
      
      fig
      
    })
    
    
    
    output$ThoraxRotCombined<- renderPlotly({
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = ThoraxRotRightNormMean, type = 'scatter', mode = 'lines',
                  line = list(color = 'rgba(250, 0, 0, 1)'),
                  showlegend = TRUE, name = 'Right Stroke Thorax Rotation')%>% 
        add_trace(y = ThoraxRotRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxRotRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 10, 10, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxRotLeftNormMean, type = 'scatter', mode = 'lines', line = list(color = 'green'),
                  showlegend = TRUE, name = 'Left Stroke Thorax Rotation')%>% 
        add_trace(y = ThoraxRotLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxRotLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Rotation Angle (deg)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        legend = list(x = 0.5, y = 0.2)) %>%
        config(displayModeBar = F)
      
      
      fig
      
      
      
      
    })
    
    
    
    
    
    output$RotSequenceRight<- renderPlotly({
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = BoatAccRightNormMean*10, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  line = list(color = 'black',dash = 'dot'),
                  showlegend = TRUE, name = 'Boat Acceleration')%>% 
        add_trace(y = BoatAccRightNormPEB*10, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(0,0,0,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = BoatAccRightNormNEB*10, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(0,0,0,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityRightNormMean, type = 'scatter', mode = 'lines',yaxis = "y2",
                  line = list(color = 'rgba(122, 39, 160, 1)',dash = 'dot'),
                  showlegend = TRUE, name = 'Minimax Velocity')%>% 
        add_trace(y = MinimaxdfVelocityRightNormPEB, type = 'scatter', mode = 'lines',yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(122, 39, 160, 0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityRightNormNEB, type = 'scatter', mode = 'lines',yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(122, 39, 160,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxRotRightNormMean, type = 'scatter', mode = 'lines', line = list(color = 'rgba(21, 0, 250, 1)'),
                  showlegend = TRUE, name = 'Thorax Rotation')%>% 
        add_trace(y = ThoraxRotRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(21, 0, 250, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxRotRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(21, 0, 250,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisRotRightNormMean, type = 'scatter', mode = 'lines',line = list(color = 'rgba(255, 117, 31,1)'),
                  showlegend = TRUE, name = 'Pelvis Rotation')%>% 
        add_trace(y = PelvisRotRightNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 117, 31,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisRotRightNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 117, 31,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Rotation Angle (deg)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE,
                                     range = c(-40,40)),
                        yaxis2 = list(title = "Velocity (m/s)",
                                      overlaying = "y",
                                      side = "right",
                                      showgrid = FALSE,
                                      showline = FALSE,
                                      showticklabels = TRUE,
                                      ticks = 'outside',
                                      zeroline = TRUE,
                                      range = c(3,4),
                                      color = 'rgba(122, 39, 160, 1)'),
                        margin = list(l = 0, r = 50, b = 0, t = 0),
                        legend = list(orientation = 'h', x =0, y = -0.2)) %>%
        config(displayModeBar = F)
      
      
      fig
      
      
      
      
    })
    
    output$RotSequenceLeft<- renderPlotly({
      
      fig <- plot_ly(x = c(0:100)) %>%
        add_trace(y = BoatAccLeftNormMean*10, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  line = list(color = 'black',dash = 'dot'),
                  showlegend = TRUE, name = 'Boat Acceleration')%>% 
        add_trace(y = BoatAccLeftNormPEB*10, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(0,0,0,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = BoatAccLeftNormNEB*10, type = 'scatter', mode = 'lines',#yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(0,0,0,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityLeftNormMean, type = 'scatter', mode = 'lines',yaxis = "y2",
                  line = list(color = 'rgba(122, 39, 160, 1)',dash = 'dot'),
                  showlegend = TRUE, name = 'Minimax Velocity')%>% 
        add_trace(y = MinimaxdfVelocityLeftNormPEB, type = 'scatter', mode = 'lines',yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(122, 39, 160, 0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = MinimaxdfVelocityLeftNormNEB, type = 'scatter', mode = 'lines',yaxis = "y2",
                  fill = 'tonexty', fillcolor='rgba(122, 39, 160,0.1)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxRotLeftNormMean, type = 'scatter', mode = 'lines', line = list(color = 'rgba(21, 0, 250, 1)'),
                  showlegend = TRUE, name = 'Thorax Rotation')%>% 
        add_trace(y = ThoraxRotLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(21, 0, 250, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = ThoraxRotLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(21, 0, 250, 0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisRotLeftNormMean, type = 'scatter', mode = 'lines',line = list(color = 'rgba(255, 117, 31,1)'),
                  showlegend = TRUE, name = 'Pelvis Rotation')%>% 
        add_trace(y = PelvisRotLeftNormPEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 117, 31,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        add_trace(y = PelvisRotLeftNormNEB, type = 'scatter', mode = 'lines',
                  fill = 'tonexty', fillcolor='rgba(255, 117, 31,0.2)', line = list(color = 'rgba(0,0,0,0)'),
                  showlegend = FALSE, name = 'Lowerlimit')%>% 
        layout(         xaxis = list(title = "% of Stroke",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE),
                        yaxis = list(title = "Rotation Angle (deg)",
                                     showgrid = TRUE,
                                     showline = FALSE,
                                     showticklabels = TRUE,
                                     ticks = 'outside',
                                     zeroline = TRUE,
                                     range = c(-40,40)),
                        yaxis2 = list(title = "Velocity (m/s)",
                                      overlaying = "y",
                                      side = "right",
                                      showgrid = FALSE,
                                      showline = FALSE,
                                      showticklabels = TRUE,
                                      ticks = 'outside',
                                      zeroline = TRUE,
                                      range = c(3,4),
                                      color = 'rgba(122, 39, 160, 1)'),
                        margin = list(l = 0, r = 50, b = 0, t = 0),
                        legend = list(orientation = 'h', x =0, y = -0.2)) %>%
        config(displayModeBar = F)
      
      
      fig
    })
    
    SequenceAngledataRight <- data.frame(MinimaxdfVelocityRightNormMeanMax, MinimaxdfVelocityRightNormMeanMaxPC, PelvisRotRightNormMeanMax,  PelvisRotRightNormMeanMaxPC, ThoraxRotRightNormMeanMax, ThoraxRotRightNormMeanMaxPC)
    colnames(SequenceAngledataRight) <- c("Minimax Velocity Max (m/s)", "%", "Pelvis Rotation Angle Max (deg)",  "%", "Thorax Rotation Angle Max (deg)", "%")
    SequenceAngledataLeft <- data.frame(MinimaxdfVelocityLeftNormMeanMax, MinimaxdfVelocityLeftNormMeanMaxPC, PelvisRotLeftNormMeanMax,  PelvisRotLeftNormMeanMaxPC, ThoraxRotLeftNormMeanMax, ThoraxRotLeftNormMeanMaxPC)
    colnames(SequenceAngledataLeft) <- c("Minimax Velocity Max (m/s)", "%", "Pelvis Rotation Angle Max (deg)",  "%", "Thorax Rotation Angle Max (deg)", "%")
    
    SequenceAngledata <- rbind(SequenceAngledataRight,SequenceAngledataLeft)
    
    
    
    output$table_AngleSequence <- renderDataTable({ 
      
      
      datatable(SequenceAngledata,  rownames = c("Right", "Left"), options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', ordering=F))%>% formatStyle(
        columns = c(3,4),
        #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
        valueColumns = 0,
        target = 'cell',
        backgroundColor = 'rgba(112, 165, 255, 1)')
    })
    
    
  })
}
