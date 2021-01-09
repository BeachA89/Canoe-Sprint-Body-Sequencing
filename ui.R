

ui <- dashboardPage(
  dashboardHeader (title = "Canoe Sprint Segmental Sequencing", titleWidth = 450),
  dashboardSidebar(
    # fileInput("file1", "Choose files", multiple = TRUE,
    #           accept = c("text/csv",
    #                      "text/comma-separated-values,text/plain",
    #                      ".csv")),
    # actionButton("goButton", "Go!"),
    
    # selectInput("distance", "Distance:",
    #             choices = c("500" = "Labelled_data_500",
    #                         "1000" = "Labelled_data_1000")),
    # selectInput("Report_Type", "Report Type:",
    #             c("Single Race" = "Single Race",
    #               "Two Races" = "Two Races",
    #               "vs Top 10" = "vs Top 10")),    
    fileInput("BoatdfAl", "Choose Boat aligned file",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fileInput("PelvisdfAl", "Choose Pelvis aligned file",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fileInput("ThoraxdfAl", "Choose Thorax aligned file",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fileInput("MinimaxdfAl", "Choose Minimax aligned file",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    actionButton("goButton", "Go!")
  ),
  # 
  # 
  # selectInput("Distance1", "Distance:",
  #          choices = c(">70" = ">70",
  #                      "68-70" = "68-70",
  #                      "66-68" = "66-68",
  #                      "64-66" = "64-66")),
  # selectInput("Distance2", "Distance:",
  #             choices = c(">70" = ">70",
  #                         "68-70" = "68-70",
  #                         "66-68" = "66-68",
  #                         "64-66" = "64-66"))
  
  
  
  dashboardBody(
    
    fluidRow(
      tabBox(width = 12,
             tabPanel("Summary",
                      
                      fluidRow(
                        h3(textOutput("Summaryhead"))
                      ),
                      fluidRow(
                        column(6,box(title = "All Boat Acceleration", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("BoatAccCombined"))),
                        column(6,box(title = "All Boat Velocity", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("MinimaxVelCombined")))
                      ),
                      
                      
                      
                      fluidRow(
                        
                        column(6,box(title = "All Pelvis Rot Velocity", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("PelvisGyroCombined"))),
                        column(6,box(title = "All Thorax Rot Velocity", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("ThoraxGyroCombined")))
                      ),
                      
                      fluidRow(
                        column(6,box(title = "Right Stroke Rotational Velocity Sequence", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("SequenceRight"))),
                        column(6,box(title = "Left Stroke Rotational Velocity Sequence", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("SequenceLeft")))
                      ),
                      
                      fluidRow(
                        column(3),
                        column(6,box(title = "Velocity Sequence Summary", status = "warning", solidHeader = TRUE, width = 12,
                          collapsible = FALSE,
                          dataTableOutput("table_VelSequence")))
                      ),

                      
                      
                      fluidRow(
                        
                        column(6,box(title = "All Pelvis Rotation Angle", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("PelvisRotCombined"))),
                        column(6,box(title = "All Thorax Rotation Angle", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("ThoraxRotCombined")))
                      ),
                      
                      
                      fluidRow(
                        column(6,box(title = "Right Stroke Rotation Angle Sequence", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("RotSequenceRight"))),
                        column(6,box(title = "Left Stroke Rotation Angle Sequence", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("RotSequenceLeft")))
                      ),
                      fluidRow(
                        column(3),
                        column(6,box(title = "Rotation Angle Sequence Summary", status = "warning", solidHeader = TRUE, width = 12,
                            collapsible = FALSE,
                            dataTableOutput("table_AngleSequence")))
                      )
                      
             ),
             tabPanel("Complete Data",
                      fluidRow(
                        column(6,box(title = "Right Stroke Boat Acceleration", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("BoatAccRight"))),
                        column(6,box(title = "Left Stroke Boat Acceleration", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("BoatAccLeft")))
                      ),
                      fluidRow(
                        column(6,box(title = "Right Stroke Boat Velocity", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("MinimaxVelRight"))),
                        column(6,box(title = "Left Stroke Boat Velocity", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("MinimaxVelLeft")))
                      ),
                      fluidRow(
                        column(6,box(title = "Right Stroke Pelvis Rot Velocity", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("PelvisGyroRight"))),
                        column(6,box(title = "Left Stroke Pelvis Rot Velocity", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("PelvisGyroLeft"))),
                      ),
                      fluidRow(
                        column(6,box(title = "Right Stroke Thorax Rot Velocity", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("ThoraxGyroRight"))),
                        column(6,box(title = "Left Stroke Thorax Rot Velocity", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("ThoraxGyroLeft"))),
                      ),
                      fluidRow(
                        column(6,box(title = "Right Stroke Pelvis Rotation Angle", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("PelvisRotRight"))),
                        column(6,box(title = "Left Stroke Pelvis Rotation Angle", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("PelvisRotLeft"))),
                      ),
                      fluidRow(
                        column(6,box(title = "Right Stroke Thorax Rotation Angle", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("ThoraxRotRight"))),
                        column(6,box(title = "Left Stroke Thorax Rotation Angle", status = "primary", solidHeader = TRUE,width = 12,
                                     collapsible = TRUE,plotlyOutput("ThoraxRotLeft"))),
                      )
                      
             )
      )
    )
    
    
    
    # fluidRow(
    #   box(title = "Right Foot Down to Left Foot Toe Up", status = "primary", solidHeader = TRUE, width = 12,
    #       collapsible = TRUE,
    #       DT::dataTableOutput("datatable3"))
    # )
    #,
    
    #fluidRow(
    #    box(title = "Distance Comparison", status = "primary", solidHeader = TRUE, width = 12,
    #        collapsible = TRUE,
    #        DT::dataTableOutput("datatable_comparison"))
    #)
    
    #    fluidRow(
    #      box(title = "Time vs Top 10 average", status = "primary", solidHeader = TRUE,width = 12,
    #          collapsible = TRUE,plotOutput("ggplot"))
    #    )
    
    
    
  )
  
)










# Create Shiny app ----
# shinyApp(ui, server)