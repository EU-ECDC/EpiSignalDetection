


##-----------------------------------------------------------------------------
##
## This is the user-interface definition of a Shiny web application.
##
##-----------------------------------------------------------------------------

shinyUI(

  fluidPage(

    #-------------------------------------------------------
    #---- Theme of the application and CSS style
    #-------------------------------------------------------
    includeCSS("style.css"),


    #-------------------------------------------------------
    #---- Application title and ECDC logo
    #-------------------------------------------------------
    img(src = "ecdc.png", height = 72, align = "right"),
    titlePanel("Signal detection tool for monitoring infectious diseases surveillance data",
               windowTitle = "Signal detection query tool"),
    hr(),


    fluidRow(

      #------------------------------------------------------------
      #---- Sidebar with selects and sliders input >> Width = 3 /12
      #------------------------------------------------------------
      column( width = 3,

              #-------------------------------------------
              #---- Parameter box: Dataset selection
              #-------------------------------------------
              wellPanel(
                span("Dataset selection:", style = "font-size:120%;color:#E95420"),
                hr(),

                fileInput('file',
                          'Use default dataset
                          or import an Atlas export csv file (<30MB)',
                          accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            '.csv'
                          )
                ),

                fluidRow(
                  selectInput(inputId = "disease",
                              label = "Health topic",
                              choices = unique(EpiSignalDetection::SignalData$HealthTopic),
                              selected = NULL,
                              multiple = FALSE),

                  selectInput(inputId = "country",
                              label = "Region name",
                              choices = c("EU/EEA - complete series",
                                          unique(EpiSignalDetection::SignalData$RegionName)),
                              multiple = FALSE,
                              selectize = TRUE),

                  conditionalPanel(condition = "input.country == 'EU-EEA - complete series'",
                                   em(p("Please note that countries presenting with at least
                                        one gap in the time series are excluded from the
                                        'EU-EEA - complete series' analysis.")),
                                   br()
                  ),

                  conditionalPanel(condition = "input.country == 'EU/EEA' | input.country == 'EU' ",
                                   em(p("Please note that all countries are included in this analysis,
                                        even those with incomplete time series.")),
                                   br()
                  ),

                  selectInput(inputId = "indicator",
                              label = "Indicator",
                              choices = unique(EpiSignalDetection::SignalData$Indicator),
                              selected = "Reported cases",
                              multiple = FALSE),

                  selectInput(inputId = "stratification",
                              label = "Population",
                              choices = unique(EpiSignalDetection::SignalData$Population),
                              multiple = FALSE,
                              selectize = TRUE)

                )),

              #-------------------------------------------------------
              #---- Parameter box : Time variable
              #-------------------------------------------------------
              wellPanel(
                span("Time settings:", style = "font-size:120%;color:#E95420"),
                hr(),
                fluidRow(
                  column(width = 6,
                         selectInput(inputId = "unit",
                                     label = "Time unit",
                                     choices = unique(EpiSignalDetection::SignalData$TimeUnit),
                                     selected = "Month",
                                     multiple = FALSE)
                  ),

                  conditionalPanel(condition = "input.unit == 'Year'",
                                   br(),
                                   em(p("Please note the Signal Detection tool does not handle yearly data."))
                  )
                ),
                fluidRow(
                  column(width = 12,

                         #---Default starting/ending date is
                         #---earliest/latest date available in the dataset
                         dateRangeInput(inputId ="daterange",
                                        label = "Study period*",
                                        start  = (max(EpiSignalDetection::SignalData$TimeDate, na.rm = TRUE) - (365.25*5) ),
                                        end    = max(EpiSignalDetection::SignalData$TimeDate, na.rm = TRUE),
                                        min    = min(EpiSignalDetection::SignalData$TimeDate, na.rm = TRUE),
                                        max    = max(EpiSignalDetection::SignalData$TimeDate, na.rm = TRUE),
                                        format = "dd/mm/yyyy",
                                        separator = " - "),

                         sliderInput(inputId ="testingperiod",
                                     label= "Signal detection period: number of months/weeks to test for signal detection",
                                     min = 1,
                                     max = 12,
                                     value = 3),

                         conditionalPanel(condition = "input.unit != 'Year'",
                                          em(htmlOutput("testingPeriod")))

                  )
                )
              ),




              #-------------------------------------------------------
              #---- Parameter box : Signal detection algorithm
              #-------------------------------------------------------
              wellPanel(
                span("Signal detection algorithm:", style = "font-size:120%;color:#E95420"),
                hr(),
                fluidRow(
                  column(width = 12,
                         selectInput(inputId = "algo",
                                     label = "Statistical algorithm",
                                     choices = c("FarringtonFlexible","GLRNB"),
                                     multiple = FALSE)
                  )
                )
              ),


              #-------------------------------------------------------
              #---- 'Run' button
              #-------------------------------------------------------
              conditionalPanel(condition = "input.unit != 'Year'",
                               actionButton("do", "Run the analysis")
              )

      ),



      #-------------------------------------------------------
      #---- Outputs panel >> Width = 9 /12
      #-------------------------------------------------------
      column( width = 9,

              #-- Time series plot
              plotOutput("plot"),
              br(),

              #-- List of reporting countries
              htmlOutput("listGapCountries"),
              br(),

              #-- Result table
              dataTableOutput("dataset"),

              #-- Download buttons to display
              column( width = 7,
                      conditionalPanel("input.do",
                                       downloadButton('downloadResTable', 'Download result table'),
                                       downloadButton('downloadGraph', 'Download time series plot'),
                                       downloadButton('downloadData', 'Download dataset'))
                      ),

              column( width = 2, offset = 0, style='padding-left:39px;',
                      conditionalPanel("input.do",
                                       downloadButton("signalDetectionReport",
                                                      "Generate and download the report of signals at country level (HTML format)",
                                                      class = "butt1"),
                                       tags$head(tags$style(".butt1{background-color:#ED764C;}
                                                    .butt1{color: black;}
                                                    .butt1{font-weight: bold;}")))
                      ),
              br(),

              #-------------------------------------------------------
              #---- Disclaimer >> Width = 12 /12
              #-------------------------------------------------------

              br(),
              column(width = 12,
                     conditionalPanel(
                       "input.do",
                       hr(),
                       tags$cite(
                         tags$b("Disclaimer:"),
                         "ECDC accepts no responsibility or liability whatsoever
                         (including but not limited to any direct or consequential loss
                         or damage it might occur to you and/or any other third party)
                         arising out of or in connection with the installation and/or usage of this software.
                         Copyright European Centre for Disease Prevention and Control, 2018."
                       )
                     )),
              br()


      )
    ),




    #-------------------------------------------------------
    #---- Footer >> Width = 12 /12
    #-------------------------------------------------------

    fluidRow(
      column( width = 3,
              tags$i( paste("Generated on:", format(Sys.Date(), "%a %B %d, %Y")))
      ),
      column( width = 9,
              tags$i( tags$b("*Please check data quality reports for information on date completeness and precision."))
      )
    )





  )
)




