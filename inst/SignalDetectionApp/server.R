

#-----------------------------------------------------------------------------
#
# This is the server logic of a Shiny web application.
#
#-----------------------------------------------------------------------------

#----
# Setting option to allow importing a file >5MB (but still <30MB)
#----
options(shiny.maxRequestSize = 30 * 1024 ^ 2 )


#----
# Shiny server
#----

shinyServer(function(input, output, session) {


  #--------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------
  #----       Parameters update section
  #--------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------


  #-------------------------------------------------------
  #---- DATASET selection: default or FILE
  #-------------------------------------------------------

  datasetInput <- reactive({
    if (is.null(input$file))
      dataset <- EpiSignalDetection::SignalData

    inFile <- input$file
    if (!is.null(inFile)) {
      dataset <- EpiSignalDetection::importAtlasExport(inFile$datapath)
      dataset <- EpiSignalDetection::cleanAtlasExport(dataset)
    }

    dataset

  })


  #-------------------------------------------------------
  #---- DATASET Update according to the disease
  #-------------------------------------------------------

  datasetUpdate <- reactive({

    dataset <- datasetInput()

    if (input$disease == "")
      return(dataset)

    dataset <- dplyr::filter(dataset, HealthTopic == input$disease)
    dataset

  })


  #-------------------------------------------------------
  #---- According to the FILE
  #---- Updating 'disease' 'country' 'stratification' 'unit' parameter
  #-------------------------------------------------------

  observeEvent(input$file, {

    dataset <- datasetInput()

    updateSelectizeInput(
      session,
      inputId = "disease",
      choices = unique(dataset$HealthTopic)
    )

    updateSelectizeInput(
      session,
      inputId = "country",
      selected = "EU/EEA - complete series",
      choices = c("EU/EEA - complete series", unique(dataset$RegionName))
    )

    updateSelectizeInput(
      session,
      inputId = "indicator",
      selected = "Reported cases",
      choices = unique(dataset$Indicator)
    )

    updateSelectizeInput(
      session,
      inputId = "stratification",
      choices = unique(dataset$Population)
    )

    updateSelectizeInput(
      session,
      inputId = "unit",
      selected = "Month",
      choices = unique(dataset$TimeUnit)
    )

    updateDateRangeInput(
      session,
      inputId = "daterange",
      start  = max(dataset$TimeDate, na.rm = TRUE) - 365.25*5 ,
      end    = max(dataset$TimeDate, na.rm = TRUE),
      min    = min(dataset$TimeDate, na.rm = TRUE),
      max    = max(dataset$TimeDate, na.rm = TRUE)
    )

    updateSliderInput(
      session,
      inputId = "testingperiod",
      label = ifelse(input$unit == "Month",
                     "Signal detection period: number of months to test for signal detection",
                     "Signal detection period: number of weeks to test for signal detection"),
      min = 1,
      max = ifelse(input$unit == "Month", 12, 53),
      value = ifelse(input$unit == "Month", 3, 13)
    )


  })


  #-------------------------------------------------------
  #---- According to the DISEASE
  #---- Updating 'country' 'stratifiaction' 'unit' 'daterange' parameters
  #-------------------------------------------------------

  observeEvent(input$disease, {

    dataset <- datasetUpdate()

    updateSelectizeInput(
      session,
      inputId = "country",
      choices = c("EU-EEA - complete series", unique(dataset$RegionName))
    )

    updateSelectizeInput(
      session,
      inputId = "indicator",
      selected = "Reported cases",
      choices = unique(dataset$Indicator)
    )

    updateSelectizeInput(
      session,
      inputId = "stratification",
      choices = unique(dataset$Population)
    )

    updateSelectizeInput(
      session,
      inputId = "unit",
      selected = "Month",
      choices = unique(dataset$TimeUnit)
    )

    updateDateRangeInput(
      session,
      inputId = "daterange",
      start  = (max(dataset$TimeDate, na.rm = TRUE) - (365.25*5) ) ,
      end    = max(dataset$TimeDate, na.rm = TRUE),
      min    = min(dataset$TimeDate, na.rm = TRUE),
      max    = max(dataset$TimeDate, na.rm = TRUE)

    )

    updateSliderInput(
      session,
      inputId = "testingperiod",
      label = ifelse(input$unit == "Month",
                     "Signal detection period: number of months to test for signal detection",
                     "Signal detection period: number of weeks to test for signal detection"),
      min = 1,
      max = ifelse(input$unit == "Month", 12, 53),
      value = ifelse(input$unit == "Month", 3, 13)
    )


  })

  #-------------------------------------------------------
  #---- According to the COUNTRY
  #---- Updating 'stratifiaction' 'unit' 'daterange' parameters
  #-------------------------------------------------------

  observeEvent(input$country, {

    if (input$country != "EU-EEA - complete series") {
      dataset <- datasetUpdate()
      dataset <- dplyr::filter(dataset, RegionName == input$country)
    } else {
      dataset <- datasetUpdate()
    }

    updateSelectizeInput(
      session,
      inputId = "indicator",
      selected = "Reported cases",
      choices = unique(dataset$Indicator)
    )

    updateSelectizeInput(
      session,
      inputId = "stratification",
      choices = unique(dataset$Population)
    )

    updateSelectizeInput(
      session,
      inputId = "unit",
      selected = "Month",
      choices = unique(dataset$TimeUnit)
    )

    updateDateRangeInput(
      session,
      inputId ="daterange",
      start  = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE) - (365.25*5) ),
      end    = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE)),
      min    = suppressWarnings(min(dataset$TimeDate, na.rm = TRUE)),
      max    = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE))
    )

    updateSliderInput(
      session,
      inputId = "testingperiod",
      label = ifelse(input$unit == "Month",
                     "Signal detection period: number of months to test for signal detection",
                     "Signal detection period: number of weeks to test for signal detection"),
      min = 1,
      max = ifelse(input$unit == "Month", 12, 53),
      value = ifelse(input$unit == "Month", 3, 13)
    )

  })


  #-------------------------------------------------------
  #---- According to the INDICATOR
  #---- Updating 'population', 'unit' and daterange' parameter
  #-------------------------------------------------------

  observeEvent(input$indicator, {

    if (input$country != "EU-EEA - complete series") {
      dataset <- datasetUpdate()
      dataset <- dplyr::filter(dataset, RegionName == input$country)
      dataset <- dplyr::filter(dataset, Indicator == input$indicator)
    } else {
      dataset <- datasetUpdate()
      dataset <- dplyr::filter(dataset, Indicator == input$indicator)
    }


    updateSelectizeInput(
      session,
      inputId = "stratification",
      choices = unique(dataset$Population)
    )


    updateSelectizeInput(
      session,
      inputId = "unit",
      selected = "Month",
      choices = unique(dataset$TimeUnit)
    )


    updateDateRangeInput(
      session,
      inputId = "daterange",
      start  = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE) - (365.25*5)),
      end    = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE)),
      min    = suppressWarnings(min(dataset$TimeDate, na.rm = TRUE)),
      max    = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE))
    )

    updateSliderInput(
      session,
      inputId = "testingperiod",
      label = ifelse(input$unit == "Month",
                     "Signal detection period: number of months to test for signal detection",
                     "Signal detection period: number of weeks to test for signal detection"),
      min = 1,
      max = ifelse(input$unit == "Month", 12, 53),
      value = ifelse(input$unit == "Month", 3, 13)
    )

  })

  #-------------------------------------------------------
  #---- According to the STRATIFICATION
  #---- Updating 'unit' and daterange' parameter
  #-------------------------------------------------------

  observeEvent(input$stratification, {

    if (input$country != "EU-EEA - complete series") {
      dataset <- datasetUpdate()
      dataset <- dplyr::filter(dataset, RegionName == input$country)
      dataset <- dplyr::filter(dataset, Indicator == input$indicator)
      dataset <- dplyr::filter(dataset, Population == input$stratification)
    } else {
      dataset <- datasetUpdate()
      dataset <- dplyr::filter(dataset, Indicator == input$indicator)
      dataset <- dplyr::filter(dataset, Population == input$stratification)
    }

    updateSelectizeInput(
      session,
      inputId = "unit",
      selected = "Month",
      choices = unique(dataset$TimeUnit)
    )


    updateDateRangeInput(
      session,
      inputId = "daterange",
      start  = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE) - (365.25*5)),
      end    = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE)),
      min    = suppressWarnings(min(dataset$TimeDate, na.rm = TRUE)),
      max    = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE))
    )

    updateSliderInput(
      session,
      inputId = "testingperiod",
      label = ifelse(input$unit == "Month",
                     "Signal detection period: number of months to test for signal detection",
                     "Signal detection period: number of weeks to test for signal detection"),
      min = 1,
      max = ifelse(input$unit == "Month", 12, 53),
      value = ifelse(input$unit == "Month", 3, 13)
    )

  })

  #-------------------------------------------------------
  #---- According to the UNIT
  #---- Updating 'daterange' parameter
  #-------------------------------------------------------

  observeEvent(input$unit, {

    if (input$country != "EU-EEA - complete series") {
      dataset <- datasetUpdate()
      dataset <- dplyr::filter(dataset, RegionName == input$country)
      dataset <- dplyr::filter(dataset, Indicator == input$indicator)
      dataset <- dplyr::filter(dataset, Population == input$stratification)
      dataset <- dplyr::filter(dataset, TimeUnit == input$unit)
    } else {
      dataset <- datasetUpdate()
      dataset <- dplyr::filter(dataset, Indicator == input$indicator)
      dataset <- dplyr::filter(dataset, Population == input$stratification)
      dataset <- dplyr::filter(dataset, TimeUnit == input$unit)
    }

    updateDateRangeInput(
      session,
      inputId = "daterange",
      start  = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE) - (365.25*5)),
      end    = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE)),
      min    = suppressWarnings(min(dataset$TimeDate, na.rm = TRUE)),
      max    = suppressWarnings(max(dataset$TimeDate, na.rm = TRUE))
    )

    updateSliderInput(
      session,
      inputId = "testingperiod",
      label = ifelse(input$unit == "Month",
                     "Signal detection period: number of months to test for signal detection",
                     "Signal detection period: number of weeks to test for signal detection"),
      min = 1,
      max = ifelse(input$unit == "Month", 12, 53),
      value = ifelse(input$unit == "Month", 3, 13)
    )

  })



  #-------------------------------------------------------
  #---- Filtering function
  #-------------------------------------------------------
  filterFinalDataset <- function(){

    #--- Filtering on the selected disease
    dataset <- datasetUpdate()

    #--- Filtering on country, stratification and time unit
    dataset <- filterAtlasExport(dataset, input)

    #--- Warning message if the StudyPeriod is not complete in the dataset
    if (sum(is.na(dataset$RegionName)) != 0) {
      warning("The time series is not complete - Gaps introduced to run signal detection algorithms")
    }

    dataset
  }


  #-------------------------------------------------------
  #---- Identifying countries with gaps in TS function
  #-------------------------------------------------------
  gapsCountries <- eventReactive(input$do, {
    dataset <- filterFinalDataset()
    excluded <- unique(dataset$RegionName[is.na(dataset$NumValue)])
    included <- unique(dataset$RegionName[!(dataset$RegionName %in% c(excluded, "EU", "EU/EEA"))])
    gaps <- list(excluded = excluded, included = included)
    gaps
  })


  #-------------------------------------------------------
  #---- RUN >> Aggregating function
  #-------------------------------------------------------
  aggFinalDataset <- eventReactive(input$do, {
    dataset <- filterFinalDataset()
    dataset <- aggAtlasExport(dataset, input)
    dataset
  })


  #-------------------------------------------------------
  #---- Sts object
  #-------------------------------------------------------
  makeSts <- eventReactive(input$do, {
    dataset <- aggFinalDataset()
    dataset.sts <- stsSD(observedCases = dataset$NumValue,
                         studyPeriod = dataset$StudyPeriod,
                         timeUnit = input$unit,
                         startYM = c(as.numeric(format(as.Date(input$daterange[1], "%Y-%m-%d"), "%Y")),
                                     as.numeric(format(as.Date(input$daterange[1], "%Y-%m-%d"), "%m"))))
    dataset.sts
  })


  #-------------------------------------------------------
  #---- Detection algorithm
  #-------------------------------------------------------
  makeAlgo <- eventReactive(input$do, {
    dataset.sts <- makeSts()
    dataset.algo <- algoSD(dataset.sts,
                           algo = input$algo,
                           timeUnit = input$unit,
                           testingPeriod = input$testingperiod)
    dataset.algo
  })


  #-------------------------------------------------------
  #---- Make plot function
  #-------------------------------------------------------

  makePlot <- function(){
    dataset <- aggFinalDataset()
    plotSD(dataset, isolate(input))
  }

  #-------------------------------------------------------
  #---- Make table function
  #-------------------------------------------------------

  makeTable <- eventReactive(input$do, {
    dataset <- aggFinalDataset()
    dataset.algo <- makeAlgo()

    StudyPeriod <- dataset$StudyPeriod
    TestingPeriod <- tail(StudyPeriod, n = input$testingperiod)
    Time <- dplyr::filter(dataset, dataset$StudyPeriod %in% TestingPeriod)

    #--- Preparation of the result table that will be append through the following loop
    Result <- data.frame( Time = as.Date("2000-01-01") , Place = "" , Observed = "" ,
                          Threshold.ALGO = "", ALGO = "", CasesByCountry = "", stringsAsFactors=FALSE)

    for(l in 1:(input$testingperiod)){
      Result <- rbind(Result,
                      data.frame(Time = as.Date(Time$StudyPeriod[l]),
                                 Place = input$country,
                                 Observed = as.character(dataset.algo@observed[l]),
                                 Threshold.ALGO = as.character(round(dataset.algo@upperbound[l],0)),
                                 ALGO = ifelse(dataset.algo@alarm[l],"Signal","no"),
                                 CasesByCountry = Time$CasesByCountry[l]
                      )
      )
    }

    Result <- Result[-1, ]
    row.names(Result) <- NULL
    names(Result) <- c(paste("Time ( by ", tolower(input$unit), ")", sep = ""),
                       "Place",
                       "Reported cases",
                       "Threshold value",
                       "Signal",
                       "Countries (cases)")
    Result

  })



  #-------------------------------------------------------
  #-------------------------------------------------------
  #---- Outputs
  #-------------------------------------------------------
  #-------------------------------------------------------

  #-------------------------------------------------------
  # Testing period definition
  #-------------------------------------------------------

  output$testingPeriod <- renderText({
    if(input$unit != "" & !is.na(input$daterange[1]) & !is.na(input$daterange[2]) ){
      studyPeriod <- studyPeriod(input)
      paste(" --> Selected signal detection period from
              <font style=\"color:#E95420;\"> ",
            studyPeriod$Time[nrow(studyPeriod)-input$testingperiod+1] ,
            "</font> to <font style=\"color:#E95420;\">",
            tail(studyPeriod$Time, "</font>", n=1))
    }
  })


  #-------------------------------------------------------
  # List of not reporting countries
  #-------------------------------------------------------

  output$listGapCountries <- renderText({
    if(input$do == FALSE) return()
    if(length(gapsCountries()$excluded) == 0) return()
    isolate({
      if(input$country == "EU-EEA - complete series"){
        if (length(gapsCountries()$included) != 0) {
          paste("During the study period from ",
                format(as.Date(input$daterange[1], "%Y-%m-%d"), "%d %B %Y"), " to ",
                format(as.Date(input$daterange[2], "%Y-%m-%d"), "%d %B %Y"),
                ", the following country(ies) presented with at least one gap in the time series:  <br>
                <font style=\"color:#E95420;\">",
                paste(gapsCountries()$excluded, collapse = ", "), ".</font>  <br>
                Please note: the 'EU-EEA - complete series' figures only include countries with complete time series.", sep="" )
        }else{
          paste("<font style=\"color:#E95420;\"> Warning: </font> All countries present with at least one gap in the time series.")
        }
      }else{
        paste("During the study period from ",
              format(as.Date(input$daterange[1], "%Y-%m-%d"), "%d %B %Y"), " to ",
              format(as.Date(input$daterange[2], "%Y-%m-%d"), "%d %B %Y"), ",
              the selected country presented with
              <font style=\"color:#E95420;\"> at least one gap in the time series.</font>", sep="" )
      }
    })
  })

  #-------------------------------------------------------
  # Time series plot
  #-------------------------------------------------------

  output$plot <- renderPlot({
    if (input$do == FALSE) return()
    isolate({makePlot()})
  })

  #-------------------------------------------------------
  # Download button: Graph
  #-------------------------------------------------------

  output$downloadGraph <- downloadHandler(
    filename = function(){paste('plot-',Sys.Date(),'.png', sep='')},
    content = function(file) {
      png(file, width = 1450, height = 500, res=80)
      isolate({makePlot()})
      dev.off()
    },
    contentType = "image/png"
  )


  #-------------------------------------------------------
  # Result table
  #-------------------------------------------------------

  output$dataset <- renderDataTable({
    if(input$do == FALSE) return()
    isolate({
      Result <- makeTable()
      Result$Signal <- ifelse(Result$Signal == "Signal" , "<span style=\"color:#DD4814\">Signal</span>" , "no")
      if(input$country != "EU-EEA - complete series"){ Result <- Result[, !(names(Result) %in% "Countries (cases)")] }
      Result
    })
  }, escape=FALSE, options = list(dom='tp' ,
                                  pageLength = 5,
                                  autoWidth = TRUE,
                                  columnDefs = list(list(width = '150px', targets = 0:1))))

  #-------------------------------------------------------
  # Download button: Result table
  #-------------------------------------------------------

  output$downloadResTable <- downloadHandler(
    filename <- function(){paste('data-result-table-', Sys.Date(), '.csv', sep = '')},
    content <- function(file){write.table(makeTable(), file, sep = ",", row.names = FALSE)},
    contentType <- "text/csv"
  )


  #-------------------------------------------------------
  # Download button: Result dataset
  #-------------------------------------------------------

  output$downloadData <- downloadHandler(
    filename <- function() {paste('data-timeserie-', Sys.Date(), '.csv', sep='')},
    content <- function(file) {
      dataset <- aggFinalDataset()
      dataset.sts <- makeSts()
      dataset.algo <- makeAlgo()
      Result <- dplyr::bind_cols(
        epoch = dataset.sts@epoch,
        time = dataset$StudyPeriod,
        observed = dataset.sts@observed)
      Algo <- dplyr::bind_cols(
        epoch = dataset.algo@epoch,
        threshold = dataset.algo@upperbound,
        signal = dataset.algo@alarm
      )
      Result <- dplyr::left_join(Result, Algo, by = "epoch")
      Result <- dplyr::select(Result, -epoch)
      write.table(Result, file, sep = ",", row.names = FALSE)
    },
    contentType <- "text/csv"
  )


  #-------------------------------------------------------
  # Download button: HTML Signal Detection Report
  #-------------------------------------------------------
  output$signalDetectionReport <- downloadHandler(
    filename <- "SignalDectectionReport.html",
    content <- function(file){
      EpiSignalDetection::runEpiSDReport(input = input, outputfile = file)
    },
    contentType <- "html"

  )

})
