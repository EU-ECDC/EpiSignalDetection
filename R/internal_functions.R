#' Import ECDC Atlas export file (csv)
#'
#' Import ECDC Atlas csv export file \cr
#' (exported from the online ECDC Atlas:
#' \href{http://atlas.ecdc.europa.eu/public/index.aspx}{http://atlas.ecdc.europa.eu/public/index.aspx}) \cr
#' e.g. "ECDC_surveillance_data_Anthrax.csv" \cr
#'
#'
#' The function will interpret missing reports '-' as NA values
#'
#'
#' @param x file name of a csv file, export from the ECDC Atlas
#'
#' (e.g. \code{x = 'ECDC_surveillance_data_Anthrax.csv'})
#'
#'
#' @return dataframe
#'
#' @usage importAtlasExport(x)
#'
#' @examples
#' \donttest{
#' dataset <- importAtlasExport(x = 'ECDC_surveillance_data_Anthrax.csv')
#' }
#'
#'
#' @seealso \code{\link{cleanAtlasExport}}
#'
#' @export

importAtlasExport <- function(x) {

  utils::read.table(x,
                    header = TRUE,
                    sep = ",",
                    quote = "\"",
                    stringsAsFactors = FALSE,
                    na.strings = "-") ##Please note that a missing report '-' will be NA

}



#' Clean the Atlas export dataframe
#'
#' Clean the Atlas data export dataframe before signal detection analysis \cr
#' (see \code{\link{importAtlasExport}} and online ECDC Atlas:
#' \href{http://atlas.ecdc.europa.eu/public/index.aspx}{http://atlas.ecdc.europa.eu/public/index.aspx}) \cr
#'
#'
#' The function will: \cr
#' \itemize{
#'    \item Filter only on case based indicators i.e. 'Reported Cases"
#'    \item Create four additional time variables to ease the analysis: \cr
#'    TimeUnit ('Year', 'Month', 'Week'), \cr
#'    TimeYear (xxxx), \cr
#'    TimeMonth (xx) \cr
#'    TimeWeek(xx)
#'    \item Keep only variables of interest i.e. "HealthTopic", "Population", "Time", "RegionName", "NumValue"
#' }
#'
#'
#'
#' @param x dataframe, usually the ouput of the import function \code{importAtlasExport(x)}
#'
#' @return dataframe
#'
#' @usage cleanAtlasExport(x)
#'
#' @seealso \code{\link{importAtlasExport}} \code{\link{filterAtlasExport}}
#'
#' @examples
#' \donttest{
#' dataset <- cleanAtlasExport( importAtlasExport(x = 'ECDC_surveillance_data_Anthrax.csv') )
#' }
#'
#'
#' @export

cleanAtlasExport <- function(x) {

  #-- Keep indicator 'Reported cases' only
  # x <- dplyr::filter(x, x$Indicator == "Reported cases")
  x <- dplyr::filter(x, x$Unit == "N")


  #-- Keep variables of interest
  x <- dplyr::select(x, c("HealthTopic", "Population",
                          "Indicator", "Time",
                          "RegionName", "NumValue"))


  #-- Formating Time Unit variable for Date variables
  x$TimeUnit <- ifelse(grepl("[0-9]{4}", x$Time),
                       "Year", NA)
  x$TimeUnit <- ifelse(grepl("[0-9]{4}-[0-9]{2}", x$Time),
                       "Month", x$TimeUnit)
  x$TimeUnit <- ifelse(grepl("[0-9]{4}-W[0-9]{2}", x$Time),
                       "Week", x$TimeUnit)


  #-- Formating Date variables
  x$TimeYear <- ifelse(x$TimeUnit == "Year",
                       as.numeric(substr(x$Time, 1, 4)), NA)
  x$TimeYear <- ifelse(x$TimeUnit == "Month",
                       as.numeric(substr(x$Time, 1, 4)), x$TimeYear)
  x$TimeYear <- ifelse(x$TimeUnit == "Week",
                       as.numeric(substr(x$Time, 1, 4)), x$TimeYear)
  x$TimeMonth <- ifelse(x$TimeUnit == "Month",
                        as.numeric(substr(x$Time, 6, 8)), NA)
  x$TimeWeek <- ifelse(x$TimeUnit == "Week",
                       substr(x$Time, 7, 9), NA)


  #-- Approximating the date
  x$TimeDate <- ifelse(x$TimeUnit == "Week",
                       paste(x$TimeYear, "-W", x$TimeWeek, "-1", sep = ""), NA)
  x$TimeDate <- ISOweek::ISOweek2date(x$TimeDate)


  #-- Adjusting the month when weekly data
  x$TimeMonth <- ifelse(x$TimeUnit == "Week",
                        as.numeric(format(x$TimeDate, "%m")), x$TimeMonth)
  x$TimeDate <- ifelse(x$TimeUnit == "Year",
                       as.Date(paste(x$TimeYear, "01", "01", sep = "/"), "%Y/%m/%d"),
                       x$TimeDate)
  x$TimeDate <- ifelse(x$TimeUnit == "Month",
                       as.Date(paste(x$TimeYear, x$TimeMonth, "01", sep = "/"), "%Y/%m/%d"),
                       x$TimeDate)
  x$TimeDate <- as.Date(x$TimeDate, origin = "1970-01-01")


  #-- Converting the week in numeric (string was necessary to convert into approximate the date using ISOweek)
  x$TimeWeek <- ifelse(x$TimeUnit == "Week", as.numeric(x$TimeWeek), NA)

  x <- dplyr::arrange(x, x$RegionName, x$Population, x$TimeYear, x$HealthTopic)

  return(x)

}


#' Compute the study period
#'
#' Compute a dataframe including two types of dates corresponding
#' to the study period defined in the list of parameters \code{input} \cr
#' (i.e. \code{StudyPeriod} = approximated daily date; \code{Time} = exact date in the format according to the time unit parameter)
#'
#' @param input list of parameters as defined in the Signal Detection Application (see \code{\link{runEpiSDApp}})
#'
#' (i.e.  \code{list(disease, country, indicator, stratification, unit, daterange, algo, testingperiod)})
#'
#' @return Dataframe including the complete time series with no gaps:
#' \item{StudyPeriod}{approximated daily date e.g. \code{2010-01-01}}
#' \item{Time}{exact date in the format according to the time unit parameter e.g. \code{2010-01}}
#'
#' @usage studyPeriod(input)
#'
#' @examples
#' #-- Setting the parameters to run the report for
#' input <- list(
#' disease = "Salmonellosis",
#' country = "EU-EEA - complete series",
#' indicator = "Reported cases",
#' stratification = "Confirmed cases",
#' unit = "Month",
#' daterange = c("2010-01-01", "2016-12-31"),
#' algo = "FarringtonFlexible",
#' testingperiod = 5
#' )
#'
#' StudyPeriod <- studyPeriod(input)
#' head(StudyPeriod)
#'
#' @export

studyPeriod <- function(input){

  #--- Defining the study period
  if (input$unit %in% c("Year", "Month", "Week")) {

    Start <- as.Date(as.character(input$daterange[1]), "%Y-%m-%d")
    if (input$unit == "Month") {
      Start <- as.Date(paste(substr(as.character(input$daterange[1]), 1, 7), "-01", sep="" ),
                       "%Y-%m-%d")
    }
    End <- as.Date(as.character(input$daterange[2]), "%Y-%m-%d")
    StudyPeriod <- seq(Start, End, tolower(input$unit) )


    #--- Filtering on the study period
    if (input$unit == "Week") {
      StudyPeriod <- dplyr::mutate(as.data.frame(StudyPeriod),
                                   Time = ISOweek::ISOweek(StudyPeriod))
    }
    if (input$unit == "Month") {
      StudyPeriod <- dplyr::mutate(as.data.frame(StudyPeriod),
                                   Time = format(StudyPeriod, "%Y-%m"))
    }
    if (input$unit == "Year") {
      StudyPeriod <- dplyr::mutate(as.data.frame(StudyPeriod),
                                   Time = format(StudyPeriod, "%Y"))
    }

    return(StudyPeriod)

  } else { return() }
}



#' Filter clean Atlas export
#'
#' Filter clean Atlas export according to input parameters
#'
#' @param x dataframe, clean Atlas export (see \code{\link{cleanAtlasExport}})
#'
#' @param input list of parameters as defined in the Signal Detection Application (see \code{\link{runEpiSDApp}})
#'
#' (i.e.  \code{list(disease, country, indicator, stratification, unit, daterange, algo, testingperiod)})
#'
#' @param stratified a logical value indicating whether the report
#' should be stratified by \code{Population} variable or not (default \code{FALSE})
#'
#' @usage filterAtlasExport(x, input, stratified)
#'
#' @return dataframe filtered on the selected parameters (input list)
#'
#' @examples
#' #-- Setting the parameters to run the report for
#' input <- list(
#' disease = "Salmonellosis",
#' country = "EU-EEA - complete series",
#' indicator = "Reported cases",
#' stratification = "Confirmed cases",
#' unit = "Month",
#' daterange = c("2010-01-01", "2016-12-31"),
#' algo = "FarringtonFlexible",
#' testingperiod = 5
#' )
#'
#' #-- Example dataset
#' dataset <- EpiSignalDetection::SignalData
#'
#' #-- Filtering on declared input parameters
#' dataset <- filterAtlasExport(dataset, input, stratified = FALSE)
#'
#' @seealso \code{\link{cleanAtlasExport}} \code{\link{aggAtlasExport}}
#'
#' @export

filterAtlasExport <- function(x, input, stratified){

  #--- Default value
  if (missing(stratified)) {
    stratified <- FALSE
  }

  #--- Filtering on disease
  x <- dplyr::filter(x, x$HealthTopic == input$disease)

  #--- Filtering on country
  if (input$country != "EU-EEA - complete series") {
    x <- dplyr::filter(x, x$RegionName == input$country)
  }

  #--- Filtering on indicator
  x <- dplyr::filter(x, x$Indicator == input$indicator)

  #--- Filtering on stratification level for not stratified reports
  if (stratified == FALSE) {
    x <- dplyr::filter(x, x$Population == input$stratification)
  }

  #--- Filtering on time unit
  x <- dplyr::filter(x, x$TimeUnit == input$unit)

  #--- Filtering on Study period
  StudyPeriod <- studyPeriod(input)
  RegionName <- data.frame(RegionName = unique(x$RegionName))
  StudyPeriod <- merge(StudyPeriod, RegionName, by = NULL)
  StudyPeriod$RegionName <- as.character(StudyPeriod$RegionName)

  if ( sum(x$Time  %in% StudyPeriod$Time) != length(x$Time)){
    x <- dplyr::filter(x, x$Time  %in% StudyPeriod$Time )
  }


  #--- Adding NA when gaps in Time Series
  if ( !("StudyPeriod" %in% names(x)) ) {
    x <- dplyr::left_join(StudyPeriod, x, by=c("Time","RegionName"))

  }

  return(x)

}



#' Aggregate filtered final Atlas export
#'
#' Aggregate filtered final Atlas export
#'
#' @param x dataframe
#' @param input list of parameters as defined in the Signal Detection Application (see \code{\link{runEpiSDApp}})
#'
#' (i.e.  \code{list(disease, country, indicator, stratification, unit, daterange, algo, testingperiod)})
#'
#' @return dataframe aggregated by geographical level and time unit
#'
#' @usage aggAtlasExport(x, input)
#'
#' @examples
#' #-- Setting the parameters to run the report for
#' input <- list(
#' disease = "Salmonellosis",
#' country = "EU-EEA - complete series",
#' indicator = "Reported cases",
#' stratification = "Confirmed cases",
#' unit = "Month",
#' daterange = c("2010-01-01", "2016-12-31"),
#' algo = "FarringtonFlexible",
#' testingperiod = 5
#' )
#'
#' #-- Example dataset
#' dataset <- EpiSignalDetection::SignalData
#'
#' #-- Filtering on declared input parameters
#' dataset <- filterAtlasExport(dataset, input)
#'
#' #-- Aggregating the data by geographical level and time point
#' dataset <- aggAtlasExport(dataset, input)
#'
#' @seealso \code{\link{filterAtlasExport}} \code{\link{SignalData}} \code{\link{stsSD}}
#'
#' @export

aggAtlasExport <- function(x, input){

  #-- Excluding countries with gaps and EU pre-computed values from the TS
  excluded <- unique(x$RegionName[ is.na(x$NumValue) ])

  if (input$country == "EU-EEA - complete series") {
    included <- unique(x$RegionName[!(x$RegionName %in% c(excluded, "EU", "EU/EEA"))])
    if (length(included) != 0) {
      x <- dplyr::filter(x, x$RegionName %in% included )
    } else {
      warning("All countries present with at least one gap in the time series")
    }
  } else if (length(excluded) !=0) {
    warning("The selected country presents with at least one gap in the time series")
  }

  #-- Aggregation by time and place
  RegionName <- NumValue <- NULL
  x <-  dplyr::group_by_(x, c("StudyPeriod") )
  x <-  dplyr::summarise(x,
                         "CasesByCountry" = paste(RegionName, " (", NumValue, ")", sep = "", collapse = ", "),
                         "NumValue" = sum(NumValue, na.rm = TRUE)  )
  x <-  dplyr::ungroup(x)

  x <- dplyr::arrange(x, x$StudyPeriod)

  return(x)
}



#' Build sts object
#'
#' Build sts surveillance object
#'
#' @param observedCases numeric vector of the number of cases by time unit (y axis of the time series)
#' @param studyPeriod vector of dates of length(obeservedCases) (x axis of the time series)
#' @param timeUnit character string for the time unit of the time series. Options are Week or Month.
#' @param startYM numeric vector including Year and Month of start of the historical data
#' @return sts
#' @usage stsSD(observedCases, studyPeriod, timeUnit = "Month", startYM = c(2000, 1) )
#' @examples
#' #-- Setting the parameters to run the report for
#' input <- list(
#' disease = "Salmonellosis",
#' country = "EU-EEA - complete series",
#' indicator = "Reported cases",
#' stratification = "Confirmed cases",
#' unit = "Month",
#' daterange = c("2010-01-01", "2016-12-31"),
#' algo = "FarringtonFlexible",
#' testingperiod = 5
#' )
#'
#' #-- Example dataset
#' dataset <- EpiSignalDetection::SignalData
#'
#' #-- Filtering on declared input parameters
#' dataset <- filterAtlasExport(dataset, input)
#'
#' #-- Aggregating the data by geographical level and time point
#' dataset <- aggAtlasExport(dataset, input)
#'
#' #-- Bulding the corresponding sts object
#' dataset.sts <- stsSD(observedCases = dataset$NumValue,
#'                      studyPeriod = dataset$StudyPeriod,
#'                      timeUnit = input$unit,
#'                      startYM = c(as.numeric(format(as.Date(input$daterange[1], "%Y-%m-%d"), "%Y")),
#'                                  as.numeric(format(as.Date(input$daterange[1], "%Y-%m-%d"), "%m"))))
#'
#' @seealso \code{\link{aggAtlasExport}} \code{\link{algoSD}}
#'
#' @export

stsSD <- function(observedCases, studyPeriod, timeUnit = "Month", startYM = c(2000, 1) ){

  if (timeUnit == "Month") {
    fq <- 12
  } else if (timeUnit == "Week") {  #This is a proxy for the number of ISO weeks in a year
    fq <- 52
  } else {
    fq <- 1
  }

  x.sts <- surveillance::sts( observed = observedCases,
                              start = startYM,
                              freq = fq,
                              epochAsDate = TRUE,
                              epoch = as.numeric(studyPeriod))
  return(x.sts)

}



#' Build algo object
#'
#' Build algo object from an sts object class
#' using either FarringtonFlexible or GLRNB surveillance algorithm
#'
#' @param x.sts sts class object (see \code{\link{stsSD}} output)
#' @param algo character string containing the name of the algorithm to use.
#' Options are "FarringtonFlexible" (default) or "GLRNB".
#' @param timeUnit character string for the time unit of the time series. Options are "Week" or "Month".
#' @param testingPeriod numeric: number of time units (months, weeks) back in time to test the algorithm on
#' (to detect outbreaks in)
#'
#' @return sts
#'
#' @usage algoSD(x.sts, algo = "FarringtonFlexible", timeUnit = "Month", testingPeriod = 5)
#'
#' @examples
#' #-- Setting the parameters to run the report for
#' input <- list(
#' disease = "Salmonellosis",
#' country = "EU-EEA - complete series",
#' indicator = "Reported cases",
#' stratification = "Confirmed cases",
#' unit = "Month",
#' daterange = c("2010-01-01", "2016-12-31"),
#' algo = "FarringtonFlexible",
#' testingperiod = 5
#' )
#'
#' #-- Example dataset
#' dataset <- EpiSignalDetection::SignalData
#'
#' #-- Filtering on declared input parameters
#' dataset <- filterAtlasExport(dataset, input)
#'
#' #-- Aggregating the data by geographical level and time point
#' dataset <- aggAtlasExport(dataset, input)
#'
#' #-- Bulding the corresponding sts object
#' dataset.sts <- stsSD(observedCases = dataset$NumValue,
#'                      studyPeriod = dataset$StudyPeriod,
#'                      timeUnit = input$unit,
#'                      startYM = c(as.numeric(format(as.Date(input$daterange[1], "%Y-%m-%d"), "%Y")),
#'                                  as.numeric(format(as.Date(input$daterange[1], "%Y-%m-%d"), "%m"))))
#'
#' #-- Building the corresponding algo object
#' dataset.algo <- algoSD(dataset.sts,
#'                        algo = input$algo,
#'                        timeUnit = input$unit,
#'                        testingPeriod =
#'                        input$testingperiod)
#'
#' @seealso \code{\link{stsSD}} \code{\link{plotSD}}
#'
#' @export

algoSD <- function(x.sts, algo = "FarringtonFlexible",
                   timeUnit = "Month", testingPeriod = 5){

  #-- Retrieving algorithm parameters to use according to the selected time unit
  params <- EpiSignalDetection::AlgoParam[[algo]]
  params <- dplyr::filter(params, params$timeunit == tolower(timeUnit))

  TestingPeriodTS <- ((length(x.sts@epoch) - testingPeriod + 1):length(x.sts@epoch))

  #-------------------------------
  #--- FarringtonFlexible
  #-------------------------------
  if (algo == "FarringtonFlexible") {

    #-- Estimating b parameter
    if (timeUnit == "Month") {
      b <- floor( ( utils::tail(x.sts@epoch,1) - x.sts@epoch[1] ) / 365.25 -
                    testingPeriod / 12)
    }
    if (timeUnit == "Week") {
      b <- floor( ( utils::tail(x.sts@epoch,1) - x.sts@epoch[1] ) / 365.25 -
                    testingPeriod / 52)
    }

    #-- Computing control parameter
    control <- list( range = TestingPeriodTS,
                     b = b,
                     w = params$w,
                     reweight = params$reweight,
                     trend = params$trend,
                     plot = FALSE,
                     weightsThreshold = params$weightsThreshold,
                     glmWarnings = params$glmWarnings,
                     pThresholdTrend = params$pThresholdTrend,
                     limit54 = c(params$limit54_1, params$limit54_2))

    #-- Error message if the algorithm fails
    shiny::validate(
      shiny::need(try(surveillance::farringtonFlexible(x.sts, control = control)),
                  "\n \n \n
             ---   The Farrington algorithm cannot proceed.   --- \n
             \n \n \n")
    )

    x.algo <- surveillance::farringtonFlexible( x.sts , control = control)

    return(x.algo)

  }

  #-------------------------------
  #--- GLRNB
  #-------------------------------
  if(algo == "GLRNB"){

    control <- list( range = TestingPeriodTS,
                     c.ARL = params$cARL,
                     mu0 = eval( parse( text = params$mu0)) ,
                     alpha = eval( parse( text = params$alpha)),
                     Mtilde = params$Mtilde,
                     M = params$M,
                     change = params$Change,
                     theta = eval( parse( text = params$theta )) ,
                     ret = "cases")

    #--- Error message in case the algorithm fails
    shiny::validate(
      shiny::need(try(surveillance::glrnb(x.sts, control = control)),
                  "\n \n \n
             ---   The GLRNB algorithm cannot proceed.   --- \n
             \n \n \n")
    )
    x.algo <- surveillance::glrnb(x.sts, control = control)
  }

  return(x.algo)

}




#' Plot the Signal Detection time series
#'
#' Plot the Signal Detection time series including historical data, alarm detection period and alarms
#'
#' @param x dataframe (default \code{\link{SignalData}})
#' @param input list of parameters as defined in the Signal Detection Application (see \code{\link{runEpiSDApp}})
#'
#' (i.e.  \code{list(disease, country, indicator, stratification, unit, daterange, algo, testingperiod)})
#'
#' @param subRegionName character string, region label to use in the plot, if different than \code{input$RegionName} (optional)
#' @param x.sts sts object (optional), see \code{\link{stsSD}})
#' @param x.algo algo object (optional), see \code{\link{algoSD}})
#'
#' @return plot
#'
#' @usage plotSD(x, input, subRegionName, x.sts, x.algo)
#'
#' @examples
#' #-- Setting the parameters to run the report for
#' input <- list(
#' disease = "Salmonellosis",
#' country = "EU-EEA - complete series",
#' indicator = "Reported cases",
#' stratification = "Confirmed cases",
#' unit = "Month",
#' daterange = c("2010-01-01", "2016-12-31"),
#' algo = "FarringtonFlexible",
#' testingperiod = 5
#' )
#'
#' #-- Plotting the signal detection output
#' plotSD(input = input)
#'
#' @seealso \code{\link{SignalData}} \code{\link{runEpiSDApp}}
#' @export

plotSD <- function(x, input, subRegionName, x.sts, x.algo){

  # ---
  # Setting default arguments if missing
  # ---

  if (missing(x)) {
    x <- EpiSignalDetection::SignalData
    x <- filterAtlasExport(x, input)
    x <- aggAtlasExport(x, input)
  }

  if (missing(subRegionName)) {
    subRegionName <- input$country
  }

  if (missing(x.sts)) {
    x.sts <- stsSD(observedCases = x$NumValue,
                   studyPeriod = x$StudyPeriod,
                   timeUnit = input$unit,
                   startYM = c(as.numeric(format(as.Date(input$daterange[1], "%Y-%m-%d"), "%Y")),
                               as.numeric(format(as.Date(input$daterange[1], "%Y-%m-%d"), "%m"))))
  }

  params <- EpiSignalDetection::AlgoParam[[input$algo]]
  params <- dplyr::filter(params, params$timeunit == tolower(input$unit))

  if (missing(x.algo)){
    x.algo <- algoSD(x.sts,
                     algo = input$algo,
                     timeUnit = input$unit,
                     testingPeriod = input$testingperiod)
  }


  #----
  # Main colors used
  #----
  COLOR1 <- "#AFD082"
  COLOR2 <- "green4"
  COLOR3 <- "#CB5F13"


  Historical <- (length(x.sts@epoch) - input$testingperiod)
  Max <- max(x.sts@observed, x.algo@upperbound, na.rm = TRUE)
  Xunit <- ifelse(input$unit == "Month", 1, 4)

  graphics::par(xpd = TRUE, cex.main = 1.6, cex.axis = 1,
                las = 1, mar = c(7.1,4.1,4.1,2.1))
  graphics::plot( x.sts@observed[1:Historical],
                  ty = "l",
                  lwd = 2,
                  col = COLOR1,
                  xlim = c(1, length(x.sts@epoch)),
                  ylim = c(0, Max),
                  xaxt = "n", bty = "l", yaxs = "i", xaxs = "i",
                  main = paste( input$disease, " (", tolower(input$indicator), ") ",
                                tolower(input$stratification), " by ", tolower(input$unit),
                                ",\n in ", subRegionName,
                                " from ", input$daterange[1]," until ", input$daterange[2], sep=""),
                  xlab = "",
                  ylab = "Number of cases")
  graphics::title(xlab = paste("Date (by ", tolower(input$unit), ")", sep = ""), line = 5.5)
  Xaxis = seq(1, length(x.sts@epoch), by = Xunit)
  graphics::axis(1 , Xaxis, x$StudyPeriod, labels = FALSE)
  graphics::text(Xaxis,
                 labels = x$StudyPeriod[Xaxis],
                 graphics::par("usr")[3],
                 srt = 90,
                 xpd = TRUE,
                 cex = 0.9 ,
                 pos = 1,
                 offset = 2.9)
  graphics::lines(y = x.algo@observed ,
                  x = ((Historical + 1):(length(x.sts@epoch) )),
                  ty = "l" ,
                  lty = 5,
                  lwd = 2,
                  col = COLOR1)
  graphics::lines(y = x.sts@observed[(Historical):(Historical+1)] ,
                  x = ((Historical):(Historical+1)),
                  ty = "l" ,
                  lty = 2,
                  lwd = 1,
                  col = COLOR1)
  graphics::lines(y = c(x.sts@observed[Historical],x.algo@upperbound[1] ) ,
                  x = ((Historical):(Historical+1)),
                  ty = "l" ,
                  lty = 2,
                  lwd = 1,
                  col = COLOR3)
  graphics::lines(y = x.algo@upperbound,
                  x = ((Historical + 1):(length(x.sts@epoch) )),
                  ty= "l" ,
                  lwd = 2,
                  col = COLOR3 )
  graphics::points(x = Historical + which(x.algo@alarm == TRUE),
                   y = rep(-0.05, sum(x.algo@alarm, na.rm = TRUE)),
                   type = "p",
                   pch = 17,
                   col = 2)
  graphics::legend(x = "topright",
                   cex = 1,
                   bty = 'n',
                   inset = c(0.01,-0.2),
                   c("Historical data", "Observed cases", "Threshold", "Alarms"),
                   lty = c(1,5,1,NA),
                   pch = c(NA,NA,NA,17),
                   lwd = 2,
                   col = c(COLOR1,COLOR1,COLOR3,"red"))


}






