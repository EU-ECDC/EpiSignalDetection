#' Run the EpiSignalDetection report (HTML markdown)
#'
#' Function to render the markdown report of alarms in HTML format for ECDC Signal Detection Report
#'
#' Datasets to use in the report:
#' \itemize{
#'   \item Default dataset included in the package
#'   (Salmonellosis 2007-2016 or Measles 1999-2018 data) (i.e.  \code{input$file = NULL});
#'   \item External dataset:
#'   \itemize{
#'     \item  --> An export (csv format) from the ECDC Surveillance Atlas of Infectious Diseases:
#'      \href{http://atlas.ecdc.europa.eu/public/index.aspx}{http://atlas.ecdc.europa.eu/public/index.aspx}.
#'
#'      On the ECDC "Surveillance Atlas of Infectious Diseases" web site:
#'
#'     \itemize{
#'       \item{1-} Choose the disease/health topic to analyse
#'       \item{2-} Export the data (csv) using the default settings
#'       \item{3-} Specify the location of this external dataset in the input
#'       argument of the runEpiSDReport() function
#'       (e.g.  \code{input <- list(file = list(datapath = "C:/Users/Downloads/ECDC_surveillance_data_Pertussis.csv"),
#'       disease = "Pertussis", country = "Greece", indicator = "Reported cases",
#'       stratification = "All cases", unit = "Month", daterange = c("2011-12-01", "2016-12-01"),
#'       algo = "FarringtonFlexible", testingperiod = 3))}
#'       \item{4-} You can now render the re markdown report...
#'       (e.g.  \code{runEpiSDReport(input = input)})
#'     }
#'     \item --> Any dataset specified as described in the package vignette.
#'  }
#' }
#'
#'
#' @param input list of parameters as defined in the Signal Detection Application (see \code{\link{runEpiSDApp}})
#'
#' (i.e.  \code{list(disease, country, indicator, stratification, unit, daterange, algo, testingperiod)})
#'
#' (see also default parameters in
#' \code{system.file("SignalDetectionReport_HTML", "SignalDetectionReport.Rmd", package = "EpiSignalDetection")})
#'
#'
#' @param stratified a logical value indicating whether the report
#' should be stratified by \code{Population} variable or not (default \code{FALSE})
#'
#'
#' @param outputfile output file name (e.g. \code{'C:/R/report.html'})
#'
#' (default value is a temporary folder - \code{file.path(tempdir(), "SignalDectectionReport.html")})
#'
#'
#' @return An HTML report
#'
#'
#' @examples
#' \donttest{
#' #-- Running the report as a standalone function
#' runEpiSDReport()    #Definition of each input parameter
#'                      #is done one by one through the R console
#' }
#' #---> OR
#'
#' #-- First setting the parameters to run the report for
#' input <- list(
#' disease = "Salmonellosis",
#' country = "Portugal",
#' indicator = "Reported cases",
#' stratification = "Confirmed cases",
#' unit = "Month",
#' daterange = c("2011-01-01", "2016-12-31"),
#' algo = "FarringtonFlexible",
#' testingperiod = 6
#' )
#'
#' #-- Second running the report based on the EpiSignalDetection::SignalData dataset
#' #-- and store it in a temporary folder
#' runEpiSDReport(input = input)
#' \donttest{
#' #-- Running the report based on the EpiSignalDetection::SignalData dataset
#' #-- and store the HTML output 'test.html' in the folder 'C:/R/'
#' runEpiSDReport(input = input, outputfile = "C:/R/test.html")
#' }
#' #-- Running the report based on external data
#' input <- list(
#' file = list(datapath = "C:/Users/Downloads/ECDC_surveillance_data_Pertussis.csv"),
#' disease = "Pertussis",
#' country = "Greece",
#' indicator = "Reported cases",
#' stratification = "All cases",
#' unit = "Month",
#' daterange = c("2011-12-01", "2016-12-01"),
#' algo = "FarringtonFlexible",
#' testingperiod = 3
#' )
#' \donttest{
#' runEpiSDReport(input = input, stratified = TRUE)
#' }
#'
#'
#' @seealso Default dataset used in the report \code{\link{SignalData}}
#'
#' Signal Detection Application \code{\link{runEpiSDApp}}
#'
#'
#' @export

runEpiSDReport <- function(input, stratified, outputfile){

  # ---
  # Setting default arguments if missing
  # ---

  if (missing(stratified)) {
    stratified <- FALSE
  }


  if (missing(input)) {
    print("Dataset (please enter the full path to the csv file, or just press 'Enter' to use the default dataset)")
    try(datapath <- scan(what=as.character(c()), sep = ";", nlines = 1), silent=TRUE)
    if(length(datapath)==0){
      file <- NULL
    } else {
      file <- list(datapath = datapath)
    }

    print("Disease / Health topic (e.g. Salmonellosis):")
    try(disease <- scan(what=as.character(c()), sep = ";", nlines = 1), silent=TRUE)

    print("Region name (e.g. EU-EEA - complete series):")
    try(country <- scan(what=as.character(c()), sep = ";", nlines = 1), silent=TRUE)

    print("Indicator (e.g. Reported cases):")
    try(indicator <- scan(what=as.character(c()), sep = ";", nlines = 1), silent=TRUE)

    if(stratified == FALSE) {
      print("Population (e.g. Confirmed cases):")
      try(stratification <- scan(what=as.character(c()), sep = ";", nlines = 1), silent=TRUE)
    }

    print("Time unit (please enter 'Month' or 'Week'):")
    try(unit <- scan(what=as.character(c()), nmax = 1), silent=TRUE)

    print("Start date (e.g. '2011-01-01'):")
    try(start <- scan(what=as.character(c()), nmax = 1), silent=TRUE)

    print("End date (e.g. '2016-12-31'):")
    try(end <- scan(what=as.character(c()), nmax = 1), silent=TRUE)

    daterange <- c( as.Date(start), as.Date(end) )

    print("Statistical algorithm (please enter 'FarringtonFlexible' or 'GLRNB'):")
    try(algo <- scan(what=as.character(c()), nmax=1), silent=TRUE)

    print(paste("Alarm detection period (number of ", tolower(unit), "s to test for signal detection) :", sep =""))
    try(testingperiod <- scan(what=as.integer(c()), nmax = 1), silent=TRUE)

    print("Do you wish to stratify the alarm report by population characteristics? Yes(TRUE) or No(FALSE):")
    try(stratified <- scan(what=as.logical(c()), nmax = 1), silent=TRUE)

    print("Output file name (e.g. C:/R/report.html or press enter to store it in a temporary folder):")
    try(outputfile <- scan(what=as.character(c()), nmax = 1), silent=TRUE)

    input <- list(
      file = file,
      disease = disease,
      country = country,
      indicator = indicator,
      stratification = stratification,
      unit = unit,
      daterange = daterange,
      algo = algo,
      testingperiod = testingperiod
    )
  }


  # ---
  # Stratified or not stratified report
  # ---
  reportName <- ifelse(stratified == TRUE, "StratifiedSignalDetectionReport", "SignalDetectionReport")
  reportHTML <- paste(reportName, ".html", sep = "")
  reportRmd <- paste(reportName, ".Rmd", sep = "")
  reportDir <- ifelse(stratified == TRUE, "StratifiedSignalDetectionReport_HTML", "SignalDetectionReport_HTML")


  # ---
  # Saving the output report file to the same temporary directory if not specified
  # ---
  tempPath <- tempdir()
  if (missing(outputfile)) {
    outputfile <- file.path(tempPath, reportHTML)
  }
  if (length(outputfile)==0) {
    outputfile <- file.path(tempPath, reportHTML)
  }


  # ---
  # Copy the report file to this temporary directory before processing it, in
  # case we don't have write permissions to the current working dir (which
  # can happen when deployed).
  # ---
  tempReport <- file.path(tempPath, reportRmd)
  file.copy(system.file(reportDir, reportRmd, package = "EpiSignalDetection"),
            tempReport,
            overwrite = TRUE)
  file.copy(system.file(reportDir, "subsection.Rmd", package = "EpiSignalDetection"),
            file.path(tempPath, "subsection.Rmd"),
            overwrite = TRUE)
  file.copy(system.file(reportDir, "style.css", package = "EpiSignalDetection"),
            file.path(tempPath, "style.css"),
            overwrite = TRUE)


  # ---
  # Running the report
  # ---

  rmarkdown::render(
    tempReport,
    output_file = outputfile,
    params = list(file = input$file,
                  disease = input$disease,
                  country = input$country,
                  indicator = input$indicator,
                  stratification = input$stratification,
                  unit = input$unit,
                  daterange = input$daterange,
                  algo = input$algo,
                  testingperiod = input$testingperiod,
                  tempPath = tempPath))
}

