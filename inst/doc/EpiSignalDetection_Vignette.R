## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE-------------------------------------------------------
pkgVersion <- packageDescription("EpiSignalDetection")$Version
pkgDate <- packageDescription("EpiSignalDetection")$Date
authorsString <- gsub("^ *|(?<= ) |\n| *$", "", 
                      packageDescription("EpiSignalDetection")$Authors, perl = TRUE)
authorList <- eval(parse(text = authorsString))
pkgAuthors <- paste(format(authorList, 
                           include = c("given", "family", "email", "comment"), 
                           braces = list(email = c("<", ">,<br />"), comment = c("", ""))), 
                    collapse = "<br /><br />")
pkgMaintainer <- packageDescription("EpiSignalDetection")$Maintainer
pkgLicense <- packageDescription("EpiSignalDetection")$License
pkgUrl <- packageDescription("EpiSignalDetection")$URL

## ---- eval=FALSE, echo=TRUE----------------------------------------------
#  install.packages("EpiSignalDetection")
#  library(EpiSignalDetection)
#  EpiSignalDetection::runEpiSDApp()

## ---- echo=FALSE, results='asis'-----------------------------------------
my_dataset <- EpiSignalDetection::importAtlasExport("./data/ECDC_surveillance_data_Pertussis_20180717.csv")
knitr::kable(head(my_dataset), 
             format = "html", table.attr = 'class="myTable"',
             format.args = list(decimal.mark = ".", big.mark = ","),
             align = 'c',
             caption = "__Tab.1 Example of Pertussis data exported from the ECDC Atlas__")

## ---- echo=FALSE, results='asis'-----------------------------------------
my_dataset <- EpiSignalDetection::SignalData
my_dataset <- dplyr::filter(my_dataset, my_dataset$HealthTopic == "Salmonellosis")
my_dataset <- dplyr::group_by_(my_dataset, c("Population") )
my_dataset <- dplyr::summarise(my_dataset, "NumValue" = sum(NumValue, na.rm = TRUE))
my_dataset <- dplyr::ungroup(my_dataset)
my_dataset <- dplyr::arrange(my_dataset, desc(my_dataset$NumValue))
knitr::kable(my_dataset,
             format = "html", table.attr = 'class="myTable"',
             format.args = list(decimal.mark = ".", big.mark = ","),
             align = 'c',
             caption = "__Tab.2 Number of cases in each stratum using Salmonellosis data exported from the ECDC Atlas__")

## ------------------------------------------------------------------------
my_parameters <- EpiSignalDetection::AlgoParam
names(my_parameters)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(my_parameters$FarringtonFlexible, 
             format = "html", table.attr = 'class="myTable"',
             format.args = list(decimal.mark = ".", big.mark = ","),
             align = 'c',
             caption = "__Tab.3 Parameters for the Farrington Flexible algorithm__")
knitr::kable(my_parameters$GLRNB, 
             format = "html", table.attr = 'class="myTable"',
             format.args = list(decimal.mark = ".", big.mark = ","),
             align = 'c',
             caption = "__Tab.4 Parameters for the GLRNB algorithm__")

## ---- echo = FALSE, results = 'hide'-------------------------------------
my_input <- list(
  disease = "Salmonellosis",
  country = "EU-EEA - complete series",
  indicator = "Reported cases",
  stratification = "Confirmed cases",
  unit = "Month",
  daterange = c("2012-01-01", "2016-12-31"),
  algo = "FarringtonFlexible",
  testingperiod = 5
)

png(file = "plots/plot_time_series.png", width = 1450, height = 500, res=90)
EpiSignalDetection::plotSD(input = my_input)
dev.off()

## ------------------------------------------------------------------------
my_dataset <- EpiSignalDetection::SignalData
knitr::kable(head(my_dataset))

## ------------------------------------------------------------------------
my_input <- list(
  disease = "Salmonellosis",
  country = "EU-EEA - complete series",
  indicator = "Reported cases",
  stratification = "Confirmed cases",
  unit = "Month",
  daterange = c("2012-01-01", "2016-12-31"),
  algo = "FarringtonFlexible",
  testingperiod = 5
)

## ---- eval = FALSE-------------------------------------------------------
#  EpiSignalDetection::plotSD(input = my_input)

## ---- eval = FALSE-------------------------------------------------------
#  EpiSignalDetection::runEpiSDReport(
#    outputfile = "C:/R/test.html",
#    input = my_input)

## ---- eval = FALSE-------------------------------------------------------
#  EpiSignalDetection::runEpiSDReport(
#    outputfile = "C:/R/test.html",
#    input = my_input,
#    stratified = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  EpiSignalDetection::runEpiSDReport()
#  [1] "Dataset (please enter the full path to the csv file, or just press 'Enter' to use the default dataset)"
#  1:
#  Read 0 items
#  [1] "Disease / Health topic (e.g. Salmonellosis):"
#  1:  Salmonellosis
#  Read 1 item
#  [1] "Region name (e.g. EU-EEA - complete series):"

## ------------------------------------------------------------------------
my_dataset <- EpiSignalDetection::importAtlasExport("./data/ECDC_surveillance_data_Pertussis_20180717.csv")

## ------------------------------------------------------------------------
my_dataset <- EpiSignalDetection::cleanAtlasExport(my_dataset)

## ------------------------------------------------------------------------
my_input <- list(
  disease = "Pertussis",
  country = "EU-EEA - complete series",
  indicator = "Reported cases",
  stratification = "All cases",
  unit = "Month",
  daterange = c("2012-01-01", "2016-12-31"),
  algo = "FarringtonFlexible",
  testingperiod = 6
)

## ------------------------------------------------------------------------
my_dataset <- EpiSignalDetection::filterAtlasExport(my_dataset, my_input)

## ------------------------------------------------------------------------
my_dataset <- EpiSignalDetection::aggAtlasExport(my_dataset, my_input)

## ---- eval = FALSE-------------------------------------------------------
#  EpiSignalDetection::plotSD(x = my_dataset, input = my_input)

## ---- echo = FALSE, results = 'hide'-------------------------------------
png(file = "plots/plot_time_series_external.png", width = 1450, height = 500, res=90)
EpiSignalDetection::plotSD(x = my_dataset, input = my_input)
dev.off()

## ---- eval = FALSE-------------------------------------------------------
#  my_input <- list(
#    file = list(datapath = "C:/data/ECDC_surveillance_data_Pertussis_20180717.csv"),
#    disease = "Pertussis",
#    country = "Greece",
#    indicator = "Reported cases",
#    stratification = "All cases",
#    unit = "Month",
#    daterange = c("2011-12-01", "2016-12-01"),
#    algo = "FarringtonFlexible",
#    testingperiod = 3
#  )
#  
#  EpiSignalDetection::runEpiSDReport(input = my_input)

## ---- eval = FALSE-------------------------------------------------------
#  my_input <- list(
#    file = list(datapath = "C:/data/ECDC_surveillance_data_Salmonella_20180717.csv"),
#    disease = "Salmonellosis",
#    country = "EU-EEA - complete series",
#    indicator = "Reported cases",
#    stratification = "Confirmed cases",
#    unit = "Month",
#    daterange = c("2011-12-01", "2016-12-01"),
#    algo = "FarringtonFlexible",
#    testingperiod = 6
#  )
#  
#  EpiSignalDetection::runEpiSDReport(input = my_input, stratified = TRUE)

