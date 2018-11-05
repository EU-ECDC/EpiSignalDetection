#' Run the EpiSignalDectection 'shiny' application
#'
#' Run the 'shiny' interactive application for signal detection analysis
#' using ECDC Atlas export data.
#'
#' Datasets to use in the tool:
#' \itemize{
#'   \item Default dataset included in the application
#'   (Salmonellosis 2007-2016 or Measles 1999-2018 data);
#'   \item External dataset using the "Browse" button in the application:
#'   \itemize{
#'     \item  --> An export (csv format) from the ECDC Surveillance Atlas of Infectious Diseases:
#'      \href{http://atlas.ecdc.europa.eu/public/index.aspx}{http://atlas.ecdc.europa.eu/public/index.aspx}.
#'
#'      On the ECDC "Surveillance Atlas of Infectious Diseases" web site:
#'
#'     \itemize{
#'       \item{1-} Choose the disease/health topic to analyse
#'       \item{2-} Export the data (csv) using the default settings
#'       \item{3-} Import the csv in the application
#'       \item{4-} You can now explore the disease time series for signal detection...
#'     }
#'     \item --> Any dataset specified as described in the package vignette.
#'  }
#' }
#'
#' @usage runEpiSDApp()
#'
#' @examples
#' \donttest{
#' # --- Run the 'shiny' app
#' # --- (NB: please open the app in an external browser
#' # --- in order to facilitate its use)
#' runEpiSDApp()
#' }
#'
#' @export
#'
runEpiSDApp <- function() {

  #----
  # Setting dates in English
  #----
  Sys.setlocale("LC_TIME", "C")

  #----
  # Running the tool
  #----
  shiny::runApp(system.file('SignalDetectionApp', package = 'EpiSignalDetection'))

}
