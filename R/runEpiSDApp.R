#' Run the EpiSignalDectection Shiny application
#'
#' Run the Shiny interactive application for signal detection analysis
#' based on ECDC Atlas export data.
#' See ECDC Atlas: \href{http://atlas.ecdc.europa.eu/public/index.aspx}{ECDC Surveillance Atlas of Infections Diseases}
#'
#' @usage runEpiSDApp()
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
