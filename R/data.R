#' Dataset for Signal Detection Analysis, reported cases, 1999-2018 (ECDC Atlas export)
#'
#' A dataset containing an export from the ECDC Atlas for salmonellosis and measles data.
#' This export is cleaned and ready for Signal Detection Analysis (see. cleanAtlasExport() )
#'
#' @format A data frame with 80,834 rows and 11 variables:
#' \describe{
#'   \item{HealthTopic}{Disease name e.g. Salmonellosis or Measles}
#'   \item{Population}{Population characteristics e.g. All cases, Confirmed cases, Serotype AGONA,
#'   Serotype BAREILLY etc.}
#'   \item{Indicator}{Indicator e.g. Hospitalised cases, Reported cases, Number of deaths, etc.}
#'   \item{Time}{Time variable including both yearly data from 1999 to 2017, and monthly data from 1999-01 to 2018-02}
#'   \item{RegionName}{Geographical level including country names e.g. Austria, Belgium, Bulgaria, etc.}
#'   \item{NumValue}{Number of cases}
#'   \item{TimeUnit}{Time unit corresponding to the format of the date in the 'Time' variable  e.g. Year or Month}
#'   \item{TimeYear}{Year of the date available in the 'Time' variable, regardless of the date format
#'   i.e. 1999 to 2018}
#'   \item{TimeMonth}{Month of the date available in the 'Time' variable, regardless of the date format i.e. 1 to 12}
#'   \item{TimeWeek}{Week of the date available in the 'Time' variable, regardless of the date format
#'   i.e. NA since this dataset does not include any weekly data}
#'   \item{TimeDate}{Approximated date corresponding to the date available in the 'Time' variable  (daily format)}
#' }
#' @docType data
#' @keywords datasets
#' @name SignalData
#' @usage SignalData
#' @source \url{http://atlas.ecdc.europa.eu/public/index.aspx}
"SignalData"


#' List of datasets containing the Farrington Flexible and GLRNB default parameters by time unit
#'
#' A list including two datasets containing the parameters used for Farrington Flexible and for GLRNB
#' for each time unit available in the Signal Detection tool
#'
#' @seealso \code{surveillance::farringtonFlexible} \code{surveillance::glrnb}
#'
#' @format A list of 2 dataframes: one with 2 rows and 9 variables and GRLNB with 2 rows and 8 variables
#' \enumerate{
#' \item \strong{Default parameters for FarringtonFlexible algorithm}
#' \describe{
#'   \item{timeunit}{Time units available in the signal detection tool i.e. week, month}
#'   \item{w}{Window's half-size, i.e. number of weeks to include before and after the current week in each year
#'   (w=2 for weeks, w=1 for months)}
#'   \item{reweight}{Logical specifying whether to reweight past outbreaks or not
#'   (TRUE for both weeks and months, past outbreaks are always reweighted)}
#'   \item{trend}{Logical specifying whether a trend should be included and kept
#'   in case the conditions in the Farrington et. al. paper are met.
#'   (TRUE for both weeks and months, a trend is always fit)}
#'   \item{weightsThreshold}{Numeric defining the threshold for reweighting past outbreaks
#'   using the Anscombe residuals
#'   (2.85 for both weeks and months, as advised in the improved method)}
#'   \item{glmWarnings}{Logical specifying whether to print warnings from the call to glm
#'   (TRUE for both weeks and months)}
#'   \item{pThresholdTrend}{Numeric defining the threshold for deciding whether to keep trend in the model
#'   (0.05 for both weeks and months)}
#'   \item{limit54_1}{Integer, the number of cases defining a threshold for minimum alarm,
#'   no alarm is sounded if fewer than 'limit54_1' cases were reported in the past 'limit54_2' weeks/months}
#'   \item{limit54_2}{Integer, the number of periods defining a threshold for minimum alarm,
#'   no alarm is sounded if fewer than 'limit54_1' cases were reported in the past 'limit54_2' weeks/months}
#' }
#'
#'
#' \item \strong{Default parameters for GLRNB algorithm}
#' \describe{
#'   \item{timeunit}{Time units available in the signal detection tool i.e. week, month}
#'   \item{mu0}{A vector of in-control values of the mean of the Poisson / negative binomial distribution
#'   with the same length as range
#'   - NULL for both weeks and months}
#'   \item{theta}{Numeric, the pre-specified value for k or lambda is used in a recursive LR scheme
#'   - log(1.2) for both weeks and months corresponding to a 20 percent increase in the mean}
#'   \item{alpha}{Numeric, the dispersion parameter of the negative binomial distribution.
#'   If alpha=NULL the parameter is calculated as part of the in-control estimation
#'   - alpha=NULL for both weeks and months}
#'   \item{cARL}{Numeric, the threshold in the GLR test, i.e. c_gamma - cARL=0.25 for both weeks and months}
#'   \item{Mtilde}{Integer, the number of observations needed before we have a full rank
#'   - Mtilde=1 for both weeks and months}
#'   \item{M}{Integer defining the number of time instances back in time in the window-limited approach.
#'   To always look back until the first observation use M=-1. M=1 for both weeks and months}
#'   \item{Change}{Character string specifying the type of the alternative.
#'   Currently the two choices are intercept and epi
#'   - Change=intercept for both weeks and months}
#' }
#'
#' }
#' @docType data
#' @keywords datasets
#' @name AlgoParam
#' @usage AlgoParam
"AlgoParam"


