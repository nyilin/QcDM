#' Example glucometrics data
#' @description Simulated data from ward BG in July 2016.
#' @format A data frame with 79 rows and 7 variables:
#' \describe{
#'   \item{ADMISSION.ID}{Admission ID.}
#'   \item{RESULT}{Glucose readings (mmol/L), where 3 entries are non-numeric by
#'     design.}
#'   \item{RESULT.DATE}{Date and time of glucose readings.}
#'   \item{LOCATION}{Name of ward.}
#'   \item{caseid}{Case ID (\code{caseid} is used as a place holder when this
#'     information is not available).}
#'   \item{BIRTH.DATE}{Birthday of each patient (\code{BIRTH.DATE} is used as a
#'     place holder when this information is not available).}
#'   \item{AGE}{Age of each patient (\code{AGE} is used as a place holder when
#'     this information is not available).}
#' }
"gluDat"
