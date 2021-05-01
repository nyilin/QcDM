#' Example glucose data
#' @description Simulated glucose data in July 2020.
#' @format A data frame with 5594 rows and 4 variables:
#' \describe{
#'   \item{ADMISSION.ID}{Randomly generated integers indicating each patient-stay.}
#'   \item{RESULT}{Randomly generated glucose readings (mmol/L). 3 of the 5594
#'     entries are non-numeric by design.}
#'   \item{RESULT.DATE}{Date-time stamp of glucose readings, randomly generated
#'     to be within the calendar month of July 2020.}
#'   \item{LOCATION}{Randomly generated strings indicating 4 different ward
#'     locations.}
#' }
"gluDat"
