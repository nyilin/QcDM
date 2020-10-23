#' @title Format date variables using \code{\link{recogniseDateTime}}
#'
#' @description Format date variable from character to R class \dQuote{POSIXct} so that we can manipulate with date variables
#'
#' @param dat  A \emph{data.frame}, has at least one key variable:
#'   \itemize{
#'  \item{RESULT.DATE} an object of classes have information calender date and times
#'  }
#' @param yy  A numeric value specifies the year without centuries of the data set.
#' @param mm A numeric value specifies the month of the data set.
#'
#' @details none
#' @return A \emph{data.table}, has at least one key variable:
#' \itemize{
#' \item{RESULT.DATE} an object of classes \dQuote{POSIXlt} and \dQuote{POSIXct} representing calender date and times.
#'  }
#'
#' @author Ying Chen
#' @export
FormatDate <- function(dat, yy, mm) {
  dat = data.table(dat)
  dat[, Result.Date := recogniseDateTime(dateTime = RESULT.DATE,
                                         month = mm, year = yy),
      by = RESULT.DATE] # need to specify month and year
  dat[, RESULT.DATE := Result.Date]
  dat[, Result.Date := NULL]
  tmp <- dat
  tmp1 <- Sys.time()
  tmp1 = as.POSIXct(strptime(tmp$RESULT.DATE, "%d/%m/%Y %H:%M:%S"))
  #id = which(is.na(tmp1))
  tmp$RESULT.DATE = tmp1
  dat = tmp
  return(dat)
}
