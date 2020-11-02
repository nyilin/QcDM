#' Recognise one date given its month and year
#' @param date A string of date, where date, month and year are separated by
#'   either "-" or "/", but not both.
#' @param month A number (1 to 12) indicating the month this time point is in.
#' @param year A number (with 4 digits) indicating the year this time point is in.
#' @return Returns the date as a character variable of the format %d/%m/%Y.
recog_one_date <- function(date, month, year) {
  year0 <- year
  if (is.null(date) || is.na(date)) return(NA)

  if (length(grep(pattern = "/", date)) > 0) {
    date_vec <- as.numeric(unlist(strsplit(date, "/")))
  } else {
    date_vec <- as.numeric(unlist(strsplit(date, "-")))
  }
  if (length(date_vec) != 3) {
    warning(simpleWarning("date should have 3 components for date, month and year, separated by either '-' or '/', but not both."))
    return(NA)
  }
  yy_i <- which(nchar(date_vec) > 2)
  # If there is a component with more than 2 digits, it is year
  if (length(yy_i) <= 1) {
    if (length(yy_i) == 0) {# Year has 2 digits
      year <- year %% 100
      yy_i <- which(date_vec == year)[1] # In case 2-digit year is equal to month or date
    }
    if (length(yy_i) == 0 || date_vec[yy_i] != year) {
      warning(simpleWarning("Year specified does not match with year detected from input."))
      return(NA)
    }
    date_vec <- date_vec[-yy_i]
    if (!any(date_vec == month)) {
      warning(simpleWarning("Month specified does not match with month detected from input."))
      return(NA)
    }
    mm_i <- which(date_vec == month)[1] # In case date is equal to month
    dd <- date_vec[-mm_i]
  } else {
    # More than 1 component with more than 2 digits: should not happen
    warning(simpleWarning("date cannot be recognised."))
    return(NA)
  }
  paste(dd, month, year0, sep = "/")
}
#' Recognise one time (input as a vector if AM/PM is specified)
#' @param time A string vector, where the second component (if any) indicates AM
#'   or PM.
#' @return Returns the time as a character variable of the format %H:%M:%S.
recog_one_time <- function(time) {
  time <- as.character(time)
  if (anyNA(time)) {
    warning(simpleWarning("Please specify time as string."))
    return(NA)
  }
  if (length(time) > 2) {
    warning(simpleWarning("time cannot be recognised."))
    return(NA)
  } else {
    if (length(time) == 1) {# Time is in 24-hour format
      time_format <- "24hr"
    } else {
      if (!toupper(time[2]) %in% c("AM", "PM")) {
        warning(simpleWarning("Error in input time."))
        return(NA)
      }
      time_format <- ifelse(toupper(time[2]) == "PM", "pm", "am")
    }
    time <- time[1]
  }
  time_vec <- unlist(strsplit(time, ":"))
  time_len <- length(time_vec)
  if (time_len < 2 | time_len > 3) {
    warning(simpleWarning("Time should have 2 or 3 components, separated by ':'."))
    return(NA)
  } else {
    hh <- as.numeric(time_vec[1])
    if (hh >= 24) {
      warning(simpleWarning("Error in input time."))
      return(NA)
    }
    if (time_format == "pm") {# Change to 24-hour format: hh should be 12-23
      hh <- ifelse(hh >= 12, hh, hh + 12)
    } else if (time_format == "am") {# Change to 24-hour format: hh should be 0-11
      hh <- ifelse(hh >= 12, hh - 12, hh)
    }
    mm <- as.numeric(time_vec[2])
    if (mm >= 60) {
      warning(simpleWarning("Error in input time."))
      return(NA)
    }
    if (time_len == 3) {
      ss <- as.numeric(time_vec[3])
      if (ss >= 60) {
        warning(simpleWarning("Error in input time."))
        return(NA)
      }
    } else {
      ss <- 0
    }
  }
  sprintf("%02d:%02d:%02d", hh, mm, ss)
}
#' Recognise date-time variables when month and year are known (otherwise there
#' is no way to differentiate date and month)
#' @inheritParams recog_one_date
#' @param dateTime A vector of strings indicating the date and time that satisfy
#'   the assumptions specified in the Details section.
#' @param time_dummy A dummy time assigned to date-time variables when time is
#'   not available. Default is 11:11:11.
#' @details Assumptions made on the format of input time are:
#' (i) only "/" or "-" is used to separate date, month and year, and no mixture
#'  of the two symbols in a single time variable;
#' (ii) only ":" is used to separate hour, minute and second, and the order is
#'   always in "h:m:s" (second can be missing, but not minute or hour); and
#' (iii) a dateTime variable should contain both date and time.
#' @return Returns a string vector of the format %d/%m/%Y %H:%M:%S.
#' @examples
#' recogniseDateTime(dateTime = c("02/04/20 1:23", "19/02/20 3:12 PM",
#'                                "11/02/2020 12:11:13", "02/04/2020"),
#'                   month = 2, year = 2020)
#' @author Yilin Ning, Ying Chen
#' @export
recogniseDateTime <- function(dateTime, month, year, time_dummy = "11:11:11") {
  if (is.null(dateTime)) stop(simpleError("Please specify dateTime"))
  dateTime <- as.character(dateTime)
  if (anyNA(dateTime)) warning(simpleWarning("There is NA in input dateTime."))
  month <- as.numeric(month)
  year <- as.numeric(year)
  if (is.null(month) || is.na(month)) {
    stop(simpleError("Please specify month as an integer."))
  }
  if (is.null(year) || is.na(year)) {
    stop(simpleError("Please specify year as a 4-digit integer."))
  }
  date_time_vec <- sapply(dateTime, function(d) {
    d_vec <- unlist(strsplit(d, split = " "))
    d_vec <- as.character(d_vec)
    date <- recog_one_date(date = d_vec[1], month = month, year = year)
    if (length(d_vec) == 1) {
      warning(simpleWarning(paste("Time not available. Replaced by", time_dummy)))
      time <- time_dummy
    } else {
      time <- recog_one_time(time = d_vec[-1])
    }
    paste(date, time)
  })
  names(date_time_vec) <- NULL
  date_time_vec
}
#' Recognise dates
#' @description An alias to \code{recogniseDateTime} when time is not available.
#'   Uses the default \code{time_dummy} parameter in \code{recogniseDateTime}.
#' @describeIn recogniseDateTime
#' @inheritParams recog_one_date
#' @param date a string vector of dates.
#' @examples
#' recogniseDate(c("02/04/2020", "04/18/20"), month = 4, year = 2020)
#' @export
recogniseDate <- function(date, month, year) {
  recogniseDateTime(dateTime = date, month = month, year = year)
}
#' @title Format date variables using \code{\link{recogniseDateTime}}
#'
#' @description Format date variable from character to R class \dQuote{POSIXct}
#'   so that we can manipulate with date variables
#'
#' @param dat  A \emph{data.frame}, has at least one key variable:
#'   \itemize{
#'  \item{RESULT.DATE} an object of classes have information calender date and times
#'  }
#' @param yy A numeric value specifies the year of the data set.
#' @param mm A numeric value specifies the month of the data set.
#'
#' @return A \emph{data.table}, has at least one key variable:
#' \itemize{
#' \item{RESULT.DATE} an object of classes \dQuote{POSIXlt} and \dQuote{POSIXct}
#'   representing calender date and times.
#' }
#' @examples
#' # Load example data
#' data("gluDat")
#' head(gluDat)
#' gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
#' head(gluDat2)
#' @author Ying Chen
#' @import data.table
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
