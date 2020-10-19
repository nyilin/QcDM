#' @title Recognise date and time
#'
#' @description Recognise date and time when month and year are known (otherwise
#'   there is no way to differentiate date and month)
#'
#' @param dateTime A single time point satisfying the assumptions, see details.
#' @param month A number (1 to 12) indicating the month this time point is in
#' @param year A number (00 to 99) indicating the year this time point is in
#'
#' @details  Assumptions made on the format of input time are:
#' - only "/" or "-" is used to separate date, month and year, and no mixture
#'  of the two symbols in a single time variable
#' - only ":" is used to separate hour, minute and second, and the order is
#'   always in "h:m:s" (second can be missing, but not minute or hour)
#' - a dateTime variable should contain both date and time
#'
#' @return Returns a character variable of the format %d/%m/%y %H:%M:%S
#'
#' @author Yilin Ning, Ying Chen
#' @export
recogniseDateTime <- function(dateTime, month, year) {
  date <- unlist(strsplit(dateTime, " "))[1]
  time <- unlist(strsplit(dateTime, " "))[-1]
  tmp <- time
  time <- NULL
  for (i in 1:length(tmp)) {
    time <- paste(time, tmp[i])
  }

  date <- recogniseDate(date, month, year)
  if (is.na(date)) {
    return(NA)
  }

  timeVec <- unlist(strsplit(time, ":"))
  timeLen <- length(timeVec)
  hour <- as.numeric(timeVec[1])

  if (timeLen == 2) {
    min <- as.numeric(unlist(strsplit(timeVec[2], " "))[1])

    if (length(grep(pattern = "am", tolower(timeVec[timeLen]))) > 0) {
      if (hour >= 12) {
        hour <- hour - 12
      } else{
        hour <- hour
      }
    } else if (length(grep(pattern = "pm", tolower(timeVec[timeLen]))) > 0) {
      if (hour + 12 >= 24) {
        hour <- hour
      } else {
        hour <- hour + 12
      }
    }
    time <- sprintf("%02d:%02d:%02d", hour, min, 0)
  } else if (timeLen == 3) {
    min <- as.numeric(timeVec[2])
    second <- as.numeric(unlist(strsplit(timeVec[3], " "))[1])
    if (length(grep(pattern = "am", tolower(timeVec[timeLen]))) > 0) {
      if (hour >= 12) {
        hour <- hour - 12
      } else {
        hour <- hour
      }
    }
    if (length(grep(pattern = "pm", tolower(timeVec[timeLen]))) > 0) {
      if (hour >= 12) {
        hour <- hour
      } else {
        hour <- hour + 12
      }
    }
    time <- sprintf("%02d:%02d:%02d", hour, min, second)
  } else {
    return(NA)
  }
  paste(date, time)
}




