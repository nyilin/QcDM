#' @title Recognise date
#'
#' @description Recognise the date component of a time variable
#'
#' @param date a single time point satisfying the assumptions, see details.
#' @param month a number (1 to 12) indicating the month this time point is in
#' @param year a number (00 to 99) indicating the year this time point is in
#'
#' @details none
#' @return Returns a character variable of the format %d/%m/%y %H:%M:%S
#'
#' @author Yilin Ning, Ying Chen
#' @export
recogniseDate <- function(date, month, year) {
  if (length(grep(pattern = "/", date)) > 0) {
    dateVec <- as.numeric(unlist(strsplit(date, "/")))
  } else {
    dateVec <- as.numeric(unlist(strsplit(date, "-")))
  }
  if (sum(nchar(dateVec) > 2) != 0) {
    yyInd <- which(nchar(dateVec) > 2)
    if (as.numeric(substr(dateVec[yyInd], 3, 4)) != year) {
      return(NA)
    }
  } else {
    yyInd <- which(dateVec == year)[1]
    if (dateVec[yyInd] != year) {
      return(NA)
    }
  }
  dateVec <- dateVec[-yyInd]
  if (!is.element(month, dateVec)) {
    return(NA)
  }
  mmInd <- which(dateVec == month)[1]
  dd <- dateVec[-mmInd]
  paste(dd, month, year, sep = "/")
}
