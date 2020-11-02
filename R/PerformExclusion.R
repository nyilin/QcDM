#' @title Apply exclusion criterion to data
#' @description By default, there are three criterion, age criterion, duration
#'   criterion, and frequency criterion.
#' @param preDat A \emph{data.table} prepared by \code{\link{GenEpisode}}.
#' @param crtVec A boolean vector of length 3, indicating whether to exclude
#'   patients by length of stay, frequency of BG measurements, and whether to
#'   exclude the readings taken within the first 24 hours.
#' @param ageRange A vector specifying the age range. Any patient younger than
#'   the minimum age or older than the maximum age should be excluded. This
#'   exclusion criterion requires the data to have a BIRTH.DATE column or an AGE
#'   column, otherwise it will be ignored.
#' @param losNum_hour A number specifying the length of stay criterion. A
#'   hospital stay need to be greater than this value to be included in the
#'   analysis. This is an optional exclusion criterion.
#' @param freqNum A numeric value specifying the smallest frequency of BG within
#'   a hospital stay needed for the analysis. This is an optional criterion.
#' @return Returns a list with a vector of exclusion criteria and the
#'   \emph{data.table} after applying exclusion criteria.
#' @examples
#' # First prepare data using GenEpisode:
#' data("gluDat")
#' gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
#' gluDat3_ls <- DataScrubbing(dat = gluDat2, unitVal = 1)
#' gluDat4 <- GenEpisode(dat = gluDat3_ls$dat, epiMethod = "Admininfo")
#' # Create an "AGE" column with all values assigned to "AGE" to indicate that age
#' # is not available in this data:
#' gluDat4$AGE <- "AGE"
#' # Then apply exclusion criteria:
#' exlList <- PerformExclusion(preDat = gluDat4)
#' exlList
#' @author Ying Chen
#' @export
PerformExclusion <- function(preDat, crtVec = c(crt.los = TRUE, crt.freq = TRUE,
                                                crt.1stday = TRUE),
                             ageRange = c(16, 120),
                             losNum_hour = 24,
                             freqNum = 5) {
  # Follow the exclusion criterion
  # so crtVec is an logic vector where each entry tells whether to perform the
  # criteria
  # entry 1: duration criteria
  # entry 2: frequency criteria
  #
  # crt.1stday works differently in the sense that it does not exclude patients
  # episodes, but only exclude certain readings from each patient episode. As a
  # result it is performed last, and does not have a number of patients
  # excluded in the returned vector.

  if (nrow(preDat) == 0) {
    return(NULL) # check if there is data to check
  }
  # Store number of patients excluded according to each criterion
  crt.los = NULL
  crt.freq = NULL
  crt.1stday = NULL
  preDat[, NO.OF.READINGS := .N, by = list(ADMISSION.ID, EPISODE.ID)]
  # Total number of patient-episodes:
  total <- nrow(unique(preDat[, c("ADMISSION.ID")]))

  # I.a) First exclusion: remove patients out of ageRange
  #
  # Only apply this criteria if we have valid BIRTH.DATE or AGE values, i.e. if
  # we have any entry in BIRTH.DATE column that is not equal to `BIRTH.DATE`, or
  # any entry in AGE column that is not equal to `AGE`.
  #
  # Otherwise we will remove AGE column and ignore this criterion.
  if (!all(preDat$AGE == "AGE")) {
    # If there is valid AGE column, make sure it is numeric
    preDat[, AGE := as.numeric(AGE)]
  } else {
    # Otherwise, try to compute AGE from BIRTH.DATE
    if (!all(preDat$BIRTH.DATE == "BIRTH.DATE")) {
      # If there is valid birth date, compute age
      preDat[, AGE := (difftime(
        as.Date(RESULT.DATE, format = "%m-%d-%y"),
        as.Date(BIRTH.DATE),
        units = "days"
      ) / 365)]
    } else {
      # Remove AGE column
      preDat[, AGE := NULL]
    }
  }
  # Exclusion patients out of age range
  rowsExlAge <- c()
  if (is.null(preDat$AGE)) {
    crt.age <- NA
  } else if (sum(!is.na(preDat$AGE)) > 0) {
    # Rows to exclude by age
    rowsExlAge <- which((preDat$AGE < min(ageRange)) |
                       (preDat$AGE > max(ageRange)))
    crt.age <- nrow(unique(preDat[rowsExlAge, c("ADMISSION.ID","EPISODE.ID"),
                                  with = FALSE]))
  } else {
    crt.age <- NA
  }

  # I.b) Second exclusion: Remove those patient episosdes with only 24 hours LOS
  rowsExlLOS <- c()
  if (crtVec[1]) {
    preDat[, DURATION := (diff(range(as.double(.SD$RESULT.DATE))) / 3600),
                by = list(ADMISSION.ID, EPISODE.ID)]
    # Rows to exclude by LOS
    rowsExlLOS <- which(preDat$DURATION <= losNum_hour)
    crt.los <- nrow(unique(preDat[rowsExlLOS, c("ADMISSION.ID","EPISODE.ID"),
                                  with = FALSE]))
  }

  # I.c) Third exclusion: Remove those patient episodes with less than 5 BG
  # measurements
  rowsExlFreq <- c()
  if (crtVec[2]) {
    # Rows to exclude with less than 5 readings
    rowsExlFreq <- which(preDat$NO.OF.READINGS < freqNum)
    crt.freq <- nrow(unique(preDat[rowsExlFreq, c("ADMISSION.ID","EPISODE.ID"),
                                   with = FALSE]))

  }

  exlM <- c(crt.age = crt.age, crt.los = crt.los, crt.freq = crt.freq,
            totalcases = total)
  # All the rows excluded by 3 criteria
  rowsExl <- unique(c(rowsExlAge, rowsExlLOS, rowsExlFreq))
  if (length(rowsExl) != 0) {
    preDat <- preDat[-rowsExl, ]
  }
  if (crtVec[3]) {
    # version 2 means remove the first 24 hours results from a patient stay
   preDat <- preDat[preDat$LOS.PSUM > 24, ]
  }

  out <- list(exlM, preDat)
  return(out)

}
