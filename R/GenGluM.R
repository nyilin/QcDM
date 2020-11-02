#' @title Generate glucometrics
#'
#' @description Generate glucometrics for specific ward/wards during specified
#'   time period
#'
#' @param dat A \emph{data.table} prepared by \code{\link{GenEpisode}}.
#' @param hypocutoffs A vector of numeric values indicating the recommended
#'   cutoffs for hypoglycemia, from mild, moderate to severe hypoglycemia.
#'   Hypoglycemia is defined as less than the specified cutoff value.
#' @param  hypercutoffs A vector of numeric values indicating the recommended
#'   cutoffs for hyperglycemia, from mild, moderate to severe hyperglycemia.
#'   Hyperglycemia is defined as no less than the specified cutoff value.
#' @param normalrange A vector of numeric values indicating the recommended range
#'   for normal glycemia, where the first value is the lower range, and the
#'   second value is the upper range. A BG is considered in recommended range if
#'   it is no less than a lower bound and less than a upper bound at the same
#'   time.
#' @param hgicutoff A numeric value indicating the cutoff used for calculating
#'   hyperglycemic index (HGI).
#' @param unitVal A unit indicator. 1 stands for mmol/L, 2 stands for md/dL
#'
#' @return Returns a list of three data.frames:
#' \itemize{
#'   \item{\code{popstat}:} A vector of all the indices at population level
#'   \item{\code{patientdaystat}:} A vector of all the indices at patient day level
#'   \item{\code{peradmissionstat}:} A vector of all the indices at admission level
#' }
#' which will be used as input to generate glucometrics tables.
#' @examples
#' # First prepare example data using GenEpisode:
#' data("gluDat")
#' gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
#' gluDat3_ls <- DataScrubbing(dat = gluDat2, unitVal = 1)
#' gluDat4 <- GenEpisode(dat = gluDat3_ls$dat, epiMethod = "Admininfo")
#' # Then generate glucometrics:
#' metricList <- GenGluM(dat = gluDat4, hypocutoffs = c(4, 3, 2.5),
#'                       hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
#'                       hgicutoff = 10, unitVal = 1)
#' # View glucometrics (round to 1 decimal place):
#' lapply(metricList, function(m) round(m, 1))
#' @seealso \code{\link{ProGluTable}}
#' @author Ying Chen, Chuen Seng Tan
#' @import data.table
#' @export
## hyper modified to non-strict inequality
## normal range modified to strict inequality on the right side
GenGluM <- function(dat, hypocutoffs, hypercutoffs, normalrange, hgicutoff,
                    unitVal) {
  dat[, RESULT.DATE1 := as.POSIXct(substring(RESULT.DATE, 1, 10),
                                   format = "%Y-%m-%d")]# hours and minutes removed
  dat[, DAY := floor((as.double(.SD$RESULT.DATE1) -
                        as.double(min(.SD$RESULT.DATE1))) / 86400) + 1,
      by = list(LOCATION, ADMISSION.ID, EPISODE.ID)]
  # DAY: report the days of records per patient per admission
  dat[, DOT := floor((as.double(.SD$RESULT.DATE1) -
                        as.double(min(.SD$RESULT.DATE1))) / 86400) + 1]
  # DOT: report the days of all the records
  dat[, RESULT.DATE1 := NULL]
  ##void RESULT.DATE1
  setkey(dat, LOCATION, ADMISSION.ID,EPISODE.ID, DAY, DOT, RESULT.DATE)
  jind <- ifelse(unitVal==1, 0.324, 0.001)
  #  Harry's code to compute HGI
  computeHGI <- function(DT, cutoff) {
    # Generate dummy data table with left and right points

    if(nrow(DT) < 2){
      DT.hgi <- DT[, list(
        LOS.PSUM.L = LOS.PSUM[1],
        LOS.PSUM = LOS.PSUM,
        LOS.PSUM.R = LOS.PSUM[.N],
        LOS.PSUM.CUT.L = 0,
        LOS.PSUM.M = 0,
        LOS.PSUM.CUT.R = 0,
        RESULT.MEAN.L = cutoff,
        RESULT.MEAN = RESULT.MEAN,
        RESULT.MEAN.R =  cutoff,
        RESULT.AREA.L = 0,
        RESULT.AREA.M = 0,
        RESULT.AREA.R = 0
      )]

    }else{
      DT.hgi <- DT[, list(
        LOS.PSUM.L = c(LOS.PSUM[1], LOS.PSUM[1:(.N - 1)]), # This requires DT to have at least 2 readings
        LOS.PSUM = LOS.PSUM,
        LOS.PSUM.R = c(LOS.PSUM[2:.N], LOS.PSUM[.N]),
        LOS.PSUM.CUT.L = 0,
        LOS.PSUM.M = 0,
        LOS.PSUM.CUT.R = 0,
        RESULT.MEAN.L = c(cutoff, RESULT.MEAN[1:(.N - 1)]),
        RESULT.MEAN = RESULT.MEAN,
        RESULT.MEAN.R = c(RESULT.MEAN[2:.N], cutoff),
        RESULT.AREA.L = 0,
        RESULT.AREA.M = 0,
        RESULT.AREA.R = 0
      )]
    }



    # Find area of all left triangles
    DT.hgi[(RESULT.MEAN > cutoff) & (RESULT.MEAN.L <= cutoff),
           LOS.PSUM.CUT.L := (LOS.PSUM + ((LOS.PSUM.L - LOS.PSUM) *
                                            (RESULT.MEAN - cutoff) /
                                            (RESULT.MEAN - RESULT.MEAN.L)
           ))]
    DT.hgi[(RESULT.MEAN > cutoff) & (RESULT.MEAN.L <= cutoff),
           RESULT.AREA.L := (0.5 * (LOS.PSUM - LOS.PSUM.CUT.L) *
                               (RESULT.MEAN - cutoff))]

    # Find area of all right triangles
    DT.hgi[(RESULT.MEAN > cutoff) & (RESULT.MEAN.R <= cutoff),
           LOS.PSUM.CUT.R := (LOS.PSUM + ((LOS.PSUM.R - LOS.PSUM) *
                                            (RESULT.MEAN - cutoff) /
                                            (RESULT.MEAN - RESULT.MEAN.R)
           ))]
    DT.hgi[(RESULT.MEAN > cutoff) & (RESULT.MEAN.R <= cutoff),
           RESULT.AREA.R := (0.5 * (LOS.PSUM.CUT.R - LOS.PSUM) *
                               (RESULT.MEAN - cutoff))]

    # Find area of all trapezoids
    DT.hgi[(RESULT.MEAN > cutoff) & (RESULT.MEAN.R > cutoff),
           RESULT.AREA.M := (0.5 * ((RESULT.MEAN.R - cutoff) +
                                      (RESULT.MEAN - cutoff)) *
                               (LOS.PSUM.R - LOS.PSUM))]

    # Compute total over the range (cumulative)
    DT.hgi[RESULT.MEAN > cutoff, LOS.PSUM.M := LOS.PSUM]
    DT.hgi[, RESULT.AREA := (RESULT.AREA.L + RESULT.AREA.M + RESULT.AREA.R)]
    DT.hgi[, LOS.PSUM.MIN := min(LOS.PSUM.CUT.L, LOS.PSUM.M, LOS.PSUM.CUT.R)]
    DT.hgi[, LOS.PSUM.MAX := max(LOS.PSUM.CUT.L, LOS.PSUM.M, LOS.PSUM.CUT.R)]
    DT.hgi[, hgi := (cumsum(RESULT.AREA) / (cummax(LOS.PSUM.MAX) -
                                              cummin(LOS.PSUM.MIN)))]
    return(DT.hgi[, ifelse(is.nan(hgi), 0, hgi)])
  }

  dat.oth <- dat[, list(hgi = max(computeHGI(.SD, hgicutoff))),
                 by = list(ADMISSION.ID,EPISODE.ID)]

  # Generate Hyper, Hypo, mean, sd, J-index variables
  dat.PatAdmDay <- dat[, list(
    FreqReading = .N,
    FreqHypo1st = .SD[RESULT.MEAN < hypocutoffs[1], .N],
    FreqHypo2nd = .SD[RESULT.MEAN < hypocutoffs[2], .N],
    FreqHypo3rd = .SD[RESULT.MEAN < hypocutoffs[3], .N],
    FreqHyper1st = .SD[RESULT.MEAN >= hypercutoffs[1], .N],
    FreqHyper2nd = .SD[RESULT.MEAN >= hypercutoffs[2], .N],
    FreqHyper3rd = .SD[RESULT.MEAN >= hypercutoffs[3], .N],
    NormalRange = .SD[(RESULT.MEAN >= normalrange[1]) &
                        (RESULT.MEAN < normalrange[2]), .N],
    RecurHypo = ifelse(sum(RESULT.MEAN < hypocutoffs[1]) <= 1, 0,
                       ifelse(diff(.SD[RESULT.MEAN < hypocutoffs[1], ]$LOS.PSUM)*60 > 10 &(
                         diff(.SD[RESULT.MEAN < hypocutoffs[1], ]$LOS.PSUM)*60 <= 240
                       ), 1, 0)),
    m = mean(.SD$RESULT.MEAN),
    s = sd(.SD$RESULT.MEAN)
  ) , by = list(ADMISSION.ID,EPISODE.ID, DAY)]


  dat.PatAdmDay[, `:=`(jindex = (jind * (m + s) ^ 2),
                       CV = s / m)]

  # Population level
  tmp = dat.PatAdmDay

  popstat = colSums(tmp[, 4:11, with = FALSE])[c(2:8, 1)]
  popMedian = median(dat$RESULT.MEAN)
  popMean = mean(dat$RESULT.MEAN)
  popSD = sd(dat$RESULT.MEAN)
  popJindex = jind * (popMean + popSD) ^ 2
  popstat = c(popstat,
              popMedian = popMedian,
              popMean = popMean,
              popSD = popSD,
              popJindex = popJindex)


  # Patient Day level
  tmp = dat.PatAdmDay
  pdstatp1 = colSums(tmp[, 4:10, with = FALSE] > 0)[c(2:7, 1)]
  pdNR = sum(tmp$m >= normalrange[1] &
               (tmp$m < normalrange[2])) # count of mean BG within normal range

  Vec = cbind(tmp$m, tmp$s, tmp$jindex)
  pdstatp2 = matrix(sapply(1:3,function(x){
    out <-c(median(Vec[,x], na.rm = TRUE),quantile(Vec[,x], 0.25, na.rm = TRUE),
            quantile(Vec[,x], 0.75, na.rm = TRUE),
            mean(Vec[,x], na.rm = TRUE), sd(Vec[,x], na.rm = TRUE))
    return(out)
  }),1,15,byrow=FALSE)
  Names <- c("M","SD","J")
  names2 <- matrix(sapply(1:3, function(x){
    out <- c(paste0("pd",Names[x],"median"),
             paste0("pd",Names[x],"1Q"),
             paste0("pd",Names[x],"3Q"),
             paste0("pd",Names[x],"mean"),
             paste0("pd",Names[x],"sd"))
    return(out)

  }),1,15,byrow = FALSE)
  NAinsd = length(which(is.na(tmp$s)))

  pdstat = c(pdstatp1[1:6],  pdNR, pdstatp1[7],
      pdstatp2,
       NAinsd
    )
  names(pdstat) <- c(names(pdstatp1)[1:6],"NormalRange",names(pdstatp1)[7],names2,"NAinSD")
  #Admission Level
  tmp <-
    unique(dat.PatAdmDay[, list(
      Day = max(DAY),
      FreqReading = sum(FreqReading),
      FreqHypo1st = sum(FreqHypo1st),
      FreqHypo2nd = sum(FreqHypo2nd),
      FreqHypo3rd = sum(FreqHypo3rd),
      FreqHyper1st = sum(FreqHyper1st),
      FreqHyper2nd = sum(FreqHyper2nd),
      FreqHyper3rd = sum(FreqHyper3rd),
      RecurHypo = sum(RecurHypo),
      NormalRange = sum(NormalRange)
      ),
    by = list(ADMISSION.ID,EPISODE.ID)])
  epistatp1_tmp = colSums(tmp[, 4:12, with = FALSE] > 0)[c(2:9, 1)]
    # tmp <-
  #   dat.PatAdmDay[, list(newadhypo4 = .SD[FreqHypo1st > 1, .N], newadhyper20 =
  #                          .SD[FreqHyper2nd > 1, .N]), by = list(LOCATION, PATIENT.NO, EPISODE.ID)]
  # newad = colSums(tmp[, 4:5, with = FALSE] > 0)

  tmp = unique(dat[, list(m = mean(RESULT.MEAN),
                   s = sd(RESULT.MEAN)),
            by = list(ADMISSION.ID,EPISODE.ID)], by = NULL)
  pNR <- sum(tmp$m >= normalrange[1] &
               (tmp$m < normalrange[2]))

  epistatp1 = c(epistatp1_tmp[1:7],  pNR, epistatp1_tmp[9])
  names(epistatp1) <- c(names(epistatp1_tmp)[1:7],"NormalRange",names(epistatp1_tmp)[9])
  tmp[, `:=`(jindex = (jind * (m + s) ^ 2))]
  tmp2 = dat.PatAdmDay[, list(wm = mean(m), ws = sd(m)), by = list(ADMISSION.ID,EPISODE.ID)]
  tmp2[, `:=`(wjindex = (jind * (wm + ws) ^ 2))]


  Vec = cbind(tmp$m, tmp$s, tmp$jindex, tmp2$wm, dat.oth$hgi)
  epistatp2 = matrix(sapply(1:5,function(x){
    out <-c(median(Vec[,x], na.rm = TRUE),quantile(Vec[,x], 0.25, na.rm = TRUE),
            quantile(Vec[,x], 0.75, na.rm = TRUE),
            mean(Vec[,x], na.rm = TRUE), sd(Vec[,x], na.rm = TRUE))
    return(out)
  }),1,25,byrow=FALSE)
  Names <- c("M","SD","J","W","hgi")
  names2 <- matrix(sapply(1:5, function(x){
    out <- c(paste0("epi",Names[x],"median"),
             paste0("epi",Names[x],"1Q"),
             paste0("epi",Names[x],"3Q"),
             paste0("epi",Names[x],"mean"),
             paste0("epi",Names[x],"sd"))
    return(out)

  }),1,25,byrow = FALSE)


  id = which(is.na(tmp$s))
  NAinSD = length(id)


  # id = which(is.na(tmp$ws))
  # NAinWSD = length(id)
  # epiWSmed = median(tmp$ws, na.rm = TRUE)
  # epiWSmean = mean(tmp$ws, na.rm = TRUE)
  # epiWJmed = median(tmp$test.jindex, na.rm = TRUE)
  # epiWJmean = mean(tmp$test.jindex, na.rm = TRUE)


  epistat <- c(epistatp1,
    epistatp2,
    NAinSD = NAinSD
    # epiWSmed = epiWSmed,
    # epiWSmean = epiWSmean,
    # epiWJmed = epiWJmed,
    # epiWJmean = epiWJmean,
    # NAinWSD = NAinWSD,

  )
  names(epistat) <- c(names(epistatp1),names2,"NAinSD")
  out <- list(popstat, pdstat, epistat)
  names(out) <- c("PopStat", "PatDayStat", "PatEpiStat")
  return(out)
}

#' @title Batch process for glucometrics
#' @description Internal function
#' @inheritParams GenGluM
#' @param span.months The number of months to aggregate by in the report, e.g.,
#'   \code{span.months = 1} for monthly report, \code{span.months = 3} for
#'   quarterly report and \code{span.months = 6} for half-annually report.
#' @return Returns a list, where each item is an output from \code{\link{GenGluM}}.
#' @seealso \code{\link{GenGluM}}
#' @author Ying Chen, Yilin Ning
bpg <- function(dat, span.months = 1, hypocutoffs, hypercutoffs, normalrange,
                hgicutoff, unitVal){
  # Batch process if individual ward results wanted
  dat$Mon = format(dat$RESULT.DATE, "%Y%m") # A combination of YYYYMM gives the correct month.
  months = sort(unique(dat$Mon))
  if(length(months)%%span.months != 0 |(length(months) <= span.months)){
    warning("You have less months than your specified span.months or the multiples of your specified span.months. Suggest try a smaller span.months.")
  }
  cutoff =  union(which((1:length(months))%%span.months == 0),length(months))

  location = sort(unique(dat$LOCATION))
  metricList = list()
  for(i in 1:length(location)){
    for(j in 1:length(cutoff)){
      id = which(dat$LOCATION == location[i] &(dat$Mon <= months[cutoff[j]]))
      dat.each = dat[id,]
      dat = dat[-id, ]
      # Generate glucometrics
      metricList[[length(cutoff)*(i-1)+j]] = GenGluM(dat = dat.each, hypocutoffs = hypocutoffs, hypercutoffs = hypercutoffs, normalrange = normalrange, hgicutoff = hgicutoff, unitVal = unitVal) # it's ordered as ward1 month1 ward1 month2....
    }
  }
  return(metricList)
}


# Line Plot of Glucometrics for individual ward of specified span of time.
# From params are a vector of 3 digits numbers,
# First digit: 1,2,3 represent population, patient-day or patient-episode
# Second digit: 1,2,3, represent  hyper to glycemia variability to hypo
# Last digit
# Hyperglycemia: 1-3 to mild, moderate and sever hyper, 4-5 median or mean,
#        6 for normal range, for patient-episode, there are  two extra ones,
#        7 for weighted mean, 8-9 for hgi median and mean
# Glycemia variability: 1-2 for SD median and mean, 3-4 for J-index median and mean,
#           for population, only single estimates for SD and j-index, 2, and 4 are not used
# Hypoglycemia: 1-3 for mild, moderate and severe hypo, for patient-episode, there is an 4 for recurrent hypo rates
# location gives the ward names that correspond to the metricList and
# lpg <- function(metricList, params = c(111,112,113,121,122,123), location, span.months, st, et, fileName = ""){
#   st <- as.Date(start.date)
#   en <- as.Date(end.date)
#   ll <- format(seq(st, en, by = '1 month'),"%Y%m")
#   cutoff =  union(which((1:length(ll))%%span.months == 0),length(ll))
#   metricName <- data.frame(location = rep(location,each = length(cutoff), time = rep(1:length(cutoff),length(location))))
#   mat <- matrix()
# }

