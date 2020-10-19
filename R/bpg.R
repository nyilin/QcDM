
#' @title Batch process for glucometrics
#' @description Internal function
#' @param dat, as in \code{\link{GenGluM}}.
#' @param span.months, as in \code{\link{GenGluM}}.
#' @param hypocutoffs, as in \code{\link{GenGluM}}.
#' @param hypercutoffs, as in \code{\link{GenGluM}}.
#' @param normalrange, as in \code{\link{GenGluM}}.
#' @author Ying Chen, Yilin Ning
#' @export
bpg <- function(dat, span.months = 1, # this is the time span you can choose, 1 month span means monthly report,
                                     # 3 means quarterly, 6 means half-annually.
                hypocutoffs, hypercutoffs,normalrange){
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
      metricList[[length(cutoff)*(i-1)+j]] = GenGluM(dat.each, hypocutoffs, hypercutoffs, normalrange) # it's ordered as ward1 month1 ward1 month2....
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

