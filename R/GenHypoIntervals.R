
# whether first repeated capillary measurement is still hypo:
# 0 means yes, 1 mean no

#' @title Calculate hypoglycemia management time intervals
#'
#' @description There are three types of intervals: hypo to next documented BG,
#'   hypo to resolution of hypo, and resolution of hypo to next documented hypo.
#'   This function will calculate these intervals.
#' @param dat A \emph{data.table} prepared by \code{\link{GenEpisode}}.
#' @return A \emph{list} containing the time intervals.
#' @examples
#' # First prepare example data using GenEpisode:
#' data("gluDat")
#' gluDat2 <- FormatDate(dat = gluDat, yy = 2020, mm = 7)
#' gluDat3 <- GenEpisode(dat = gluDat2, epiMethod = "Admininfo")
#' # Then generate hypo intervals:
#' GenHypoIntervals(dat = gluDat3)
#' @author Ying Chen
#' @export
GenHypoIntervals <- function(dat) {
  dat[, hypocounts := sum(RESULT.MEAN < 4),
      by = list(ADMISSION.ID,EPISODE.ID)] # count hypo by episodes
  dat.hypo = dat[dat$hypocounts > 0, ] # use only those episodes with hypo
  dat.hypo[, hypo := as.numeric(RESULT.MEAN < 4)] # recode BG as hypo or non-hypo
  dat.hypo[, HypoNext := ifelse(hypo == 0,  -1,
                                .SD[(.I-min(.I)+ 2),]$LOS.EACH*60),
           by = list(ADMISSION.ID,EPISODE.ID)] # calculate the time interval between hypo and next BG
  dat.hypo[, change := c(0,diff(hypo)),
           by =list(ADMISSION.ID,EPISODE.ID)] # code it as -1 if there is a change from hypo to non-hypo, i.e., resolution.


  dat.hypo[, epi:= .SD[, rep(1:(length(.I[change == 1]) + 1),
                             diff(c(1, .I[change == 1] - min(.I) + 1, .N +
                                      1)))-1],
           by = list(ADMISSION.ID,EPISODE.ID)] # count hypo episodes, as long as a hypo is preceded by either no BG or a non-hypo BG, it will be coded as a new hypo episode

  dat.hypo = dat.hypo[dat.hypo$epi >= 1,] # extract hypo episodes from inpatient episodes

  dat.hypo[, reso := ifelse(any(change == (-1)), 1, 0),
           by = list(ADMISSION.ID,EPISODE.ID, epi)] # check if there is a resolution to non-hypo in each hypo episode
  dat.hypo2 = dat.hypo[dat.hypo$reso == 1,]
  dat.hypo2[, HypoResoID := ifelse(hypo == 0, -1, min(.I[hypo == 0])-min(.I)),
            by = list(ADMISSION.ID,EPISODE.ID, epi)] # find the number of BG needed to resoultion, starts from 1, i.e., immediate resolution
  dat.hypo2[, HypoReso := ifelse(hypo == 0, -1, (.SD[(min(.I[hypo == 0])-min(.I)+1),]$LOS.PSUM-LOS.PSUM)*60),
            by = list(ADMISSION.ID,EPISODE.ID, epi)] # calculate the time interval between resolution and hypo.
  dat.hypo2[, ResoNext := ifelse(hypo == 0, -1, (.SD[(min(.I[hypo == 0])-min(.I)+2),]$LOS.PSUM-.SD[(min(.I[hypo == 0])-min(.I)+1),]$LOS.PSUM)*60),
            by = list(ADMISSION.ID,EPISODE.ID, epi)] # calculate the time interval between reosultion of hypo to next BG


  HypoNextInt<- dat.hypo[dat.hypo$HypoNext != -1,]$HypoNext
  HypoResoInt <- dat.hypo2[dat.hypo2$HypoReso != -1,]$HypoReso
  ResoNextInt <- dat.hypo2[dat.hypo2$ResoNext != -1,]$ResoNext
  out <- list(HypoNextInt, HypoResoInt, ResoNextInt)
  names(out) <- c("HypoNextInt","HypoResoInt", "ResoNextInt")
  return(out)

  # id = which(dat$RESULT_MEAN < 4)
  # tmpdat = dat[id, ]
  # id1 = which(!is.na(match(
  #   dat$PATIENT_NO, unique(tmpdat$PATIENT_NO)
  # )))
  # tmpttdat = dat[id1, ]
  # new_dat = tmpttdat[, list(
  #   RESULT_DATE_Bef = .SD[which(.SD$RESULT_MEAN < 4), ]$RESULT_DATE,
  #   RESULT_MEAN_Bef = .SD[which(.SD$RESULT_MEAN < 4), ]$RESULT_MEAN,
  #   RESULT_DATE_Fst = .SD[which(.SD$RESULT_MEAN < 4) + 1, ]$RESULT_DATE,
  #   RESULT_MEAN_Fst = .SD[which(.SD$RESULT_MEAN < 4) + 1, ]$RESULT_MEAN,
  #   RESULT_DATE_Snd = .SD[which(.SD$RESULT_MEAN < 4) + 2, ]$RESULT_DATE,
  #   RESULT_MEAN_Snd = .SD[which(.SD$RESULT_MEAN < 4) + 2, ]$RESULT_MEAN,
  #   FstT = (as.double(.SD[which(.SD$RESULT_MEAN < 4) + 1, ]$RESULT_DATE)
  #           - as.double(.SD[RESULT_MEAN < 4, ]$RESULT_DATE)) / 60,
  #   SndT = (as.double(.SD[which(.SD$RESULT_MEAN < 4) + 2, ]$RESULT_DATE)
  #           - as.double(.SD[which(.SD$RESULT_MEAN < 4) + 1, ]$RESULT_DATE)) /
  #     60
  # ),
  # by = list(LOCATION, PATIENT_NO, EPISODE_ID)]
  # new_dat[, vld1 := ifelse(.SD$RESULT_MEAN_Fst < 4, 0, 1)]
  # return(new_dat)
}




