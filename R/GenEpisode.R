#  Read in glucose data and calculate pseudo admission episodes, average the multiple readings at
#      the same time, calculate the time intervals between consecutive readings
#      and accumulated time intervals within each episode for each individual, identify the day in a week,
#      month, year, week number in a year, convert timings into hours using the standard 24-hour clock.

#   Input: data frame with colnames ADMISSION.ID, RESULT.DATE, RESULT


#' @title Calculate pseudo admission episodes
#'
#' @description  Read in glucose data and calculate pseudo admission episodes, calculate the time interval between consecutive readings
#'     and accumulated time intervals within each episode for each individual, identify the day in a week,
#'     month, year, week number in a year, convert timings into hours using the standard 24-hour clock.
#'
#'
#' @param dat A \emph{data.frame}, normally is the outcome of function \emph{cleandata}, at least have three key variables:
#' \itemize{
#'  \item{RESULT.DATE} an object of classes \dQuote{POSIXlt} and \dQuote{POSIXct} representing calender date and times;
#'  \item{ADMISSION.ID} an object of classes "charater" representing the identification number for each hospital stay;
#' \item{RESULT} an object of classes "numeric" representing the Blood Glucose readings.
#' }
#' @param epiMethod This indicates the method takes to compute episodes. 
#' If None, the admission id will be assumed to be case id; 
#' if Pseudo, then episode number will be generated using 48 hours as a cutoff. 
#' 
#' @details none
#' @return A \emph{data.table} object, below are the variables that have been created:
#' \item{LOS.EACH}{time difference between readings}
#' \item{LOS.PSUM}{cumulative time differences between readings}
#' \item{EPISODE.ID}{In the case of pseudo, admission times will be increase by 1 if LOS.EACH greater than 48 hours. Otherwise, it will always be 1, corresponding to that each admission id identifies one hospital stay.}
#' \item{mond}{day of month}
#' \item{weekd}{day of week}
#' \item{yday}{day of year}
#' \item{hour}{numerical hours}
#' \item{weekn}{number of weeks in a year}
#' These variables are created using original key variable \code{RESULT.DATE}.
#'
#' @author Chuen Seng Tan, Ying Chen
#' @export
GenEpisode <- function(dat, epiMethod = "Pseudo" # epivec:
                                                      #   Pseudo generate epi id using 48 hours as cutoff
                                                      #   Each input admission id corresponds to one admission
                            
                            ) {
  dat <- data.table(dat)
  dat <- unique(dat)
  setkey(dat, ADMISSION.ID, RESULT.DATE)
  dat[, LOS.EACH := c(0, diff(as.double(RESULT.DATE))) / 3600, by = list(ADMISSION.ID)] # Compute Time Difference for each observation of each patient
 
  
  if( tolower(epiMethod) == 'pseudo' ){
    # Generate Admission ID based on 48h criteria
    dat[, EPISODE.ID := .SD[, rep(1:(length(.I[LOS.EACH > 48]) + 1),diff(setdiff(c(1, .I[LOS.EACH > 48], .N + 1),0)))],
        by = list(LOCATION, ADMISSION.ID)]
    
    ## treat each patient as one patient-stay
   
  }else{
    
    # if(length(id) > 0){
    #   warning(paste0("We removed ", length(id), " (",round(length(id)/nrow(dat)*100,2),"% ) "," samples without both admission and discharge information."))
    #   dat = dat[-id, ]
    # }
    # 
    # dat[, case.id := paste(, , sep = "|")] # concatenating patient id and admission date and discharge date to create caseid
    # if both admission/discharge date are NA, reomove it
    # setnames(dat, names(dat)[caseidCol], "case.id")
    # 
    # dat[, intermediate:= mean(RESULT.DATE), by = list(ADMISSION.ID, case.id)]
    # dat[, EPISODE.ID := rank(unique(intermediate))[match(intermediate,unique(intermediate))], by = ]
    dat[, EPISODE.ID := 1]
  }

  setkey(dat, LOCATION, ADMISSION.ID, EPISODE.ID, RESULT.DATE)
  dat[, LOS.PSUM := cumsum(LOS.EACH)-.SD[which.min(RESULT.DATE),]$LOS.EACH, by = list(ADMISSION.ID, EPISODE.ID)] # Compute cummulative sum of duration, hence the maximum of it should be the length of stay.

  dat[, RESULT := NULL]

  # dat[, weekd := as.numeric(format(.SD$RESULT.DATE, "%u"))] # give the day of the week in 1 to 7
  # dat[, weekn := as.numeric(format(.SD$RESULT.DATE, "%W"))] # give the week of the year from 1 to 53
  # dat[, mond := as.numeric(format(.SD$RESULT.DATE, "%e"))] # give the day of the month from 01 to 31
  # dat[, yday := as.numeric(format(.SD$RESULT.DATE, "%j"))] # give the day of the year, a number in 1 to 366
  dat$hour = as.numeric(difftime(
    dat$RESULT.DATE,
    paste(substring(dat$RESULT.DATE, 1, 10), "00:00:00", sep = " "),
    units = "hours"
  )) # tranfrom timing to numeric hours
  out <- dat
  return(out)
}
