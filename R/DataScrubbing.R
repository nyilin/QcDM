#' @title Data scrubbing
#' @description If the unit of BG measurement is mmol/L, this function, performs the following steps: 1. "<0.6" or ">33.3" reset to 0.6 and 33.3 respectively
#' 2. "<" or ">" for middle values, decrement or increment by 0.1 3 "Low/lo" or "H/High" reset to limiting values 0.6 and 33.3
#' 4. take the average BG at the same timing for particular patient, this might be system error
#' Report affected proportions of readings.
#' @param dat, the output object of \code{\link{FormatDate}}.
#' @param unitVal, the unit indicator, 1 for mmol/L, 2 for md/dL.
#' @return The function will return a list of three elements. The first element will be the data after data scrubbing. 
#' The second element will be a vector of counts for the situations that decribed in the first three steps taken to clean the data.
#' The last elemenet will report the data containing repeated measurements.
#' @author Ying Chen 
#' @export
DataScrubbing <- function(dat, unitVal){
  # Scrubbing data, should come after formatDate
  # 1. "<0.6" or ">33.3" reset to 0.6 and 33.3 respectively
  # 2. "<" or ">" for middle values, decrement or increment by 0.1
  # 3 "Low/lo" or "H/High" reset to limiting values 0.6 and 33.3 respectively
  # 4. take the average BG at the same timing for particular patient, this might be system error
  # Report affected proportions of readings
  if(unitVal == 1){
    dat$RESULT <- gsub(" ","",dat$RESULT)
    
    idminl <- which(grepl("<0.6", dat$RESULT))
    
    if(length(idminl) > 0){
      dat[idminl, ]$RESULT = "0.6"
    }
    
    
    idmaxl <- which(grepl(">33.3", dat$RESULT))
    
    if( length(idmaxl) > 0){
      dat[idmaxl, ]$RESULT = "33.3"
    }
    
    
    idlt <- which(grepl("<", dat$RESULT))
    if(length(idlt) > 0){
      dat[idlt, ]$RESULT = as.character(as.numeric(gsub("<","", dat[idlt, ]$RESULT)) - 0.1)
    }
    
    
    idgt <- which(grepl(">", dat$RESULT))
    if(length(idgt) > 0){
      dat[idgt, ]$RESULT = as.character(as.numeric(gsub(">","", dat[idgt, ]$RESULT)) + 0.1)
    }
    
    # detect non-numerical values, will be set limiting values
    idlo <- which(grepl("l",tolower(dat$RESULT)))
    if(length(idlo) > 0){
      dat[idlo, ]$RESULT = 0.6
    }
    
    idhi <- which(grepl("h",tolower(dat$RESULT)))
    if(length(idhi) > 0){
      dat[idhi, ]$RESULT = 33.3
    }
    
    # numericalize the readings
    dat$RESULT = as.numeric(dat$RESULT)
    # remove those with NAs
    if(length(which(is.na(dat$RESULT))) > 0){
      warning(paste0("We removed ", length(which(is.na(dat$RESULT))), " (",round(length(which(is.na(dat$RESULT)))/nrow(dat)*100,2),"% ) "," non-numerical values other than involing < and > signs."))
      dat = dat[!is.na(dat$RESULT), ]
      
    }
  }else{
   idminl <- NULL
   idmaxl <- NULL
   idlt <- NULL
   idgt <- NULL
   idlo <- NULL
   idhi <- NULL
  }
  
  l <- c(minLim = length(idminl), maxLim = length(idmaxl), ltSign = length(idlt), gtSign = length(idgt), lo = length(idlo), hi = length(idhi), total = nrow(dat))
  
  duplex <- unique(dat[, list(count = length(unique(RESULT)), Dif = diff(unique(RESULT))), by = list(LOCATION, ADMISSION.ID, RESULT.DATE)])
  d <- list(c(sum(duplex$count > 1), nrow(duplex)), duplex$Dif)
  dat[, RESULT.MEAN := RESULT, by = list(LOCATION, ADMISSION.ID, RESULT.DATE)] # Generate RESULT.MEAN depends on duplicated measurements
  dat = unique(dat)
  out <- list(dat, l, d)
  
  names(out) <- c("dat","ProbValues", "Duplicates")
  return(out)
}
