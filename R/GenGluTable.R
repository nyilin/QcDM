#' @title Produce Glucometrics table
#' @param metricList A list of glucometrics, from \code{\link{GenGluM}}
#' @param unitVal A unit indicator. 1 stands for mmol/L, 2 stands for md/dL
#' @return Returns a list containing a table summarising the number of
#'   patient-days and patient-stays with glucose reading in prespecified ranges,
#'   and a vector containing footnotes to this table.
#' @examples
#' # First generate glucometrics using GenGluM:
#' data("gluDat")
#' gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
#' gluDat3_ls <- DataScrubbing(dat = gluDat2, unitVal = 1)
#' gluDat4 <- GenEpisode(dat = gluDat3_ls$dat, epiMethod = "Admininfo")
#' metricList <- GenGluM(dat = gluDat4, hypocutoffs = c(4, 3, 2.5),
#'                       hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
#'                       hgicutoff = 10, unitVal = 1)
#' # Then generate glucometrics table:
#' ProGluTable(metricList = metricList, unitVal = 1)
#' @author Ying Chen, Mark Salloway
#' @export
ProGluTable <- function(metricList, unitVal){
  # Input the list of metrics
  # Output the structured table.
  # unitVal <- c('mmol/L','md/dL')[unitVal]
  # rname <- list(c("Number (count)",
  #
  #            paste("Percent with glucose >=",hypercutoffs[1],unitVal),
  #            paste("Percent with glucose >=",hypercutoffs[2],unitVal),
  #            paste("Percent with glucose >=",hypercutoffs[3],unitVal),
  #             "Median HGI", "Mean HGI",
  #            paste("Percent with glucose >=",normalrange[1]," and <",normalrange[2],unitVal)),
  #            paste0("Median glucose (",unitVal,")"),
  #            paste0("Mean glucose (",unitVal,")"),
  #            paste0("Patient-day weighted median glucose (",unitVal,")"),
  #            paste0("Patient-day weighted mean glucose (",unitVal,")"),
  #            c(paste("Percent with glucose <",hypocutoffs[1],unitVal),
  #              paste("Percent with glucose <",hypocutoffs[2],unitVal),
  #              paste("Percent with glucose <",hypocutoffs[3],unitVal),
  #              "Percent of patient-stays with a recurrent hypoglycemia day (10-240 mins)"),
  #            c("Median SD", "Mean SD",
  #              "Median J-index", "Mean J-index")
  #            )
  HyperFreqNR <- rbind(
    sapply(1:3,function(x) {return(metricList[[x]][grepl("Hyper1st",names(metricList[[x]]))])}),
    sapply(1:3,function(x) {return(metricList[[x]][grepl("Hyper2nd",names(metricList[[x]]))])}),
    sapply(1:3,function(x) {return(metricList[[x]][grepl("Hyper3rd",names(metricList[[x]]))])}),
    sapply(1:3,function(x) {return(metricList[[x]][grepl("NormalRange",names(metricList[[x]]))])})
  )


  TotFreq <- rbind(
    sapply(1:3, function(x){return(metricList[[x]][grepl("Reading",names(metricList[[x]]))])})
  )
  HypoFreq <- rbind(
    sapply(1:3,function(x) {return(metricList[[x]][grepl("Hypo1st",names(metricList[[x]]))])}),
    sapply(1:3,function(x) {return(metricList[[x]][grepl("Hypo2nd",names(metricList[[x]]))])}),
    sapply(1:3,function(x) {return(metricList[[x]][grepl("Hypo3rd",names(metricList[[x]]))])})
  )
  HyperPropNR <- t(apply(HyperFreqNR, 1, function(x){return(x/TotFreq)}))
  HypoProp <- t(apply(HypoFreq, 1, function(x){return(x/TotFreq)}))

  round_digits <- ifelse(unitVal==1,1,0)
  round_digits_p <-  1 # for percentage
  c1 <- round(c(metricList[[1]][grepl("popMedian",names(metricList[[1]]))],
                metricList[[1]][grepl("popMean",names(metricList[[1]]))],
                metricList[[1]][grepl("popSD",names(metricList[[1]]))],
                metricList[[1]][grepl("popJindex",names(metricList[[1]]))]),round_digits)
  c1_upd <- c1
  ## median: report IQR in the bracket
  ## mean: report SD in the bracket
  ## rounding to integer
  eTab <- round(rbind(
    sapply(2:3,function(x) {return(metricList[[x]][grepl("Mmedian",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("Mmean",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("SDmedian",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("SDmean",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("Jmedian",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("Jmean",names(metricList[[x]]))])})
  ),round_digits)
  cTab1 <- round(rbind(
    sapply(2:3,function(x) {return(metricList[[x]][grepl("M1Q",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("Msd",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("SD1Q",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("SDsd",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("J1Q",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("Jsd",names(metricList[[x]]))])})
  ),round_digits)
  cTab2 <- round(rbind(
    sapply(2:3,function(x) {return(metricList[[x]][grepl("M3Q",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(2*metricList[[x]][grepl("Msd",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("SD3Q",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(2*metricList[[x]][grepl("SDsd",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(metricList[[x]][grepl("J3Q",names(metricList[[x]]))])}),
    sapply(2:3,function(x) {return(2*metricList[[x]][grepl("Jsd",names(metricList[[x]]))])})
  ),round_digits)
  finTab <-matrix(paste0(eTab, " (",cTab2 - cTab1,")"),nrow(eTab),ncol(eTab),byrow = FALSE)

  eC2 <- round(c(metricList[[3]][grepl("Wmedian",names(metricList[[3]]))],
                 metricList[[3]][grepl("Wmean",names(metricList[[3]]))],
                 metricList[[3]][grepl("hgimedian",names(metricList[[3]]))],
                 metricList[[3]][grepl("hgimean",names(metricList[[3]]))]),round_digits)
  cC2l <- round(c(metricList[[3]][grepl("W1Q",names(metricList[[3]]))],
                  metricList[[3]][grepl("Wsd",names(metricList[[3]]))],
                  metricList[[3]][grepl("hgi1Q",names(metricList[[3]]))],
                  metricList[[3]][grepl("hgisd",names(metricList[[3]]))]),round_digits)
  cC2u <- round(c(metricList[[3]][grepl("W3Q",names(metricList[[3]]))],
                  2*metricList[[3]][grepl("Wsd",names(metricList[[3]]))],
                  metricList[[3]][grepl("hgi3Q",names(metricList[[3]]))],
                  2*metricList[[3]][grepl("hgisd",names(metricList[[3]]))]),round_digits)

  finc2 <- paste0(eC2, " (",cC2u - cC2l,")")

  recur <- paste0(
    metricList[[3]][grepl("RecurHypo",names(metricList[[3]]))],
    " (",
    round(metricList[[3]][grepl("RecurHypo",names(metricList[[3]]))] /
            metricList[[3]][grepl("FreqReading",names(metricList[[3]]))]*100,
          round_digits_p),
    "%)"
  )
  table1 <- rbind(
    TotFreq,
    matrix(paste0(HyperFreqNR, " (",round(HyperPropNR*100, round_digits_p),"%)"),
           nrow(HyperFreqNR),ncol(HyperFreqNR),byrow = FALSE)[1:3,],
    c(rep("",2),finc2[3]),
    c(rep("",2),finc2[4]),# HGI median and mean
    matrix(paste0(HyperFreqNR, " (",round(HyperPropNR*100, round_digits_p),"%)"),
           nrow(HyperFreqNR),ncol(HyperFreqNR),byrow = FALSE)[4,], # normal range
    c(paste0(c1_upd[1], " (", cC2u[1]-cC2l[1], ")"),finTab[1,]), # median
    c(paste0(c1_upd[2], " (", cC2l[2], ")"),finTab[2,]), # mean
    c(rep("",2),finc2[1]), # weighted median
    c(rep("",2),finc2[2]), # weighted mean
    matrix(paste0(HypoFreq, " (",round(HypoProp*100, round_digits_p),"%)"),
           nrow(HypoFreq),ncol(HypoFreq),byrow = FALSE),
    c(rep("",2),recur),
    cbind(rep("",4),finTab[3:6,])[1:2,],
    cbind(rep("",4),finTab[3:6,])[3:4,]
  )

  message <-c(
    "*:The summary statistics of the glucometrics for patient-day means and patient-stay means.",
    paste0("#1: ",metricList[[2]]["NAinSD"]," (",
           round(metricList[[2]]["NAinSD"]/metricList[[2]]["FreqReading"]*100,2),
           "%) patient-days were removed for calculation of patient-day SD and J-index."),
    paste0("#2: ",metricList[[3]]["NAinSD"]," (",
           round(metricList[[3]]["NAinSD"]/metricList[[3]]["FreqReading"]*100,2),
           "%) patient-stays were removed for calculation of patient-stay SD and J-index.")
  )
  out <- list(table1, message)
  return(out)
}
