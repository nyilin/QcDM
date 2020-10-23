library(data.table)
library(Matching)
library(lmtest)
library(QcDM)
# e0013210
wkdir <- "/Users/NingYilin/Google Drive/Academic/PhD/Collaborated/qcDM_ChenYing/qcDMData (1)"
wkdir <- "C:/Users/e0013210/Desktop/qcDMData"
wkdir <- "C:/Users/e0013210/Desktop/qcDMData_withCaseID/"
wkdir <- "C:/Users/chenying/Desktop/qcDMData"
input <- list(wkdir = wkdir, #choose.dir(),
              hypo3 = 2, hypo2 = 3,	hypo1 = 4,	hyper3 = 14,	hyper2 = 20,
              hyper1 = 24,	normalrange_lower = 4,	normalrange_upper = 10,
              lgZERO = -5, ageRange = c(16, 120), exclude = c(1, 2),
              wardselect = c("A", "B", "C"),
              monthselect = 1:6, yearselect = 2015)
# units <- c("A","B","C")
wards <- dir(paste0(input$wkdir, "/GLU_data"))
start.date <- "2015-01-01"
end.date <- "2015-06-30"
crtVec = c(#crt.age = FALSE, # exclusion criterion: age < 16yrs
           crt.los = TRUE, # los < 24 hrs
           crt.freq = TRUE, # BG total frequency < 5
           crt.1stday = FALSE) # first day excluded
epiMethod = 'Admininfo'
epiMethod <- 'pseudo'
working.dir <- input$wkdir
hypocutoffs <- c(input$hypo1, input$hypo2, input$hypo3)
hypercutoffs <- c(input$hyper1, input$hyper2, input$hyper3)
normalrange <- c(input$normalrange_lower, input$normalrange_upper)


#
wkdir = choose.dir()
wards = c('A','B','C')
periods = c('201501','201502','201503')
upper = 120
lower = 5


# Generate glucometrics
st <- as.Date(start.date)
en <- as.Date(end.date)
ll <- format(seq(st, en, by = '1 month'),"%Y%m")
preDatList <- lapply(
  as.list(wards),
  function(w) {
    datList <- lapply(
      as.list(ll),
      function(l) {
        yy = as.numeric(substr(l, 1, 4))
        m = as.numeric(substr(l, 5, nchar(l)))
        data = read.csv(
          # paste0(working.dir, "/GLU_data_withcaseid/", w, "/", yy, "/", m,
          #        "/GLU_data.csv"),
          paste0(working.dir, "/GLU_data/", w, "/", yy, "/", m,
                 "/GLU_data.csv"),
          header = TRUE,
          stringsAsFactors = FALSE
        )
        if(tolower(epiMethod) == "pseudo"){
          colnames(data) <- c("PATIENT.NO", "RESULT", "RESULT.DATE", "LOCATION")
        }else{
          colnames(data) <- c("PATIENT.NO", "RESULT", "RESULT.DATE", "LOCATION","caseid")
        }

        FormatDate(data, yy = substring(yy, 3, 4), mm = m)
      })
    do.call("rbind", datList)
  })
preDat <- do.call("rbind", preDatList)
# preDat <- NULL
# for(i in 1:length(units)){
#   for(j in 1:length(ll)){
#     w = units[i]
#     yy = as.numeric(substr(ll[j],1,4))
#     m = as.numeric(substr(ll[j],5,nchar(ll[j])))
#     data = read.csv(
#       paste0(working.dir, "/GLU_data/", w, "/", yy, "/", m,
#              "/GLU_data.csv"),
#       header = TRUE,
#       stringsAsFactors = FALSE
#     )
#     colnames(data) <- c("PATIENT.NO", "RESULT", "RESULT.DATE", "LOCATION")
#     preDat <- rbind(preDat,FormatDate(data, substring(yy, 3, 4),m))
#   }
# }
scrubList <- DataScrubbing(preDat)
preDat <- scrubList[[1]]
preDat <- GenEpisode(preDat, epiMethod = epiMethod, caseidCol = 5)
exlList <- PerformExclusion(preDat, crtVec = crtVec)
Dat.Exl <- exlList[[2]]

metricList <- GenGluM(Dat.Exl, hypocutoffs, hypercutoffs, normalrange)
tbGlu <- ProGluTable(metricList, hypocutoffs, hypercutoffs, normalrange)
tbGlu1 <- tbGlu[[1]]

# Timeliness
pvalues <- pCalfunc(preDat)

# HypoManagement
HypoInt <- GenHypoIntervals(preDat)


# Batch process glucometrics
metricList <- bpg(Dat.Exl,span.months = 1, hypocutoffs, hypercutoffs, normalrange)

nMonths <- countMonths(2015, 2015, 1, 6)
useAdmin = TRUE
