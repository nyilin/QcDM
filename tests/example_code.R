
# FormatDate ---

# Load example data
data("gluDat")
head(gluDat)
gluDat2 <- FormatDate(dat = gluDat, yy = 2020, mm = 7)
head(gluDat2)

# GenEpisode ---

# Load example data
data("gluDat")
# Focus on data from Ward A. Process date-time variable and blood glucose readings:
gluDat2 <- FormatDate(dat = gluDat[gluDat$LOCATION == "A", ], yy = 2020, mm = 7)
# Specify admission episodes based on admission ID:
gluDat3 <- GenEpisode(dat = gluDat2, epiMethod = "Admininfo")

# GenGluM ---

# Focus on data from Ward A. First prepare example data using GenEpisode:
data("gluDat")
gluDat2 <- FormatDate(dat = gluDat[gluDat$LOCATION == "A", ], yy = 2020, mm = 7)
gluDat3 <- GenEpisode(dat = gluDat2, epiMethod = "Admininfo")
# Then generate glucometrics:
metricList <- GenGluM(dat = gluDat3, hypocutoffs = c(4, 3, 2.5),
                      hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
                      hgicutoff = 10, unitVal = 1)
# View glucometrics (round to 1 decimal place):
lapply(metricList, function(m) round(m, 1))

# PerformExclusion ---

# Focus on data from Ward A. First prepare data using GenEpisode:
data("gluDat")
gluDat2 <- FormatDate(dat = gluDat[gluDat$LOCATION == "A", ], yy = 2020, mm = 7)
gluDat3 <- GenEpisode(dat = gluDat2, epiMethod = "Admininfo")
# Create an "AGE" column with all values assigned to "AGE" to indicate that age
# is not available in this data:
gluDat3$AGE <- "AGE"
# Then apply exclusion criteria:
exlList <- PerformExclusion(preDat = gluDat3)
exlList

# GenGluTable ---

# Focus on data from Ward A. First generate glucometrics using GenGluM:
data("gluDat")
gluDat2 <- FormatDate(dat = gluDat[gluDat$LOCATION == "A", ], yy = 2020, mm = 7)
gluDat3 <- GenEpisode(dat = gluDat2, epiMethod = "Admininfo")
metricList <- GenGluM(dat = gluDat3, hypocutoffs = c(4, 3, 2.5),
                      hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
                      hgicutoff = 10, unitVal = 1)
# Then generate glucometrics table:
ProGluTable(metricList = metricList, unitVal = 1)

# GenHypoIntervals ---

# Focus on data from Ward A. First prepare example data using GenEpisode:
data("gluDat")
gluDat2 <- FormatDate(dat = gluDat[gluDat$LOCATION == "A", ], yy = 2020, mm = 7)
gluDat3 <- GenEpisode(dat = gluDat2, epiMethod = "Admininfo")
# Then generate hypo intervals:
GenHypoIntervals(dat = gluDat3)
