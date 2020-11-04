data("gluDat")
gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
gluDat3_ls <- DataScrubbing(dat = gluDat2, unitVal = 1)
gluDat4 <- GenEpisode(dat = gluDat3_ls$dat, epiMethod = "pseudo")

metricList <- GenGluM(dat = gluDat4, hypocutoffs = c(4, 3, 2.5),
                      hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
                      hgicutoff = 10, unitVal = 1)
lapply(metricList, function(m) round(m, 1))

ProGluTable(metricList = metricList, unitVal = 1)

GenHypoIntervals(dat = gluDat4)

PerformExclusion(preDat = gluDat4)

# DataScrubbing ---

# Use a part of the example data for illustration:
data("gluDat")
gluDat <- gluDat[sort.list(gluDat$ADMISSION.ID), ]
dat <- gluDat[1:20, ]
# First format RESULT.DATE column in dat:
dat <- FormatDate(dat = dat, yy = 2016, mm = 7)
# Then distort some of the entries in dat for illustrative purpose:
dat$RESULT.DATE[2] <- dat$RESULT.DATE[1] # Make two timings identical for this stay
dat$RESULT[c(3, 8, 12, 15, 20)] <- c("<9.4", ">33.3", "c6.4", "lo", "<0.6")
# Make entry 12 unrecognisable
dat
# Now scrub dat:
DataScrubbing(dat = dat, unitVal = 1)

# FormatDate ---

# Load example data
data("gluDat")
head(gluDat)
gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
head(gluDat2)

# GenEpisode ---

# Load example data
data("gluDat")
# Process date-time variable and blood glucose readings:
gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
gluDat3_ls <- DataScrubbing(dat = gluDat2, unitVal = 1)
# Specify admission episodes based on admission ID:
gluDat4 <- GenEpisode(dat = gluDat3_ls$dat, epiMethod = "Admininfo")

metricList <- GenGluM(dat = gluDat4, hypocutoffs = c(4, 3, 2.5),
                      hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
                      hgicutoff = 10, unitVal = 1)

# GenGluM ---

# First prepare example data using GenEpisode:
data("gluDat")
gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
gluDat3_ls <- DataScrubbing(dat = gluDat2, unitVal = 1)
gluDat4 <- GenEpisode(dat = gluDat3_ls$dat, epiMethod = "Admininfo")
# Then generate glucometrics:
metricList <- GenGluM(dat = gluDat4, hypocutoffs = c(4, 3, 2.5),
                      hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
                      hgicutoff = 10, unitVal = 1)
# View glucometrics (round to 1 decimal place):
lapply(metricList, function(m) round(m, 1))

# PerformExclusion ---

# First prepare data using GenEpisode:
data("gluDat")
gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
gluDat3_ls <- DataScrubbing(dat = gluDat2, unitVal = 1)
gluDat4 <- GenEpisode(dat = gluDat3_ls$dat, epiMethod = "Admininfo")
# Create an "AGE" column with all values assigned to "AGE" to indicate that age
# is not available in this data:
gluDat4$AGE <- "AGE"
# Then apply exclusion criteria:
exlList <- PerformExclusion(preDat = gluDat4)
exlList

# GenGluTable ---

# First generate glucometrics using GenGluM:
data("gluDat")
gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
gluDat3_ls <- DataScrubbing(dat = gluDat2, unitVal = 1)
gluDat4 <- GenEpisode(dat = gluDat3_ls$dat, epiMethod = "Admininfo")
metricList <- GenGluM(dat = gluDat4, hypocutoffs = c(4, 3, 2.5),
                      hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
                      hgicutoff = 10, unitVal = 1)
# Then generate glucometrics table:
ProGluTable(metricList = metricList, unitVal = 1)

# GenHypoIntervals ---

# First prepare example data using GenEpisode:
data("gluDat")
gluDat2 <- FormatDate(dat = gluDat, yy = 2016, mm = 7)
gluDat3_ls <- DataScrubbing(dat = gluDat2, unitVal = 1)
gluDat4 <- GenEpisode(dat = gluDat3_ls$dat, epiMethod = "Admininfo")
# Then generate hypo intervals:
GenHypoIntervals(dat = gluDat4)