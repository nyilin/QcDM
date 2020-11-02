data(glu_dat)
dat <- glu_dat[1:10, ]
# First format RESULT.DATE column in dat:
dat <- FormatDate(dat = dat, yy = 2016, mm = 7)
# Then distort some of the entries in dat:
dat$RESULT.DATE[7] <- dat$RESULT.DATE[4] # Make two timings identical for this stay
DataScrubbing(dat = dat, unitVal = 1)

dat$RESULT[3] <- " 11.5"
DataScrubbing(dat = dat, unitVal = 1)

dat$RESULT[c(2, 5, 8, 10)] <- c("<9.4", ">33.3", "lo", "<0.6")
dat
DataScrubbing(dat = dat, unitVal = 1)
