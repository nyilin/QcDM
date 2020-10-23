expect_equal(QcDM:::recog_one_date(date = "02/04/2020", month = 2, year = 2020),
             "4/2/2020")
expect_equal(QcDM:::recog_one_date(date = "02/04/2020", month = 4, year = 2020),
             "2/4/2020")
expect_true(is.na(QcDM:::recog_one_date(date = "02/04/2020", month = 5, year = 2020)))
expect_true(is.na(QcDM:::recog_one_date(date = "02/04/2020", month = 2, year = 20)))
expect_true(is.na(QcDM:::recog_one_date(date = "02/04/2020", month = "a", year = 2020)))

expect_equal(QcDM:::recog_one_date(date = "04/18/20", month = 4, year = 2020),
             "18/4/2020")

expect_equal(QcDM::recogniseDate(c("02/04/2020", "04/18/20"),
                                 month = 4, year = 2020),
             c("2/4/2020 11:11:11", "18/4/2020 11:11:11"))

expect_equal(QcDM:::recog_one_time(time = c("3:12", "PM")), "15:12:00")
expect_equal(QcDM:::recog_one_time(time = c("3:12", "AM")), "03:12:00")
expect_equal(QcDM:::recog_one_time(time = c("3:12:30", "AM")), "03:12:30")

expect_equal(
  QcDM::recogniseDateTime(dateTime = c("02/04/20 1:23", "19/02/20 3:12 PM",
                                       "11/02/2020 12:11:13", "02/04/2020"),
                          month = 2, year = 2020),
  c("4/2/2020 01:23:00", "19/2/2020 15:12:00", "11/2/2020 12:11:13",
    "4/2/2020 11:11:11")
)
