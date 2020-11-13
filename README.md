QcDM: **Q**uality **c**are for **D**iabetes **M**ellitus
========================================================

Overview
--------

QcDM is an R package developed for generating glucometrics measures from
point-of-care blood glucose data at three different units of analysis:
patient-sample, patient-day, and patient-stay.

This package is a part of the [QcDM
Project](https://github.com/nyilin/QcDM_Project.git), which provides a
user-friendly R-Shiny user interface for convenient and flexible data
processing, and generates a detailed and well-formatted report on the
glucometrics measurements.

Users are strongly recommended to use QcDM via the R-Shiny interface.
Sections below describes the installation of the package and its basic
usage.

Installation
------------

Package devtools is needed to install the QcDM package from Github:

    # First, install devtools if it is not already installed:
    install.packages("devtools")
    # Then, load devtools to install QcDM from Github:
    library(devtools)
    install_github("nyilin/QcDM")

Basic usage
-----------

The QcDM package includes a simulated example data to illustrate its
basic usage, which was generated for 10 hypothetical ward locations
(indicated by letters A to J) in July 2020. The following is a preview
of the first 5 rows of the example data:

    library(QcDM)
    data("gluDat")

The unit of measurement of glucose readings in the example data is
mmol/L.

Before generating glucometrics measurements, first process the date-time
stamps of glucose readings, and identify glucose monitoring episodes
based on the admission information in `ADMISSION.ID`. In this example,
we focus on ward A.

    gluDat2 <- FormatDate(dat = gluDat[gluDat$LOCATION == "A", ], yy = 2020, mm = 7)
    gluDat3 <- GenEpisode(dat = gluDat2, epiMethod = "Admininfo")
    ## 3 rows with non-numeric glucose readings are removed.

Then, generate basic glucometrics measurements using the following
cutoffs as an example:

-   Hyperglycemia: consider three cutoff values, i.e., higher than or
    equal to 14 mmol/L, 20 mmol/L or 24 mmol/L.
-   Hypoglycemia: consider three cutoff values, i.e., below 4 mmol/L, 3
    mmol/L or 2.5 mmol/L.
-   Normal range of glucose readings: above or equal to 4 mmol/L and
    below 10 mmol/L.
-   Hyperglycemia index (HGI): above 10 mmol/L per hour of hospital
    stay.

<!-- -->

    metricList <- GenGluM(dat = gluDat3, hypocutoffs = c(4, 3, 2.5),
                          hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
                          hgicutoff = 10, unitVal = 1)
    knitr::kable(ProGluTable(metricList = metricList, unitVal = 1)[[1]])

|                                                                          | Patient-sample | Patient-day | Patient-stay |
|--------------------------------------------------------------------------|:---------------|:------------|:-------------|
| Number (count)                                                           | 600            | 141         | 15           |
| Percent with glucose &gt;= hyper-cutoff1                                 | 56 (9.3%)      | 25 (17.7%)  | 6 (40%)      |
| Percent with glucose &gt;= hyper-cutoff2                                 | 15 (2.5%)      | 7 (5%)      | 2 (13.3%)    |
| Percent with glucose &gt;= hyper-cutoff3                                 | 8 (1.3%)       | 3 (2.1%)    | 1 (6.7%)     |
| Median HGI                                                               |                |             | 0 (1.8)      |
| Mean HGI                                                                 |                |             | 1.6 (2.7)    |
| Percent with glucose in normal range                                     | 439 (73.2%)    | 110 (78%)   | 11 (73.3%)   |
| Median glucose                                                           | 6.7 (4.9)      | 6.9 (4)     | 7 (4.9)      |
| Mean glucose                                                             | 8.2 (4.1)      | 8.4 (4.2)   | 8.7 (4)      |
| Patient-day weighted median glucose                                      |                |             | 7 (4.9)      |
| Patient-day weighted mean glucose                                        |                |             | 8.7 (4)      |
| Percent with glucose &lt; hypo-cutoff1                                   | 13 (2.2%)      | 10 (7.1%)   | 7 (46.7%)    |
| Percent with glucose &lt; hypo-cutoff2                                   | 0 (0%)         | 0 (0%)      | 0 (0%)       |
| Percent with glucose &lt; hypo-cutoff3                                   | 0 (0%)         | 0 (0%)      | 0 (0%)       |
| Percent of patient-stays with a recurrent hypoglycemia day (10-240 mins) |                |             | 2 (13.3%)    |
| Median SD                                                                |                | 1.2 (2)     | 2 (1.9)      |
| Mean SD                                                                  |                | 1.6 (1.3)   | 2.4 (1.9)    |
| Median J-index                                                           |                | 22.5 (37.9) | 31.2 (57.6)  |
| Mean J-index                                                             |                | 41.5 (48.4) | 52 (50.2)    |
