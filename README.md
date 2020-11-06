QcDM: <ins>Q</ins>uality <ins>c</ins>are for <ins>D</ins>iabetes <ins>M</ins>ellitus
====================================================================================

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
based on the admission information in `ADMISSION.ID`:

    gluDat2 <- FormatDate(dat = gluDat, yy = 2020, mm = 7)
    gluDat3 <- GenEpisode(dat = gluDat2, epiMethod = "Admininfo")
    ## 6 rows with non-numeric glucose readings are removed.

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

|                                                                          | Patient-sample | Patient-day\#1 | Patient-stay\#2 |
|--------------------------------------------------------------------------|:---------------|:---------------|:----------------|
| Number (count)                                                           | 4453           | 962            | 100             |
| Percent with glucose &gt;= hyper-cutoff1                                 | 326 (7.3%)     | 150 (15.6%)    | 42 (42%)        |
| Percent with glucose &gt;= hyper-cutoff2                                 | 64 (1.4%)      | 33 (3.4%)      | 9 (9%)          |
| Percent with glucose &gt;= hyper-cutoff3                                 | 23 (0.5%)      | 11 (1.1%)      | 5 (5%)          |
| Median HGI                                                               |                |                | 0.1 (1.1)       |
| Mean HGI                                                                 |                |                | 0.8 (1.5)       |
| Percent with glucose in normal range                                     | 3129 (70.3%)   | 759 (78.9%)    | 76 (76%)        |
| Median glucose                                                           | 7.7 (3.4)      | 7.5 (3)        | 7.7 (3.4)       |
| Mean glucose                                                             | 8.5 (2.6)      | 8 (2.6)        | 8.3 (2.8)       |
| Patient-day weighted median glucose                                      |                |                | 7.6 (3.4)       |
| Patient-day weighted mean glucose                                        |                |                | 8.1 (2.6)       |
| Percent with glucose &lt; hypo-cutoff1                                   | 151 (3.4%)     | 95 (9.9%)      | 34 (34%)        |
| Percent with glucose &lt; hypo-cutoff2                                   | 24 (0.5%)      | 19 (2%)        | 7 (7%)          |
| Percent with glucose &lt; hypo-cutoff3                                   | 8 (0.2%)       | 7 (0.7%)       | 4 (4%)          |
| Percent of patient-stays with a recurrent hypoglycemia day (10-240 mins) |                |                | 6 (6%)          |
| Median SD                                                                |                | 1.3 (1.7)      | 1.6 (1.7)       |
| Mean SD                                                                  |                | 1.7 (1.4)      | 2.2 (1.5)       |
| Median J-index                                                           |                | 26.9 (29.4)    | 30.4 (35.4)     |
| Mean J-index                                                             |                | 35.9 (30.6)    | 41.4 (31.4)     |
