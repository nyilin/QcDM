# QcDM: **Q**uality **c**are for **D**iabetes **M**ellitus

## Overview

QcDM is an R package developed for generating glucometrics measures from
point-of-care blood glucose data at three different units of analysis:
patient-sample, patient-day, and patient-stay. This package is
distributed under the [MIT license](LICENSE).

This package is a part of the [QcDM
Project](https://github.com/nyilin/QcDM_Project.git), which provides a
user-friendly R-Shiny user interface for convenient and flexible data
processing, and generates a detailed and well-formatted report on the
glucometrics measurements.

Users are strongly recommended to use QcDM via the R-Shiny interface.
Sections below describes the installation of the package and its basic
usage.

## Installation

Package devtools is needed to install the QcDM package from Github:

``` r
# First, install devtools if it is not already installed:
install.packages("devtools")
# Then, load devtools to install QcDM from Github:
library(devtools)
install_github("nyilin/QcDM")
```

## Basic usage

The QcDM package includes a simulated example data to illustrate its
basic usage, which was generated for 4 hypothetical ward locations
(indicated by letters A to D) in July 2020. The following is a preview
of the first 5 rows of the example data:

``` r
library(QcDM)
data("gluDat")
head(gluDat, 5)
##   ADMISSION.ID RESULT    RESULT.DATE LOCATION
## 1            1   12.6 7/1/2020 03:13        A
## 2            2   14.1 7/1/2020 03:15        B
## 3            2    8.6 7/1/2020 06:43        B
## 4            1   17.3 7/1/2020 11:56        A
## 5            1   14.1 7/1/2020 12:26        A
```

The unit of measurement of glucose readings in the example data is
mmol/L. Three BG readings were deliberately assigned non-numeric value
to illustrate the functionality of the QcDM package to detect invalid
data:

``` r
gluDat[is.na(as.numeric(gluDat$RESULT)), ]
## Warning in `[.data.frame`(gluDat, is.na(as.numeric(gluDat$RESULT)), ): NAs
## introduced by coercion
##      ADMISSION.ID RESULT     RESULT.DATE LOCATION
## 2300           60   c7.7 7/16/2020 14:04        D
## 4605           70   d9.6 7/24/2020 13:53        A
## 5143           47   a4.4 7/26/2020 22:58        B
```

Before generating glucometrics measurements, first process the date-time
stamps of glucose readings, and identify glucose monitoring episodes
based on the admission information in `ADMISSION.ID`. In this example,
we focus on ward A.

``` r
gluDat2 <- FormatDate(dat = gluDat[gluDat$LOCATION == "A", ], yy = 2020, mm = 7)
gluDat3 <- GenEpisode(dat = gluDat2, epiMethod = "Admininfo")
## 1 rows with non-numeric glucose readings are removed.
```

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

``` r
metricList <- GenGluM(dat = gluDat3, hypocutoffs = c(4, 3, 2.5),
                      hypercutoffs = c(14, 20, 24), normalrange = c(4, 10),
                      hgicutoff = 10, unitVal = 1)
knitr::kable(ProGluTable(metricList = metricList, unitVal = 1)[[1]])
```

|                                                            | Patient-sample | Patient-day | Patient-stay |
|:-----------------------------------------------------------|:---------------|:------------|:-------------|
| Number (count)                                             | 1610           | 316         | 29           |
| Percent with glucose &gt;= hyper-cutoff1                   | 697 (43.3%)    | 292 (92.4%) | 29 (100%)    |
| Percent with glucose &gt;= hyper-cutoff2                   | 61 (3.8%)      | 56 (17.7%)  | 23 (79.3%)   |
| Percent with glucose &gt;= hyper-cutoff3                   | 5 (0.3%)       | 5 (1.6%)    | 4 (13.8%)    |
| Median HGI                                                 |                |             | 3.5 (0.7)    |
| Mean HGI                                                   |                |             | 3.6 (0.5)    |
| Percent with glucose in normal range                       | 247 (15.3%)    | 6 (1.9%)    | 0 (0%)       |
| Median glucose                                             | 13.2 (0.4)     | 13.6 (2)    | 13.5 (0.5)   |
| Mean glucose                                               | 13.5 (0.5)     | 13.5 (1.7)  | 13.5 (0.5)   |
| Patient-day weighted median glucose                        |                |             | 13.5 (0.4)   |
| Patient-day weighted mean glucose                          |                |             | 13.5 (0.5)   |
| Percent with glucose &lt; hypo-cutoff1                     | 1 (0.1%)       | 1 (0.3%)    | 1 (3.4%)     |
| Percent with glucose &lt; hypo-cutoff2                     | 0 (0%)         | 0 (0%)      | 0 (0%)       |
| Percent with glucose &lt; hypo-cutoff3                     | 0 (0%)         | 0 (0%)      | 0 (0%)       |
| Percent of patient-stays with a recurrent hypoglycemia day |                |             | 0 (0%)       |
| Median SD                                                  |                | 3.2 (1.6)   | 3.4 (0.5)    |
| Mean SD                                                    |                | 3.3 (1.3)   | 3.5 (0.5)    |
| Median J-index                                             |                | 90.8 (31)   | 92.6 (10.5)  |
| Mean J-index                                               |                | 92.9 (24.2) | 92.8 (7.3)   |
