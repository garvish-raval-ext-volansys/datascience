# farreport

[![Build Status](https://travis-ci.org/garvish/datascience.svg?branch=master)](https://travis-ci.org/garvish/datascience)

The goal of farreport is to Sumarize and Plot US National Highway Traffic Safety Administration's Fatality Analysis Reporting System, which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.

## Example

We first need to set Working directory where data files are available in local compter or just copy all data files into working directory.

```R
library(farreport)
setwd(system.file("extdata", package = "farreport"))
fars_summarize_years(c(2013,2014,2015)
fars_map_state(c(2013,2014,2015)
```
