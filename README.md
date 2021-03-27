# OHLCMerge

[![Build Status](https://travis-ci.org/jmayalag/OHLCMerge.svg?branch=master)](https://travis-ci.org/jmayalag/OHLCMerge)


Merge OHLC csv files using R.

## Installation

```r
# install.packages("devtools")
devtools::install_github("jmayalag/OHLCMerge")
```

## Usage
Files need to be grouped by dataset.
You can do this manually with `group_files`, or it can be inferred from the filename using
`auto_group_files`. After grouping your files, use the `merge_files_by_group` function.

```r
# manual
groups <- group_files(dir("data", "AAPL.*.csv", full.names = T), "AAPL)

# auto
groups <- auto_group_files(dir("data", "*.csv", full.names = T))


merge_files_by_group(groups, "merged")
```

Check the `demo` directory for some examples.
