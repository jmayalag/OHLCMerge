library(OHLCMerge)
library(dplyr)
library(tidyr)

dirpath <- "~/Downloads/audjpym15"
save_dir <- file.path("~/Downloads/ohlcmerge_demo")

files <- dir(dirpath, pattern = "*.csv", full.names = TRUE)

grouped <- auto_group_files(files)

summary <- merge_files_by_group(grouped, save_dir)

print(summary)
