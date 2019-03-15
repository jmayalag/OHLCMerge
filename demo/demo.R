library(OHLCMerge)
library(dplyr)
library(tidyr)

dirpath <- "~/Google Drive/py_stock_market_prediction/datos/datasets_paper1"
save_dir <- file.path("~/Downloads/datasets_paper1")

files <- dir(dirpath, pattern = "*.csv", full.names = TRUE)

grouped <- auto_group_files(files)

summary <- merge_files_by_group(grouped, save_dir)

print(summary)
