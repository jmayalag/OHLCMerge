library(OHLCMerge)
library(dplyr)
library(tidyr)
library(purrr)

save_dir <- file.path("example_merged")
files <- dir(system.file("extdata", "", package="OHLCMerge"), full.names = T)

grouped <- auto_group_files(files)

grouped

summary <- merge_files_by_group(grouped, save_dir)

print(summary)
