library(OHLCMerge)
library(dplyr)

merge_group <- function(group, group_name, save_dir = ".", ...) {
  if (!dir.exists(save_dir)) {
    dir.create(savedir, recursive = TRUE)
  }

  files <- group$abspath

  all <- Reduce(
    function(all, file) {
      combine_xts(all, read_ohlcv(file, ...), update = TRUE)
    },
    files[-1],
    read_ohlcv(files[1], ...)
  )

  path <- file.path(save_dir, paste0(group_name, ".csv"))
  export_csv(all, path)
  print(paste("Saved '", group_name, "' in", path))
  group
}

dirpath <- "E:/Jordan/Downloads/datos_ig"

files <- dir(dirpath)

files_split <- stringr::str_match(files, "^(.*)_.*")

colnames(files_split) <- c("file", "dataset")

files_tb <- as_tibble(files_split)

files_tb %>%
  head(20) %>%
  mutate(abspath = file.path(dirpath, file)) %>%
  group_by(dataset) %>%
  do(merge_group(., .$dataset[1],
                 save_dir=dirname(.$abspath[1]),
                 date_format = "%Y.%m.%d"))
