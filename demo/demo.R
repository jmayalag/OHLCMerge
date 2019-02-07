library(OHLCMerge)
library(dplyr)

periodicity_df <- function(df) {
  p <- xts::periodicity(df) %>% do.call(data.frame, .)
  p
}

compatible_periodicity <- function(first, second) {
  cols <- c("difftime", "frequency", "units", "scale", "label")
  equal <- first == second
  compatible <- all(equal[, cols])

  compatible
}

merge_group <- function(group, group_name, save_dir = ".", verify = FALSE, ...) {
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }

  files <- group$abspath

  first <- read_ohlcv(files[1], ...)

  group_periodicity <- periodicity_df(first)


  all <- Reduce(
    function(all, file) {
      current <- read_ohlcv(file, ...)
      periodicity <- periodicity_df(current)
      compat <- compatible_periodicity(group_periodicity, periodicity)
      if (!compat) {
        stop(paste(file, "has different periodicity: ", periodicity," \n Expected: ", group_periodicity))
      }


      combine_xts(all, current, update = TRUE)
    },
    files[-1],
    first
  )

  path <- file.path(save_dir, paste0(group_name, ".csv"))
  export_csv(all, path)
  message(paste("Saved '", group_name, "' in", path))
  group
}

dirpath <- "E:/Jordan/Downloads/datos_ig"
save_dir <- file.path(dirpath, "merged")

files <- dir(dirpath, pattern = "*.csv")

files_split <- stringr::str_match(files, "^(.*)_.*")

colnames(files_split) <- c("file", "dataset")

files_tb <- as_tibble(files_split) %>% mutate(abspath = file.path(dirpath, file))

grouped <- files_tb %>%
  group_by(dataset)

grouped %>%
  do(merge_group(.,
                 .$dataset[1],
                 save_dir=save_dir,
                 log=T)
     )
