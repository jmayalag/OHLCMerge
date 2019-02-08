library(OHLCMerge)
library(dplyr)


merge_group <- function(files, group_name, save_dir = ".", verify = FALSE, ...) {
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }

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

  filename <- paste0(group_name, ".csv")
  path <- file.path(save_dir, filename)
  export_csv(all, path)
  message(paste0("Saved '", group_name, "' in", path))
  tibble(
    file = filename,
    abspath = path,
    periodicity = format_periodicity(xts::periodicity(all)),
    observations = nrow(all),
    files_merged = length(files),
    p = list(periodicity_df(all)),
    xts = list(all)
  )
}

dirpath <- "E:/Jordan/Downloads/datos_ig"
save_dir <- file.path(dirpath, "merged")

files <- dir(dirpath, pattern = "*.csv")

files_split <- stringr::str_match(files, "^(.*)_.*")

colnames(files_split) <- c("file", "dataset")

files_tb <- as_tibble(files_split) %>% mutate(abspath = file.path(dirpath, file))

grouped <- files_tb %>%
  group_by(dataset)

merged <- grouped %>%
  do(merge_group(.$abspath,
                 .$dataset[1],
                 save_dir=save_dir,
                 log=T, info=F)
  )

summary <- merged %>% select(dataset, files_merged, observations, periodicity, p) %>% unnest(p)

print(summary)

export_csv(summary, file.path(save_dir, "summary.csv"))
