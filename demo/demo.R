library(OHLCMerge)
library(dplyr)
library(tidyr)

merge_group <- function(files, group_name, save_dir = ".", verify = FALSE, verbose = FALSE, ...) {
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
        stop(paste(file, "has different periodicity: ", periodicity, " \n Expected: ", group_periodicity))
      }


      combine_xts(all, current, update = TRUE, verbose = verbose)
    },
    files[-1],
    first
  )

  filename <- paste0(group_name, ".csv")
  path <- file.path(save_dir, filename)
  export_csv(all, path)
  message(paste0("Saved '", group_name, "' in ", path))
  tibble(
    file = filename,
    filepath = path,
    periodicity = format_periodicity(xts::periodicity(all)),
    observations = nrow(all),
    files_merged = length(files),
    p = list(periodicity_df(all)),
    xts = list(all)
  )
}

#' Agrupa multiples datasets de acuerdo al nombre del archivo.
#' Ejemplo: AAPL_1.csv, AAPL_2.csv, AAPL_3.csv agrupa a AAPL.csv
#'
#' @param files filepath de archivos
#'
#' @return tabla agrupada de datasets
#' @export
#'
#' @examples
#' auto_group_files(c("AAPL_1.csv", "AAPL_2.csv", "AMZN_1.csv", "AMZN_2.csv"))
auto_group_files <- function(files) {
  names <- basename(files)
  files_split <- stringr::str_match(names, "^(.*)_.*")

  colnames(files_split) <- c("file", "dataset")

  files_tb <- as_tibble(files_split) %>%
    mutate(filepath = files) %>%
    drop_na()

  grouped <- files_tb %>%
    group_by(dataset)

  grouped
}

#' Agrupa archivos sin tener en cuenta el formato del nombre de archivo.
#' Debe asegurarse que los archivos esten en el orden correcto, de lo contrario el
#' resultado no estara ordenado por fecha.
#'
#' Los archivos deben tener periodicidades iguales para poder ser mezclados.
#'
#' @param files filepath de archivos
#' @param name nombre del dataset final
#'
#' @return tabla agrupada de los archivos
#' @export
#'
#' @examples
#' group_files(c("PreciosApple2016.csv", "AAPL2017.csv", "StockApple2018.csv"), "AAPL")
group_files <- function(files, name) {
  tibble(file = basename(files), dataset = name, filepath = files) %>%
    group_by(dataset)
}

merge_files_by_group <- function(grouped, save_dir, save_summary = FALSE) {
  merged <- grouped %>%
    do(merge_group(.$filepath,
      .$dataset[1],
      save_dir = save_dir,
      log = F, info = F, verbose = T
    ))

  suppressWarnings(summary <- merged %>%
    select(dataset, files_merged, observations, periodicity, p) %>%
    unnest(p))

  if (save_summary) {
    export_csv(summary, file.path(save_dir, "summary.csv"))
  }


  summary
}

dirpath <- "~/Google Drive/py_stock_market_prediction/datos/datasets_paper1"
save_dir <- file.path("~/Downloads/datasets_paper1")

files <- dir(dirpath, pattern = "*.csv", full.names = TRUE)

grouped <- auto_group_files(files)

summary <- merge_files_by_group(grouped, save_dir)

print(summary)

group_files(c("AAPL.csv", "Apple2018.csv", "Apple 2019.csv"), "AAPL")
