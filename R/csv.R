#' Read OHLCV csv
#'
#' First column must be the date, and the second must be the time component (optional)
#'
#' @param filename path to file
#' @param date_format date format
#' @param time_format time format
#'
#' @return a xts
#' @export
#'
#' @examples
#' read_ohlc("ohlc.csv")
read_ohlcv <- function(filename,
                       date_format = "%Y-%m-%d",
                       time_format = "%H:%M") {
  # Check if headers are present
  first <- data.table::fread(filename, header = FALSE, nrows = 1)
  if (is.character(first$V3)) {
    skip <- 1
  } else {
    skip <- 0
  }
  dt <- data.table::fread(filename, header = FALSE, skip = skip)

  if (is.character(dt$V2)) {
    # Has time column
    datetime_format <- paste0(date_format, " ", time_format)
    dt[, V1 := as.POSIXct(paste(V1, V2), format = datetime_format, tz = "UTC")]
    dt[, V2 := NULL]
  } else {
    dt[, V1 := as.POSIXct(V1, format = date_format, tz = "UTC")]
  }

  x <- data.table::as.xts.data.table(dt)
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume")
  x
}

#' Exporta el dataset a un archivo csv
#'
#' @param data el dataset
#' @param file path del archivo a exportar
#'
#' @return el archivo csv
#' @export
#'
#' @examples
#' export_csv(DJI)
export_csv <- function(data, file) {
  df <- to_dataframe(data)
  write.csv(df, file, row.names = FALSE, quote = FALSE)
}

#' Obtiene un dataset del formato csv
#'
#' @param dataset nombre del dataset
#' @param datadir directorio en donde se encuentra el dataset
#' @param recursive buscar recursivamente el dataset dentro del `datadir``
#'
#' @return el xts con los datos
#' @export
#'
#' @examples
#' ## Not Run
#' read_dataset("US30D1", datadir = "~/Downloads/datos_ohlc")
read_dataset <- function(dataset, datadir, recursive = TRUE, verbose = FALSE) {
  extension <- ".csv"
  filename <- paste0(dataset, extension)
  files <- list.files(datadir, pattern = filename, recursive = recursive, full.names = TRUE)
  file <- files[1]
  if (is.na(file)) {
    stop(filename, " dataset was not found in ", datadir)
  }
  if (verbose) {
    if (length(files) > 1) {
      message("Multiple datasets for ", dataset, "found")
    }
    message("Loading ", file)
  }
  read_ohlcv(file)
}

#' Carga el dataset en el entorno global
#'
#' @param dataset nombre del dataset
#' @param datadir directorio en donde se encuentra el dataset
#' @param envir entorno donde asignar la variable
#' @param recursive buscar recursivamente el dataset dentro del `datadir``
#'
#' @return el nombre de la variable a la que se asigno el dataset
#' @export
#'
#' @examples
#' ## Not Run
#' load_dataset("US30D1", datadir = "~/Downloads/datos_ohlc")
load_dataset <- function(dataset, datadir, envir = .GlobalEnv, recursive = TRUE) {
  data <- read_dataset(dataset, datadir, recursive = recursive)
  assign(dataset, data, envir = envir)

  dataset
}
