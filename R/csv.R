#' Validacion basica de csv
#' Si tiene header, debe tener dos filas
#' Si no tiene header, debe tener por lo menos una fila
#'
#' @param filename nombre de archivo
#'
#' @return TRUE si es valido, de lo contrario FALSE
valid_csv <- function(filename) {
  df <- read.csv(filename, nrows = 3, header = FALSE, stringsAsFactors = FALSE)
  has_header <- is.character(df$V3)
  valid <- (has_header && nrow(df) == 3) || (!has_header && nrow(df) >= 2)

  valid
}

#' as.POSIXct ompatibility function for R < 3.5
#'
#' @param x R object to be converted.
#' @param try_formats vector of date formats.
#' @param ... additional arguments for as.POSIXct.
#'
#' @return a POSIXct object.
#' @export
#'
#' @examples
#' as_posixct_compat("2019-01-01")
as_posixct_compat <- function(x, try_formats="%Y-%m-%d", ...) {
  if (uses_try_formats()) {
    as.POSIXct(x = x, tryFormats = try_formats, ...)
  } else {
    err <- NULL

    for (format in try_formats) {
      result <- tryCatch(as.POSIXct(x = x, format = format, ...), error = function(e) {
        if (grepl("unambiguous", e$message, fixed = TRUE)) {
          e
        }
        else {
          stop(e)
        }
      })

      if (inherits(result, "error")) {
        err <- result
      } else if (!is.null(result) && !is.na(result)) {
        return(result)
      }
    }
    stop(err)
  }
}

#' Read OHLCV csv
#'
#' First column must be the date, and the second must be the time component (optional)
#'
#' @param filename path to file
#' @param date_format date format
#' @param time_format time format
#' @param log should log which file is read?
#' @param info show additional info about file
#'
#' @return a xts
#' @export
#'
#' @examples
#' \donttest{
#' read_ohlc("ohlc.csv")
#' }
read_ohlcv <- function(filename,
                       date_format = c("%Y-%m-%d", "%Y.%m.%d"),
                       time_format = "%H:%M",
                       log = FALSE,
                       info = FALSE) {
  if (log) {
    message(paste0("Reading OHLCV from ", filename))
  }

  loginf <- log && info

  # Check if headers are present
  first <- data.table::fread(filename, header = FALSE, nrows = 1)
  if (is.character(first$V3)) {
    skip <- 1
  } else {
    skip <- 0
  }

  if (loginf && skip > 0) {
    message("Headers found")
  }

  dt <- data.table::fread(filename, header = FALSE, skip = skip)
  datetime_format <- expand.grid(date_format, time_format) %>% tidyr::unite("f", sep = " ")

  if (is.character(dt$V2)) {
    # Has time column
    dt[, V1 := as_posixct_compat(paste(V1, V2), try_formats = datetime_format[, 1], tz = "UTC")]

    dt[, V2 := NULL]
    if (loginf) {
      message("Has time column")
    }
  } else {
    dt[, V1 := as_posixct_compat(V1, try_formats = date_format, tz = "UTC")]
  }

  x <- data.table::as.xts.data.table(dt)
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume")

  if (loginf) {
    message(format_periodicity(xts::periodicity(x)))
  }

  x
}

#' Exporta el dataset a un archivo csv
#'
#' @param data el dataset, puede ser un xts o data.frame
#' @param file path del archivo a exportar
#'
#' @return el archivo csv
#' @export
#'
#' @examples
#' require(xts)
#' data(sample_matrix)
#' x <- as.xts(sample_matrix)
#' export_csv(x, "sample.csv")
export_csv <- function(data, file) {
  if (xts::is.xts(data)) {
    data <- to_dataframe(data)
  }
  write.csv(data, file, row.names = FALSE, quote = FALSE)
}

#' Obtiene un dataset del formato csv
#'
#' @param dataset nombre del dataset
#' @param datadir directorio en donde se encuentra el dataset
#' @param recursive buscar recursivamente el dataset dentro del `datadir`
#' @param verbose muestra mensajes de advertencia si es `TRUE`
#'
#' @return el xts con los datos
#' @export
#'
#' @examples
#' \donttest{
#' read_dataset("US30D1", datadir = "~/Downloads/datos_ohlc")
#' }
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
#' \donttest{
#' load_dataset("US30D1", datadir = "~/Downloads/datos_ohlc")
#' }
load_dataset <- function(dataset, datadir, envir = .GlobalEnv, recursive = TRUE) {
  data <- read_dataset(dataset, datadir, recursive = recursive)
  assign(dataset, data, envir = envir)

  dataset
}


#' Checks if this version of R has `try_formats` as a parameter
#'
#' @return TRUE if R >= 3.5
#' @export
#'
#' @examples
#' uses_try_formats()
uses_try_formats <- function() {
  major <- as.numeric(version$major)
  minor <- version$minor
  splt <- stringr::str_split_fixed(minor, "\\.", 2)
  minor <- as.numeric(splt[1])
  patch <- as.numeric(splt[2])

  major > 3 || minor >= 5
}
