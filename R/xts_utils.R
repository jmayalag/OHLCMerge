#' Check if two periodicities are compatible
#'
#' @param first periodicity data.frame
#' @param second other periodicity data.frame
#'
#' @return TRUE if compatible, FALSE otherwise
#' @export
#'
#' @examples
#' compatible_periodicty(a, b)
compatible_periodicity <- function(first, second) {
  cols <- c("difftime", "frequency", "units", "scale", "label")
  equal <- first == second
  compatible <- all(equal[, cols])

  compatible
}

#' Get periodicity in data.frame
#'
#' @param x a xts object
#'
#' @return data.frame with periodicity
#' @export
#'
#' @examples
#' periodicity_df(AAPL)
periodicity_df <- function(x) {
  xts::periodicity(x) %>% do.call(data.frame, .)
}

#' Set difference between POSIXct
#'
#' @param first a POSIXct vector
#' @param second another POSIXct vector
#'
#' @return the setdiff between both vectors
#' @export
#'
#' @examples
#' a <- as.POSIXct(sample(1:1000, 4), origin = "1970-01-01")
#' b <- as.POSIXct(sample(1:1000, 4), origin = "1970-01-01")
#' setdiff.POSIXct(a, b)
setdiff.POSIXct <- function(first, second) {
  if (attr(first, "tzone") != attr(second, "tzone")) {
    warning("Time zones are not equal. Using tzone of first argument.")
  }
  as.POSIXct(base::setdiff(first, second), origin = "1970-01-01", tz = attr(first, "tzone"))
}

#' Set intersection between POSIXct
#'
#' @param first a POSIXct vector
#' @param second another POSIXct vector
#'
#' @return the intersection between both vectors
#' @export
#'
#' @examples
#' a <- as.POSIXct(sample(1:1000, 4), origin = "1970-01-01")
#' b <- as.POSIXct(sample(1:1000, 4), origin = "1970-01-01")
#' intersect.POSIXct(a, b)
intersect.POSIXct <- function(first, second) {
  if (attr(first, "tzone") != attr(second, "tzone")) {
    warning("Time zones are not equal. Using tzone of first argument.")
  }
  as.POSIXct(base::intersect(first, second), origin = "1970-01-01", tz = attr(first, "tzone"))
}

#' Union los xts y retorna un xts con indices unicos
#'
#' @param first un xts
#' @param second otro xts
#' @param verbose muestra indices duplicados
#' @param update actualizar con el segundo xts. Si es FALSE, se mantiene el primer xts.
#'
#' @return Ambos xts combinados
#' @export
#'
#' @examples
#' a <- index(AAPL[1:3, ])
#' b <- index(AAPL[3:5, ])
#' combine_xts(a, b)
combine_xts <- function(first, second, verbose = FALSE, update = FALSE) {
  f <- index(first)
  s <- index(second)

  x <- first[setdiff.POSIXct(f, s)]
  y <- second[setdiff.POSIXct(s, f), ]
  # to avoid repeating indexes, the intersection is taken only from one
  common <- intersect.POSIXct(f, s)
  z <- first[common, ]

  if (!identical(second[common, ], z)) {
    warning("Indices conflict with different values")

    if (verbose) {
      print(common)
    }
  }

  if (update) {
    z <- second[common, ]
  }

  rbind(x, y, z)
}

#' Convierte un xts a un dataframe
#'
#' @param data el xts con los datos OHLC
#'
#' @return un dataframe con datos OHLC y fecha de trading
#' @importFrom zoo index coredata
#' @export
#'
#' @examples
#' to_dataframe(DJI)
to_dataframe <- function(data, date_format = "%Y-%m-%d", time_format = "%H:%M:%S") {
  df <- data.frame(coredata(data))
  index <- index(data)
  date <- strftime(index, date_format)
  time <- strftime(index, time_format)
  data.frame(Date = date, Time = time, df)
}

#' Returns a formatted string of periodicity
#'
#' @param x periodicity object
#' @param ... additional arguments
#'
#' @return a character vector
#' @export
#'
#' @examples
#' print(format_periodicity(x))
format_periodicity <- function(x, ...) {
  x.freq <- ifelse(x$scale %in% c("minute", "seconds"), x$frequency,
    ""
  )
  if (x.freq == "") {
    cap.scale <- paste(toupper(substring(x$scale, 1, 1)),
      substring(x$scale, 2),
      sep = ""
    )
    return(paste(cap.scale, "periodicity from", x$start, "to",
      x$end,
      sep = " "
    ))
  }
  else {
    return(paste(x.freq, x$scale, "periodicity from", x$start,
      "to", x$end,
      sep = " "
    ))
  }
}
