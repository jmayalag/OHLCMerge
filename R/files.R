#' Mezcla y guarda archivos agrupados.
#'
#' @param files archivos a mezclar
#' @param group_name nombre del grupo
#' @param save_dir directorio donde guarda resultados
#' @param verbose muestra mensajes adicionales
#' @param ... parametros adicionales para read_ohlcv
#'
#' @return tabla con el resumen de los archivos guardados
#'
#' @examples
#' \donttest{
#' files <- dir("data")
#' merge_group(files, "AAPL", "processed")
#' }
merge_group <- function(files, group_name, save_dir = ".", verbose = FALSE, ...) {
  if (!dir.exists(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
  }
  filtered <- files
  # Skip empty files
  repeat {
    valid_first <- valid_csv(filtered[1])
    if (valid_first) {
      break
    }
    message(paste("\nSkipping", filtered[1]))
    filtered <- filtered[-1]
  }

  # TODO: Do first case inside loop (Use purrr maybe?)

  first <- read_ohlcv(filtered[1], ...)

  group_periodicity <- periodicity_df(head(first))

  all <- Reduce(
    function(all, file) {
      current <- read_ohlcv(file, ...)
      periodicity <- periodicity_df(head(current))
      compat <- compatible_periodicity(group_periodicity, periodicity)
      if (!compat) {
        message(paste0(
          "\n",
          basename(file), " has different periodicity:\n",
          format_periodicity(periodicity),
          "\nExpected: ", format_periodicity(group_periodicity), "\n"
        ))
      }


      combine_xts(all, current, update = TRUE, verbose = verbose)
    },
    filtered[-1],
    first
  )

  filename <- paste0(group_name, ".csv")
  path <- file.path(save_dir, filename)
  export_csv(all, path)
  message(paste0("\nSaved '", group_name, "' in ", path))
  dplyr::tibble(
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
#' \donttest{
#' auto_group_files(c("AAPL_1.csv", "AAPL_2.csv", "AMZN_1.csv", "AMZN_2.csv"))
#' }
auto_group_files <- function(files) {
  names <- basename(files)
  files_split <- stringr::str_match(names, "^(.*)[_-].*")

  colnames(files_split) <- c("file", "dataset")

  files_tb <- dplyr::as_tibble(files_split) %>%
    dplyr::mutate(filepath = files) %>%
    tidyr::drop_na()

  grouped <- files_tb %>%
    dplyr::group_by(dataset)

  grouped
}

#' Agrupa archivos sin tener en cuenta el formato del nombre de archivo.
#' Los archivos deben tener periodicidades iguales para poder ser mezclados.
#'
#' @param files filepath de archivos
#' @param name nombre del dataset final
#'
#' @return tabla agrupada de los archivos
#' @export
#'
#' @examples
#' \donttest{
#' group_files(c("PreciosApple2016.csv", "AAPL2017.csv", "StockApple2018.csv"), "AAPL")
#' }
group_files <- function(files, name) {
  dplyr::tibble(file = basename(files), dataset = name, filepath = files) %>%
    dplyr::group_by(dataset)
}

#' Mezcla archivos en cada grupo y guarda el csv final.
#'
#' @param grouped tabla agrupada
#' @param save_dir directorio donde guardar resultados
#' @param save_summary guardar el reporte como  csv
#' @param log mostrar logs
#' @param info mostrar mensajes de info
#' @param verbose mostrar todos los mensajes
#' @param ... argumentos para merge_group
#'
#' @return el reporte de los resultados
#' @export
#'
#' @examples
#' \donttest{
#' grouped <- auto_group_files(c("AAPL_1.csv", "AAPL_2.csv"))
#' merge_files_by_group(grouped, "processed")
#' }
merge_files_by_group <- function(grouped, save_dir, save_summary = FALSE, log = FALSE, info = FALSE, verbose = FALSE, ...) {
  merged <- grouped %>%
    dplyr::do(merge_group(.$filepath,
      .$dataset[1],
      save_dir = save_dir,
      log = log, info = info, verbose = verbose,
      ...
    ))

  suppressWarnings(summary <- merged %>%
    dplyr::select(dataset, files_merged, observations, periodicity, p) %>%
    tidyr::unnest(p))

  if (save_summary) {
    export_csv(summary, file.path(save_dir, "summary.csv"))
  }


  summary
}
