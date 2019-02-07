read_dir <- function(path) {
  if(missing(path)) {
    stop("Dir path is missing.")
  }
  files <- dir(path)
}
