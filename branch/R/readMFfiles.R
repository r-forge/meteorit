readMFfiles <- function(path = ".", type = c("gz", "csv"), progress = FALSE) {
  stopifnot(length(path) == 1L)
  type <- match.arg(type)
  if (path == ".") {
    path <- getwd()
  } else {
    path <- as.character(path)
  }
  if (file.exists(path)) {
    path <- normalizePath(path)
  } else {
    stop("Path should be an existing folder.")
  }
  res <- list()
  list_files <- list.files(path = path, pattern = paste0(".", type, "$"), full.names = TRUE)
  if (length(list_files) == 0L)
    stop("The folder '", path, "' must contain at least one ", type, " file.", call. = FALSE)
  if (type == "gz" & progress) {
    for (i in 1:length(list_files)) {
      res[[i]] <- read.table(list_files[i], header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
      Progress(i, max.value = length(list_files))
 }
  }
  else if (type == "gz" & !progress) {
  for (i in 1:length(list_files)) {
      res[[i]] <- read.table(list_files[i], header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
 }
  }
  if (type == "csv" & progress) {
    for (i in 1:length(list_files)) {
      res[[i]] <- fread(list_files[i], header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
      Progress(i, max.value = length(list_files))
    }
  }
    else if (type == "csv" & !progress) {
  for (i in 1:length(list_files)) {
      res[[i]] <- fread(list_files[i], header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
 }
  }
  res <- data.frame(do.call(rbind, res))
  return(res)
}
