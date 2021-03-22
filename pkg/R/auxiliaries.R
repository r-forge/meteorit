Progress <-  function (value, max.value = NULL) {
  erase_only <- (value == max.value)
  cmd_progress <- get_temp(".progress", default = list(), mode = "list")
  max.value <- as.character(round(max.value))
  l <- nchar(max.value)
  value <- formatC(round(value), width = l)
  msg1 <- gettext("Progression:")
  l1 <- nchar(msg1)
  msg2 <- gettext("sur")
  l3 <- def(cmd_progress$msglength, 0, mode = "numeric", length.out = 1)
  if (l3 < 0) 
    l3 <- 0
  cmd_progress$msglength <- NULL
  backspaces <- paste(rep("\b", l3), collapse = "")
  if (erase_only) {
    message <- ""
    cat(backspaces, rep(" ", l3), sep = "")
  } else {
    message <- paste(msg1, value, msg2, max.value, "fichiers",  collapse = "")
  }
  cat(backspaces, message, sep = "")
  cmd_progress$msglength <- nchar(message)
  assign_temp(".progress", cmd_progress)
  flush.console()
  invisible(NULL)
}
