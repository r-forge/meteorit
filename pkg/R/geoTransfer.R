geoTransfer <- function(meteoM, poids, var.maille, var.met, var.poids, var.geo, var.temp) {
  if (missing(meteoM)) 
    stop("L'argument 'meteoM' doit \u00eatre renseign\u00e9", call. = FALSE)
  if (missing(poids)) 
    stop("L'argument 'poids' doit \u00eatre renseign\u00e9", call. = FALSE)
  if (missing(var.maille)) 
    stop("L'argument 'var.maille' doit \u00eatre renseign\u00e9", call. = FALSE)
  if (missing(var.met)) 
    stop("L'argument 'var.met' doit \u00eatre renseign\u00e9", call. = FALSE)
  if (missing(var.poids)) 
    stop("L'argument 'var.poids' doit \u00eatre renseign\u00e9", call. = FALSE)
  if (missing(var.geo)) 
    stop("L'argument 'var.geo' doit \u00eatre renseign\u00e9", call. = FALSE)
  if (missing(var.temp)) 
    stop("L'argument 'var.temp' doit \u00eatre renseign\u00e9", call. = FALSE)
  
  stopifnot(is.data.frame(meteoM))
  stopifnot(is.data.frame(poids))
  
  if (length(var.maille) == 1L) {
    var.maille <- rep(var.maille, 2)
  }
  stopifnot(length(var.maille) == 2L)
  stopifnot(is.character(var.maille))
  if (is.character(var.maille) && !(var.maille[1L] %in% names(meteoM))) 
    stop("La variable '", var.maille[1L], "' n'est pas pr\u00e9sente dans '", deparse(substitute(meteoM)), "'.", call. = FALSE)
  if (is.character(var.maille) && !(var.maille[2L] %in% names(poids))) 
    stop("La variable '", var.maille[2L], "' n'est pas pr\u00e9sente dans '", deparse(substitute(poids)), "'.", call. = FALSE)
  
  stopifnot(is.character(var.met) || is.numeric(var.met))
  if (is.numeric(var.met) && max(var.met) > ncol(meteoM)) 
    stop("Certaines variables de 'var.met' ne sont pas pr\u00e9sentes dans l'\u00e9l\u00e9ment '", deparse(substitute(meteoM)), "'.", call. = FALSE)
  if (is.character(var.met) && !all(var.met %in% names(meteoM))) 
    stop("Certaines variables de 'var.met' ne sont pas pr\u00e9sentes dans l'\u00e9l\u00e9ment '", deparse(substitute(meteoM)), "'.", call. = FALSE)
  
  stopifnot(length(var.poids) == 1L)
  stopifnot(is.character(var.poids))
  if (is.character(var.poids) && !(var.poids %in% names(poids))) 
    stop("La variable ", var.poids, " n'est pas une variable valide de '", deparse(substitute(poids)), "'.", call. = FALSE)
  
  stopifnot(length(var.geo) == 1L)
  stopifnot(is.character(var.geo))
  if (is.character(var.geo) && !(var.geo %in% names(poids))) 
    stop("La variable ", var.geo, " n'est pas une variable valide de '", deparse(substitute(poids)), "'.", call. = FALSE)
  
  stopifnot(length(var.temp) == 1L)
  stopifnot(is.character(var.temp))
  if (is.character(var.temp) && !(var.temp %in% names(meteoM))) 
    stop("La variable ", var.temp, " n'est pas une variable valide de '", deparse(substitute(meteoM)), "'.", call. = FALSE)
  
  if (length(class(meteoM)) != 1L) {
    meteoM <- as.data.frame(meteoM)
  }
  if (is.numeric(var.met)) {
    var.met <- names(meteoM[, var.met])
  }
  if (!is.character(meteoM[, var.maille[1L]])) {
    meteoM[, var.maille[1L]] <- as.character(meteoM[, var.maille[1L]])
  }
  meteoM <- as.data.table(meteoM)

  if (length(class(poids)) != 1L) {
    poids <- as.data.frame(poids)
  }
  if (!is.character(poids[, var.maille[2L]])) {
    poids[, var.maille[2L]] <- as.character(poids[, var.maille[2L]])
  }
  poids <- as.data.table(poids)
  
  Mailles <- sort(intersect(data.frame(meteoM)[, var.maille[1L]], data.frame(poids)[, var.maille[2L]]))
  
  meteoM <- filter_(meteoM, paste(var.maille[1L], "%in% Mailles"))
  poids <- filter_(poids, paste(var.maille[2L], "%in% Mailles"))
  
  temp <- left_join(poids, meteoM, by = setNames(nm = var.maille[2L], var.maille[1L]))
  remove(meteoM)
  remove(poids)
  remove(Mailles)
  gc()
  temp <- temp %>% mutate_at(funs(. * (!!sym(var.poids))), .vars = vars(var.met))
  temp <- temp %>% group_by(!!sym(var.geo), !!sym(var.temp)) %>% summarise_at(funs(sum), .vars = vars(var.met))
  temp <- data.frame(temp)
  return(temp)
}
