geoWeights <- function(safran, geo, parallel = FALSE, parallel.var = NULL) {
  if (missing(safran)) 
    stop("L'argument 'safran' doit \u00eatre renseign\u00e9", call. = FALSE)
  if (is.character(safran)) {
  stopifnot(length(safran) == 1L)}
  if (missing(geo)) 
    stop("L'argument 'geo' doit \u00eatre renseign\u00e9", call. = FALSE)
  if (is.character(geo)) {
  stopifnot(length(geo) == 1L)}
  stopifnot(is.logical(parallel))
  if(parallel & is.null(parallel.var))
    stop("L'argument 'parallel.var' doit \u00eatre renseign\u00e9", call. = FALSE)
  if(parallel & !is.null(parallel.var)) {
    stopifnot(is.character(parallel.var))
    stopifnot(length(parallel.var) == 1L)}
  if (!is.character(safran) & class(safran) != "SpatialPolygonsDataFrame") 
    stop("L'argument 'safran' doit \u00eatre un chemin valide ou un objet de classe 'SpatialPolygonsDataFrame'", call. = FALSE)
  if (!is.character(geo) & class(geo) != "SpatialPolygonsDataFrame") 
    stop("L'argument 'geo' doit \u00eatre un chemin valide ou un objet de classe 'SpatialPolygonsDataFrame'", call. = FALSE)
  if (is.character(safran)) {
    safran <- enc2utf8(normalizePath(safran))
    if (!file.exists(safran)) 
      stop("Impossible de trouver le fichier ", basename(safran), call. = FALSE)
    if (tolower(file_ext(safran)) != "shp") 
      stop("Le fichier ", basename(safran), " doit \u00eatre un fichier *.shp valide", call. = FALSE)
    safran <- readOGR(dsn = dirname(safran), layer = file_path_sans_ext(basename(safran)), verbose=FALSE)
  }
  if (is.character(geo)) {
    geo <- enc2utf8(normalizePath(geo))
    if (!file.exists(geo)) 
      stop("Impossible de trouver le fichier ", basename(geo), call. = FALSE)
    if (tolower(file_ext(geo)) != "shp") 
      stop("Le fichier ", basename(geo), " doit \u00eatre un fichier *.shp valide", call. = FALSE)
    geo <- readOGR(dsn = dirname(geo), layer = file_path_sans_ext(basename(geo)), verbose=FALSE)
  }
  if (!is.null(parallel.var) & !any(names(geo@data) == parallel.var)) 
    stop(parallel.var, " doit \u00eatre un nom de variable valide", call. = FALSE)
  if (!identicalCRS(safran, geo)) {
    safran <- spTransform(safran, CRS("+init=epsg:27572"))
    geo <- spTransform(geo, CRS("+init=epsg:27572"))
  }
  
  geo$Areageo <- gArea(geo, byid = TRUE)
  
  cores <- detectCores()
  if (parallel == TRUE & cores == 1) {
    parallel <- FALSE
    warning("Votre ordinateur ne dispose que d'un seul c\u0153ur. Le calcul n'est pas parall\u00e9lisable.")
  }
  if (parallel == FALSE & !is.null(parallel.var)) {
  parallel.var <- NULL
  }
  if (parallel) {
    cl <- makeCluster(cores[1] - 1)
    registerDoParallel(cl)
    .each <- levels(as.factor(as.character(geo@data[, parallel.var])))
    temp <- foreach(i = .each, .combine = rbind, .packages = "raster") %dopar% {
      raster::intersect(geo[geo@data[,parallel.var]== i,], safran)
    }
    stopCluster(cl)
    temp$PoidsGeoSafran <- gArea(temp, byid = TRUE) / temp$Areageo
    temp$Areageo <- NULL
  } else {
    temp <- raster::intersect(geo, safran)
    temp$PoidsGeoSafran <- gArea(temp, byid=TRUE) / temp$Areageo
    temp$Areageo <- NULL
  }
  temp <- temp@data
  return(temp)
}
