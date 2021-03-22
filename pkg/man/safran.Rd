\name{safran}
\alias{safran}
\docType{data}

\title{Grille des mailles SAFRAN}

\description{Grille SAFRAN au format vectoriel (couche shapefile). 

Cette grille comprend 9892 mailles carrées de 64km² (8km x 8km), identifiées par un numéro unique.

La maille SAFRAN représente l'unité dans laquelle sont fournies les données spatialisées de Météo France.
}
\usage{safran}

\format{
Un objet de classe \link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}, contenant la table d'attributs suivante:
  \describe{
    \item{maille:}{Numéro de la maille.}
    \item{xl2e:}{Coordonnées X en Lambert II étendu (en m).}
    \item{yl2e:}{Coordonnées Y en Lambert II étendu (en m).}
    \item{altitude:}{Altitude moyenne de la maille (en m).}
}
}

\details{La projection utilisée est la projection Lambert II étendu (epsg:27572).}

\seealso{
- \link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}

- \href{http://w3.avignon.inra.fr/geoserver/www/get_safran_number.html}{Utilitaire d'identification des mailles SAFRAN}
}

\examples{
\dontrun{
library(sp)
summary(safran)
head(safran@data)
}
}

\note{Les données spatialisées sont initialement des données issues de stations météorologiques (stations Météo France et stations INRA), sur lesquelles sont appliquées des méthodes d'interpolation spécifiques afin de présenter ces données sur la grille SAFRAN.

La liste des stations météo, avec leurs différents détails, est disponible ici: \url{http://w3.avignon.inra.fr/carto/}
}

\keyword{datasets}
