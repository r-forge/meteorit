\name{dep17}
\alias{dep17}
\docType{data}

\title{Shapefile des communes de Charente-Maritime}

\description{Extraction de la base de données GEOFLA® Communes, édition 2011, produite par l'IGN. 

Seules les communes de Charente-Maritime (17) sont représentées.}

\usage{dep17}

\format{
Un object de classe \link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}, contenant la table d'attributs suivante:
  \describe{
    \item{ID_GEOFLA:}{Identifiant unique du polygone.}
    \item{INSEE_COM:}{Numéro INSEE de la commune.}
    \item{NOM_COMM:}{Nom de la commune.}
    \item{CODE_CANT:}{Numéro du canton de la commune.}
  }
}
\details{La projection utilisée est la projection Lambert II étendu (epsg:27572).}

\source{\url{http://professionnels.ign.fr/adminexpress}}

\references{\url{https://www.data.gouv.fr/fr/datasets/code-officiel-geographique-cog/}}

\seealso{
\link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}
}

\examples{
\dontrun{
library(sp)
summary(dep17)
}
}

\keyword{datasets}
