\name{geoWeights}
\alias{geoWeights}

\title{Calculs du poids des mailles au sein d'entités géographiques définies}

\description{
Afin de transférer des données météo spatialisées vers un référentiel différent de celui fourni, il convient 
de définir une matrice de poids entre le référentiel géographique de départ et celui d'arrivée.

Les données spatialisées fournies par Météo France sont initialement présentées (spatialisées) au niveau de la grille SAFRAN (\code{\link{safran}}). 

Cependant ce niveau géographique peut ne pas être le plus pertinent pour certaines analyses ou certaines activités de recherche.

La fonction \code{\link{geoWeights}} permet la production de cette matrice de poids nécessaire au passage du référentiel 
SAFRAN à un autre référentiel spatialisé (par exemple un zonage administratif existant). 
Cette matrice est le résultat d'un croisement (SIG) entre ces deux référentiels.
}

\usage{geoWeights(safran, geo, parallel = FALSE, parallel.var = NULL)}

\arguments{
  \item{safran}{Grille SAFRAN au format vectoriel. L'argument \code{safran} peut, au choix, être un 
  chemin valide (au format \code{character}) vers la couche shapefile des mailles SAFRAN, ou un objet 
  de classe \link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}.}
  
  \item{geo}{Couche géographique du niveau d'agrégation choisi. L'argument \code{geo} peut, au choix, être un 
  chemin valide (au format \code{character}) ou un objet de classe \link[sp:SpatialPolygonsDataFrame-class]{SpatialPolygonsDataFrame}.}
  
  \item{parallel}{Par défaut, le calcul s'effectue en mode séquentiel (\code{parallel = FALSE}). 
  Si \code{parallel = TRUE} alors le calcul du croisement entre les couches de départ (\code{safran}) et 
  d'arrivée (\code{geo}) se fait de manière parallèle, sur la variable \code{parallel.var}. 
  Le nombre de cœurs retenus pour le calcul parallèle est égal au {nombre de cœurs disponibles - 1}. 
  Dans le cas où le nombre de cœurs disponibles = 1, alors le calcul s'effectue en mode séquentiel.}
  
  \item{parallel.var}{Nom de la variable sur laquelle s'effectuera la parallélisation du calcul. Utilisé seulement si \code{parallel = TRUE}.}
}

\details{Lorsque le calcul concerne peu de mailles SAFRAN ou peu d'entités géographique de croisement, 
alors un calcul en mode séquentiel (\code{parallel = FALSE}) s'avère souvent plus rapide qu'en mode parallèle.}

\value{La fonction \code{\link{geoWeights}} retourne un dataframe de classe \verb{'data.frame'}.

Pour chacune des entités résultant du croisement entre le référentiel de départ (\code{safran}) et celui 
d'arrivée (\code{geo}), le dataframe contient:
  \itemize{
    \item L'ensemble des variables contenues dans \code{safran};
    \item L'ensemble des variables contenues dans \code{geo}; 
    \item Et une variable \strong{GeoPoidsSafran}, contenant la pondération du croisement SIG \code{safran} × \code{geo}. Note: 
          La somme des poids alloués aux différentes unités de \code{safran} à l'intérieur d'une même entité de \code{geo} est égale à 1.
  }
}

\author{Yann Desjeux}


\note{A toutes fins utiles, la matrice de poids issue des croisements entre les mailles SAFRAN et l'ensemble des 
communes françaises (France métropolitaine) selon le référentiel 2011 est disponible dans le répertoire 'exemple' du package.
}

\seealso{
\code{\link[rgdal]{readOGR}}

\code{\link{readMFfiles}}

\code{\link{safran}}

\code{\link{dep17}}
}

\examples{
## Pondération des différentes mailles SAFRAN au sein des communes du département de la 
## Charente-Maritime
\dontrun{
poids_dep17 <- geoWeights(safran = safran, geo = dep17)
}

## Pondération sur l'ensemble des communes françaises (France métropolitaine) telles que 
## définies par le Code Officiel Géographique (COG) de 2011:
\dontrun{
path <- system.file("exemple", "Poids_Communes_Safran_2011.csv", package = "meteoRIT")
Poids_Communes_Safran_2011 <- read.table(path, sep = ";", dec = ".", header = TRUE)
}
}

\keyword{manip}
\keyword{data}
