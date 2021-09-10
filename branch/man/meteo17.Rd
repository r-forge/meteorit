\name{meteo17}
\alias{meteo17}
\docType{data}

\title{Données du département de la Charente-Maritime}

\description{Jeu de données météo fictif, spatialisé à la maille SAFRAN pour le département de la Charente-Maritime. 

L'emprise temporelle des données est de un an (12 mois).}

\usage{meteo17}
\format{
Un objet de classe \verb{'data.frame'} contenant 1860 observations (155 mailles SAFRAN x 12 mois) et 6 variables: 
\describe{
  \item{numero_de_maille:}{Numéro de maille SAFRAN.}
  \item{mois:}{Mois calendaire.}
  \item{temp_moyenne:}{Température moyenne observée (en °C).}
  \item{temp_min:}{Température minimale observée (en °C).}
  \item{temp_max:}{Température maximale observée (en °C).}
  \item{precip_tot:}{Précipitations totales relevées (en mm).}
}
}

\examples{
str(meteo17)
summary(meteo17)
}

\note{Les données fournies dans ce jeu de données sont factices.}

\keyword{datasets}
