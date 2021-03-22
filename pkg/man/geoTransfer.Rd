\name{geoTransfer}
\alias{geoTransfer}
\title{Transfert de données météo "à la maille"  vers un référentiel géographique différent}

\description{
La fonction \code{\link{geoTransfer}} permet de transférer des données météo spatialisées vers un 
référentiel différent de celui fourni.

Sur la base d'une matrice de correspondance (pondérations) générée par \code{\link{geoWeights}}, des données 
spatialisées sur la grille SAFRAN peuvent, par exemple, être transférées vers un référentiel géographique plus 
pertinent (ex: zonage administratif).
}

\usage{geoTransfer(meteoM, poids, var.maille, var.met, var.poids, var.geo, var.temp)}

\arguments{
  \item{meteoM}{Objet de classe \verb{'data.frame'} contenant les données météo spatialisées "à la maille SAFRAN" 
  (cf. \code{\link{readMFfiles}}).}
  
  \item{poids}{Objet de classe \verb{'data.frame'} contenant les différents poids issus du croisement 
  entre le référentiel géographique initial (maille SAFRAN) et le référentiel géographique souhaité 
  (cf. \code{\link{geoWeights}}).}
  
  \item{var.maille}{Variables (nom au format \verb{'character'}) représentant les mailles SAFRAN dans,
  respectivement, les arguments \code{meteoM} et \code{poids}.}
  
  \item{var.met}{Variables météo (nom au format \verb{'character'} ou  index au format \verb{'numeric'}) 
  à transférer. Les variables \code{var.met} doivent être des éléments de \code{meteoM}.}
  
  \item{var.poids}{Variable (nom au format \verb{'character'}) représentant la pondération à utiliser issue 
  du croisement SIG (cf. variable \code{'GeoPoidsSafran'} produite par \code{\link{geoWeights}}). 
  \code{var.poids} doit être une variable de \code{poids}.}
  
  \item{var.geo}{Variable (nom au format \verb{'character'}) représentant l'unité géographique vers 
  laquelle effectuer le transfert. \code{var.geo} doitêtre une variable de \code{poids}.}
  
  \item{var.temp}{Variable (nom au format \verb{'character'}) représentant la temporalité 
  des données météo. \code{var.temp} doit être une variable de \code{meteoM}.}
}

\details{Les données \code{meteoM} et \code{poids} sont classiquement générées à l'aide des fonctions \code{\link{readMFfiles}} et \code{\link{geoWeights}}, respectivement.}

\value{
\code{\link{geoTransfer}} retourne un objet de classe \verb{'data.frame'} comportant:
\itemize{
  \item La variable identifiant l'unité géographique vers laquelle ont été transférées les données (\code{var.geo}). 
  \item La variable représentant le pas de temps utilisé (\code{var.temp}).
  \item L'ensemble des données définies dans \code{var.met}, transférées de la maille vers \code{var.geo} et dont la dimension temporelle \code{var.temp} est conservée.
}
}

\author{Yann Desjeux}

\note{L'application classique de \code{\link{geoTransfer}} est le transfert de données météo, fournies à la maille SAFRAN, 
vers un référentiel géographique administratif (ex: communes).

A toutes fins utiles, la matrice de poids (\code{poids}) issue des croisements entre les mailles SAFRAN et l'ensemble des 
communes françaises (France métropolitaine) selon le référentiel 2011 est disponible dans le répertoire 'exemple' du package.
}

\seealso{
\code{\link{readMFfiles}}

\code{\link{geoWeights}}
}

\examples{
\dontrun{
path <- system.file("exemple", "Poids_dep17.csv", package = "meteoRIT")
poids17 <- read.table(path, sep = ";", dec = ".", header = TRUE)
Meteo_Communes17 <- geoTransfer(meteoM = meteo17, poids = poids17, 
var.maille = c("numero_de_maille", "maille"), var.met = c("temp_moyenne", 
"temp_min", "temp_max", "precip_tot"), var.poids = "PoidsGeoSafran", 
var.geo = "INSEE_COM", var.temp = "mois")

head(Meteo_Communes17)
summary(Meteo_Communes17)
}
}

\keyword{manip}

\keyword{data}
