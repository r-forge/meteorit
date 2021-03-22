\name{readMFfiles}
\alias{readMFfiles}

\title{Lecture des fichiers fournis par Météo France}

\description{
Par l'intermédiaire de sa "Convention Recherche", Météo France met à disposition des données météo spatialisées 
et présentées sur la grille SAFRAN. Cela représente la mise à disposition d'un fichier de données par maille SAFRAN, 
soit 9892 fichiers pour l'ensemble de la grille. Ces fichiers de données sont fournis sous une forme compressée au 
format \code{*.gz}; la forme décompressée étant au format \code{*.csv}. Ainsi, selon le nombre de fichiers à traiter 
il peut assez rapidement s'avérer très fastidieux de traiter un par un l'ensemble des fichiers fournis.

La fonction \code{\link{readMFfiles}} permet de lire en une seule fois l'ensemble des fichiers contenus dans 
un répertoire donné, et de les retourner à l'utilisateur sous la forme d'un dataframe classique qui pourra 
ainsi être facilement exploité par la suite.}

\usage{readMFfiles(path = ".", type = c("gz", "csv"), progress = FALSE)}

\arguments{
  \item{path}{Chemin d'accès du répertoire contenant les fichiers météo à traiter. Le répertoire 
  renseigné comme répertoire de travail, \code{\link{getwd}()}, est celui utilisé par défaut.}
  \item{type}{Type de fichiers qu'il convient d'ouvrir. Fichiers compressés \code{*.gz} (par défaut) 
  ou fichiers décompressés \code{*.csv}. Tous les fichiers contenus dans \code{path} doivent avoir le même format.}
  \item{progress}{\code{FALSE} par défaut. Si \code{TRUE}, alors un compteur montrant l'évolution du 
  processus de traitement des fichiers s'affiche. Cette dernière option (\code{progress = TRUE}) est 
  utile lorsque le nombre de fichiers à lire est important.}
}

\value{
La fonction retourne un objet de classe \verb{'data.frame'} contenant l'ensemble des variables 
présentes dans les fichiers initiaux.
}

\author{Yann Desjeux}

\note{
Lorsque le nombre de fichiers à lire est important (au delà du millier), il y a généralement un gain de temps 
significatif à appeler la fonction \code{\link{readMFfiles}} sur des fichiers \code{*.gz} plutôt que sur leur 
format décompressé \code{*.csv}.
}

\keyword{manip}

\keyword{data}
