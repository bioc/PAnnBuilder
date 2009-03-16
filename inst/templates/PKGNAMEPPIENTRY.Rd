\name{#PKGNAME#PPIENTRY}
\alias{#PKGNAME#PPIENTRY}
\title{Extract Protein-Protein Interaction Data Entry}
\description{
  #PKGNAME#PPIENTRY is an R environment that provides entry information 
  for protein-protein interactions.
}
\details{
  Each entry describe the protein-protein interaction extracted from
  public database. Usually eanch entry includes experiment description,
  interaction detection method, interactor-A, interactor-B, and so on.

  Mappings were based on data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
     # Convert to a list
     xx <- as.list(#PKGNAME#PPIENTRY)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the first five interaction information
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

