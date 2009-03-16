\name{#PKGNAME#PEPMAP}
\alias{#PKGNAME#PEPMAP}
\title{Map Peptide to Chromosomal Coordinates}
\description{
  #PKGNAME#PEPMAP is an R environment that provides mappings from a
 peptide identifier to their coding sequence and chromosomal coordinates. 
}
\details{
  Each peptide identifier maps to their coding sequence and chromosomal coordinates. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#PEPMAP)
        # randomly display 10 peptides
        sample(xxx, 10)
}
\keyword{datasets}

