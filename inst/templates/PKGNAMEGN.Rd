\name{#PKGNAME#GN}
\alias{#PKGNAME#GN}
\title{Map Protein IDs and Gene Names}
\description{
  #PKGNAME#GN is an R environment that maps protein
  identifiers to the corresponding gene names.
}
\details{
  Each protein identifier maps to a named vector containing the gene
  name. The vector name corresponds to the protein identifier. If the gene
  name is unknown, the vector will contain an \code{NA}.  
  Gene names currently include both the official (validated by a
  nomenclature committee) and preferred names (interim selected for
  display) for genes. Efforts are being made to differentiate the two
  by adding a name to the vector.

  Mappings were based on data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
     # Convert to a list
     xx <- as.list(#PKGNAME#GN)
     # Remove protein that do not map to any GENENAME
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the gene names for the first five probe identifiers
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

