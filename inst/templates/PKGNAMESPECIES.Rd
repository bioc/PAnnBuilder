\name{#PKGNAME#SPECIES}
\alias{#PKGNAME#SPECIES}
\title{Map Protein to the species scientific name}
\description{
  #PKGNAME#SPECIES is an R environment that provides mappings between a
  protein and the species scientific name. 
}
\details{ 
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#SPECIES)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

