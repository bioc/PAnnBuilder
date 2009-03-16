\name{#PKGNAME#SQ}
\alias{#PKGNAME#SQ}
\title{Map Protein IDs to the protein Sequence}
\description{
  #PKGNAME#SQ is an R environment that provides mappings between a
 protein identifier and the protein Sequence. 
}
\details{
  Each protein identifier maps to the the protein Sequence. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#SQ)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

