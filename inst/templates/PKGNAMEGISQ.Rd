\name{#PKGNAME#GISQ}
\alias{#PKGNAME#GISQ}
\title{Map GI of Refseq Protein to the protein Sequence}
\description{
  #PKGNAME#GISQ is an R environment that provides mappings between a
  GI of refseq protein  and the protein Sequence. 
}
\details{
  Each protein GI identifier maps to the the protein Sequence. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#GISQ)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

