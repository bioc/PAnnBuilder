\name{#PKGNAME#SPSQ}
\alias{#PKGNAME#SPSQ}
\title{Map Swiss-Prot Protein Identifier to the protein Sequence}
\description{
  #PKGNAME#SPSQ is an R environment that provides mappings between a
  Swiss-Prot protein identifier and the protein Sequence. 
}
\details{
  Each Swiss-Prot protein identifier maps to the the protein Sequence. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#SPSQ)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

