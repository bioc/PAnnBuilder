\name{#PKGNAME#IPISQ}
\alias{#PKGNAME#IPISQ}
\title{Map IPI Protein Identifier to the protein Sequence}
\description{
  #PKGNAME#IPISQ is an R environment that provides mappings between a
  IPI protein identifier and the protein Sequence. 
}
\details{
  Each IPI protein identifier maps to the the protein Sequence. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#IPISQ)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

