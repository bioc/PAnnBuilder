\name{#PKGNAME#PEPSQ}
\alias{#PKGNAME#PEPSQ}
\title{Map peptide IDs to the peptide Sequence}
\description{
  #PKGNAME#PEPSQ is an R environment that provides mappings between a
 peptide identifier and the peptide Sequence. 
}
\details{
  Each peptide identifier maps to the the peptide Sequence. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#PEPSQ)
        # randomly display 10 peptides
        sample(xxx, 10)
}
\keyword{datasets}

