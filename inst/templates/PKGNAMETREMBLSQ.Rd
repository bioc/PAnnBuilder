\name{#PKGNAME#TREMBLSQ}
\alias{#PKGNAME#TREMBLSQ}
\title{Map TrEMBL Protein Identifier to the protein Sequence}
\description{
  #PKGNAME#TREMBLSQ is an R environment that provides mappings between a
  TrEMBL protein identifier and the protein Sequence. 
}
\details{
  Each TrEMBL protein identifier maps to the the protein Sequence. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#TREMBLSQ)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

