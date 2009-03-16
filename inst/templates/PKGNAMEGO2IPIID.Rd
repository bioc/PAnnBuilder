\name{#PKGNAME#GO2IPIID}
\alias{#PKGNAME#GO2IPIID}
\title{Map Gene Ontology (GO) to IPI Protein Identifiers}
\description{
   #PKGNAME#GO2IPIID is an R environment that provides mappings between
   GO terms and IPI protein identifiers.
}
\details{
  Each GO term maps to a vector of IPI protein identifiers. 
   
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO2IPIID)
    if(length(xx) > 0){
        # Get the first ten ones
        xx[1:10]
    }
}
\keyword{datasets}

