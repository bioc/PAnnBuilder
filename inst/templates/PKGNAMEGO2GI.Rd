\name{#PKGNAME#GO2GI}
\alias{#PKGNAME#GO2GI}
\title{Map Gene Ontology(GO) Terms to NCBI Protein GI}
\description{
   #PKGNAME#GO2GI is an R environment that provides mappings between
   GO identifiers and NCBI Protein GI.
}
\details{
  Each GO term maps to a vector of NCBI protein GIs. 
   
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO2GI)
    if(length(xx) > 0){
        # Get the first ten ones
        xx[1:10]
    }
}
\keyword{datasets}

