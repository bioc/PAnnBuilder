\name{#PKGNAME#GO2SPID}
\alias{#PKGNAME#GO2SPID}
\title{Map Gene Ontology(GO) Terms to SwissProt Protein Identifiers}
\description{
   #PKGNAME#GO2SPID is an R environment that provides mappings between
   GO identifiers and SwissProt protein identifiers.
}
\details{
  Each GO term maps to a vector of SwissProt protein identifiers. 
   
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO2SPID)
    if(length(xx) > 0){
        # Get the first ten ones
        xx[1:10]
    }
}
\keyword{datasets}

