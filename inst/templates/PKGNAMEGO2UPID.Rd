\name{#PKGNAME#GO2UPID}
\alias{#PKGNAME#GO2UPID}
\title{Map Gene Ontology (GO) to Uniprot Protein Identifiers}
\description{
   #PKGNAME#GO2UPID is an R environment that provides mappings between
   GO identifiers and protein identifiers.
}
\details{
  Each GO term maps to a vector of UniProt protein identifiers. 
   
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO2UPID)
    if(length(xx) > 0){
        # Get the first ten ones
        xx[1:10]
    }
}
\keyword{datasets}

