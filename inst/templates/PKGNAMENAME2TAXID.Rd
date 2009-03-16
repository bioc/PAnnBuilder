\name{#PKGNAME#NAME2TAXID}
\alias{#PKGNAME#NAME2TAXID}
\title{Map Taxonomy Name to Identifier}
\description{
   #PKGNAME#NAME2TAXID is an R environment that provides mappings of taxonomy
   name to its id.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#NAME2TAXID)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

