\name{#PKGNAME#TAXID2NAME}
\alias{#PKGNAME#TAXID2NAME}
\title{Map Taxonomy Identifiers to Name}
\description{
   #PKGNAME#TAXID2NAME is an R environment that provides mappings of taxonomy
   id to its name.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#TAXID2NAME)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

