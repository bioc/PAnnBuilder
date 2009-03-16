\name{#PKGNAME#INTERPRO2NAME}
\alias{#PKGNAME#INTERPRO2NAME}
\title{Map InterPro Identifiers to their name}
\description{
   #PKGNAME#INTERPRO2NAME is an R environment that provides mappings of InterPro 
   ID to their name.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#INTERPRO2NAME)
    if(length(xx) > 0){        
        xx[1]
    }
}
\keyword{datasets}

