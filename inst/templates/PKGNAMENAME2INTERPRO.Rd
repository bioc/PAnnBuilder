\name{#PKGNAME#NAME2INTERPRO}
\alias{#PKGNAME#NAME2INTERPRO}
\title{Map InterPro Name to ID}
\description{
   #PKGNAME#NAME2INTERPRO is an R environment that provides mappings of InterPro 
   domain name to its identifier.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#NAME2INTERPRO)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

