\name{#PKGNAME#SCOP2PARENT}
\alias{#PKGNAME#SCOP2PARENT}
\title{Map SCOP Identifiers to their parents}
\description{
   #PKGNAME#SCOP2PARENT is an R environment that provides mappings SCOP Identifiers 
   to their parents.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#SCOP2PARENT)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

