\name{#PKGNAME#SCOP2NAME}
\alias{#PKGNAME#SCOP2NAME}
\title{Map SCOP Identifiers to a "short name"}
\description{
   #PKGNAME#SCOP2NAME is an R environment that provides mappings SCOP Identifiers 
   to a "short name".
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#SCOP2NAME)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

