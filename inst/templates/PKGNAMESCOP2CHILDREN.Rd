\name{#PKGNAME#SCOP2CHILDREN}
\alias{#PKGNAME#SCOP2CHILDREN}
\title{Map SCOP Identifiers to their children}
\description{
   #PKGNAME#SCOP2CHILDREN is an R environment that provides mappings SCOP Identifiers 
   to their children.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#SCOP2CHILDREN)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

