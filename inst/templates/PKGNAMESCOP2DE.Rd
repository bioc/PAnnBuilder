\name{#PKGNAME#SCOP2DE}
\alias{#PKGNAME#SCOP2DE}
\title{Map SCOP Identifiers to their description}
\description{
   #PKGNAME#SCOP2DE is an R environment that provides mappings SCOP Identifiers 
   to description.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#SCOP2DE)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

