\name{#PKGNAME#SCOP2CLASSIFICATION}
\alias{#PKGNAME#SCOP2CLASSIFICATION}
\title{Map SCOP Identifiers to domain classification}
\description{
   #PKGNAME#SCOP2CLASSIFICATION is an R environment that provides mappings SCOP Identifiers 
   to a compact representation of a SCOP domain classification.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#SCOP2CLASSIFICATION)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

