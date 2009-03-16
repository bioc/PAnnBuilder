\name{#PKGNAME#SCOP2TYPE}
\alias{#PKGNAME#SCOP2TYPE}
\title{Map SCOP Identifiers to their Types}
\description{
   #PKGNAME#SCOP2TYPE is an R environment that provides mappings between
   SCOP identifiers and SCOP entry types.
}
\details{
	Possible entry types are:
	cl - class 
	cf - fold
	sf - superfamily
	fa - family
	dm - protein domain
	sp - species
	px - domain entry
   
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#SCOP2TYPE)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

