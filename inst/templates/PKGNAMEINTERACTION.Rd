\name{#PKGNAME#INTERACTION}
\alias{#PKGNAME#INTERACTION}
\title{An annotation data file that maps protein identifiers to
 proteins with interaction }
\description{
  #PKGNAME#INTERACTION maps protein identifiers to interaction proteins   
}
\details{  

  Mappings were based on data provided by:
  #SOURCE#

  Package built: #DATE#
}

\examples{
    xx <- as.list(#PKGNAME#INTERACTION)
    if(length(xx) > 0){
        # Get the value of the first key
        xx[[1]]
        # Get the values for multiget for a few keys
        if(length(xx) >= 3){
   	    xx[1:3]
        }
    }
}
\keyword{datasets}

