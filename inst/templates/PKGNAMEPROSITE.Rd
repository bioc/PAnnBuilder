\name{#PKGNAME#PROSITE}
\alias{#PKGNAME#PROSITE}
\title{Map Protein IDs to PROSITE ID}
\description{
  #PKGNAME#PROSITE is an R environment that provides mappings between a
 protein identifier and the associated PROSITE identifiers.
}
\details{
  Each protein identifier maps to a named vector of PROSITE identifiers. 
  
  Mappings were derived from data provided by:
  #SOURCE#  
  
  Package built #DATE#
}
\references{  
  \url{http://www.expasy.ch/prosite/}
}
\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#PROSITE)
        if(length(xx) > 0){
         # randomly display 1 proteins
         sample(xx, 1)
        }
}
\keyword{datasets}

