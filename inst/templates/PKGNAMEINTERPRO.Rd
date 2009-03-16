\name{#PKGNAME#INTERPRO}
\alias{#PKGNAME#INTERPRO}
\title{Map Manufacturer IDs to Interpro IDs}
\description{
  #PKGNAME#INTERPRO is an R environment that provides mappings between a
 protein identifier and the associated Interpro identifiers.
}
\details{
  Each protein identifier maps to a vector of InterPro identifiers.  
  
  Mappings were derived from data provided by:
  #SOURCE#	
  
  Package built #DATE#
}
\references{
  \url{http://www.ebi.ac.uk/interpro/}
}
\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#INTERPRO)
        if(length(xx) > 0){
         # randomly display 1 proteins
         sample(xx, 1)
        }
}
\keyword{datasets}

