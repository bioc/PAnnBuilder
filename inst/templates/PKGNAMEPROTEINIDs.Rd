\name{#PKGNAME#PROTEINIDs}
\alias{#PKGNAME#PROTEINIDs}
\title{Map Protein to identifiers}
\description{
  #PKGNAME#PROTEINIDs is an R environment that maps protein to diverse protein 
  identifiers of different protein databases.  
}
\details{  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#PROTEINIDs)
        if(length(xx) > 0){
         # randomly display 1 proteins
         sample(xx, 1)
        }
}
\keyword{datasets}

