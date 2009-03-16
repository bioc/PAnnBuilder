\name{#PKGNAME#MW}
\alias{#PKGNAME#MW}
\title{Map Protein Identifiers to its Molecular Weight}
\description{
  #PKGNAME#MW is an R environment that provides mappings between
  protein identifiers and its molecular weight. 
}
\details{
  Each protein identifier is mapped to the molecular weight.
  
  Mappings were based on data provided by:  
  #SOURCE#
  
  Package built #DATE#
}
\examples{
    # Convert the environment to a list
    xx <- as.list(#PKGNAME#MW)
    if(length(xx) > 0){
        # The molecular weight for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    }
}

\keyword{datasets}

