\name{#PKGNAME#UPPAIR}
\alias{#PKGNAME#UPPAIR}
\title{Protein-Protein Interaction List}
\description{
  #PKGNAME#UPPAIR is an R environment that gives Protein-Protein interactions.
}
\details{
  Each component describe the interaction parter of the given protein. More detail
  interaction information can be acquired from #PKGNAME#PPIENTRY.

  Mappings were based on data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
     # Convert to a list
     xx <- as.list(#PKGNAME#UPPAIR)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the first five interaction list
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

