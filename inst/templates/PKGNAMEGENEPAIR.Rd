\name{#PKGNAME#GENEPAIR}
\alias{#PKGNAME#GENEPAIR}
\title{Gene-Gene Interaction List}
\description{
  #PKGNAME#GENEPAIR is an R environment that gives gene-gene interactions.
}
\details{
  Each component describe the interaction parter of the given gene. Gene 
  identifier is given as NCBI Gene ID. More detail interaction information 
  can be acquired from #PKGNAME#PPIENTRY.

  Mappings were based on data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
     # Convert to a list
     xx <- as.list(#PKGNAME#GENEPAIR)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the first five interaction list
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

