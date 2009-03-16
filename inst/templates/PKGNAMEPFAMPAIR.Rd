\name{#PKGNAME#PFAMPAIR}
\alias{#PKGNAME#PFAMPAIR}
\title{Domain-Domain Interaction List}
\description{
  #PKGNAME#PFAMPAIR is an R environment that gives Domain-Domain interactions.
}
\details{
  Each component describe the interaction parter of the given domain. Domain 
  identifier is given as Pfam identifier. More detail interaction information 
  can be acquired from #PKGNAME#DDIENTRY.

  Mappings were based on data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
     # Convert to a list
     xx <- as.list(#PKGNAME#PFAMPAIR)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the first five interaction list
         xx[[1]]
     }
}
\keyword{datasets}

