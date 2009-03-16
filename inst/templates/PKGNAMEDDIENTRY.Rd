\name{#PKGNAME#DDIENTRY}
\alias{#PKGNAME#DDIENTRY}
\title{Extract Domain-Domain Interaction Data Entry}
\description{
  #PKGNAME#DDIENTRY is an R environment that provides entry information 
  for Domain-Domain interactions.
}
\details{
  Each entry describe the Domain-Domain interaction extracted from
  public database. Usually eanch entry includes domain-A, domain-B, 
  and so on.

  Mappings were based on data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
     # Convert to a list
     xx <- as.list(#PKGNAME#DDIENTRY)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the first five interaction information
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

