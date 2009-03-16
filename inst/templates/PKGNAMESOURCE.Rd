\name{#PKGNAME#SOURCE}
\alias{#PKGNAME#SOURCE}
\title{Data Source}
\description{
  #PKGNAME#SOURCE is an R environment that gives the source of data.
}
\details{
  Mappings were based on data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
     # Convert to a list
     xx <- as.list(#PKGNAME#SOURCE)
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){         
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

