\name{#PKGNAME#PFAM2NAME}
\alias{#PKGNAME#PFAM2NAME}
\title{Map Pfam Identifiers to Name}
\description{
   #PKGNAME#PFAM2NAME is an R environment that provides mappings of Pfam ID
   to their name.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#PFAM2NAME)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

