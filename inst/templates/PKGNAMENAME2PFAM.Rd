\name{#PKGNAME#NAME2PFAM}
\alias{#PKGNAME#NAME2PFAM}
\title{Map Pfam Identifiers to their name}
\description{
   #PKGNAME#NAME2PFAM is an R environment that provides mappings of Pfam 
   domain name to its ID.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#NAME2PFAM)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

