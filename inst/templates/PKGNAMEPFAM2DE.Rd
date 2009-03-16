\name{#PKGNAME#PFAM2DE}
\alias{#PKGNAME#PFAM2DE}
\title{Map Pfam Identifiers to their description}
\description{
   #PKGNAME#PFAM2DE is an R environment that provides mappings Pfam Identifiers 
   to description.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#PFAM2DE)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

