\name{#PKGNAME#KEGG2NAME}
\alias{#PKGNAME#KEGG2NAME}
\title{Map KEGG Pathway Identifiers to Name}
\description{
   #PKGNAME#KEGG2NAME is an R environment that provides mappings of KEGG 
   pathway ID to their name.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#KEGG2NAME)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

