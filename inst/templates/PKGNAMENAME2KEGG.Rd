\name{#PKGNAME#NAME2KEGG}
\alias{#PKGNAME#NAME2KEGG}
\title{Map KEGG Pathway Name to its Identifier}
\description{
   #PKGNAME#NAME2KEGG is an R environment that provides mappings of KEGG 
   pathway name to its ID.
}
\details{ 
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#NAME2KEGG)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

