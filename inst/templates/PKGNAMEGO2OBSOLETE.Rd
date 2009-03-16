\name{#PKGNAME#GO2OBSOLETE}
\alias{#PKGNAME#GO2OBSOLETE}
\title{Annotation of GO identifiers by terms defined by Gene Ontology
  Consortium and their status are obsolete}
\description{
This is an R environment (hash table) mapping GO identifiers to the specific
terms in defined by Gene Ontology Consortium and their definition are obsolete
}
\details{
  All the obsolete GO terms are collected in this file will no longer exist 
  on other environments. 

  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO2OBSOLETE)
    if(length(xx) > 0){
        xx[[1]]
        names(xx[[1]])
    }
}
\keyword{datasets}

