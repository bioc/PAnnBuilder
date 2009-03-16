\name{#PKGNAME#GO2ANCESTOR}
\alias{#PKGNAME#GO2ANCESTOR}
\title{Map Gene Ontology(GO) Terms to Their Ancestors Terms}
\description{
  #PKGNAME#GO2ANCESTOR describes associations between GO 
  terms and their ancestor terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  environment mapping the GO terms to all ancestor terms, where an
  ancestor term is a more general GO term that precedes
  the given GO term in the DAG (in other words, the parents, and all
  their parents, etc.).
}
\details{
  Each GO term is mapped to a vector of ancestor GO terms.
   
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO2ANCESTOR)
    if(length(xx) > 0){
        xx[[1]]
        names(xx[[1]])
    }
}
\keyword{datasets}

