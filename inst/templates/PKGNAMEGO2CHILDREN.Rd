\name{#PKGNAME#GO2CHILDREN}
\alias{#PKGNAME#GO2CHILDREN}
\title{Map Gene Ontology(GO) Terms to Their Children Terms}
\description{
  #PKGNAME#GO2CHILDREN describes associations between GO 
  terms and their direct children terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  environment mapping the GO terms to all direct children terms, where a
  direct child term is a more specific GO term that is immediately preceded
  by the given GO term in the DAG.
}
\details{
  Each GO term is mapped to a vector of children GO terms.
   
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO2CHILDREN)
    if(length(xx) > 0){
        xx[[1]]
        names(xx[[1]])
    }
}
\keyword{datasets}

