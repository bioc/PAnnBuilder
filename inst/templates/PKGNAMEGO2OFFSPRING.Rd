\name{#PKGNAME#GO2OFFSPRING}
\alias{#PKGNAME#GO2OFFSPRING}
\title{Map Gene Ontology(GO) Terms to Their Offspring}
\description{
  #PKGNAME#GO2OFFSPRING describes associations between GO 
  terms and their offspring terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  environment mapping the GO terms to all offspring terms, where an
  ancestor term is a more specific GO term that is preceded
  by the given GO term in the DAG (in other words, the children and all
  their children, etc.).
}
\details{
  Each GO term is mapped to a vector of offspring GO terms.
   
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO2OFFSPRING)
    if(length(xx) > 0){
        xx[[1]]
        names(xx[[1]])
    }
}
\keyword{datasets}

