\name{#PKGNAME#GO2PARENTS}
\alias{#PKGNAME#GO2PARENTS}
\title{Map Gene Ontology(GO) terms to their Parents}
\description{
  #PKGNAME#GO2PARENTS describes associations between GO 
  terms and their direct parent terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  environment mapping the GO terms to all direct parent terms, where a
  direct parent term is a more general GO term that immediately precedes
  the given GO term in the DAG.
}
\details{
  Each GO term is mapped to their direct parent GO terms. The name
  associated with the parent term will be either \emph{isa}, \emph{hasa} or \emph{partof},
  where \emph{isa} indicates that the child term is a more specific version
  of the parent, and \emph{hasa} and \emph{partof} indicate that the
  child term is a part of the parent. For example, a telomere is part of
  a chromosome.
   
  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO2PARENTS)
    if(length(xx) > 0){
        xx[[1]]
        names(xx[[1]])
    }
}
\keyword{datasets}

