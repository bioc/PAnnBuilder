\name{#PKGNAME#GO2TERM}
\alias{#PKGNAME#GO2TERM}
\title{Annotation of GO Identifiers to GO Terms}
\description{
  This data set gives mappings between GO identifiers and their respective terms.
}
\details{
  Each GO identifier is mapped to a object that has 6 slots:
  \item{GOID}{GO Identifier}
  \item{Term}{The term for that GO id}
  \item{Synonym}{Synonymous terms}
  \item{Secondary}{Secondary terms that have been merged into this term}
  \item{Definition}{Further definition of the GO term}
  \item{Ontology}{One of MF - molecular function, BP - biological
    process, or CC - cellular component}
 
  All the obsolete GO terms are under the nodes "obsolete molecular
  function" (GO:0008369), "obsolete cellular component" (GO id
  GO:0008370), and "obsolete biological process" (GO:0008371). Each of
  these GO identifiers has a group of GO identifiers as their direct children with GO
  terms that were defined by GO but are
  deprecated in the current build. These deprecated GO terms were
  appended by "(obsolete)" when the data package was built. 

  Mappings were based on data provided by:
  #SOURCE#

  Package built: #DATE#
}
\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#GO2TERM)
        xx <- xx[!is.na(xx)]
        if(length(xx) > 0){
            # Get the first one
            xx[[1]]
        }
}
\keyword{datasets}

