\name{#PKGNAME#GO2NAME}
\alias{#PKGNAME#GO2NAME}
\title{Annotation of GO Identifiers to their Names}
\description{
  This data set gives mappings between GO identifiers and their names.
}
\details{
  Mappings were based on data provided by:
  #SOURCE#

  Package built: #DATE#
}
\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#GO2NAME)
        xx <- xx[!is.na(xx)]
        if(length(xx) > 0){
            # Get the first one
            xx[[1]]
        }
}
\keyword{datasets}

