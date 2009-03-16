\name{#PKGNAME#SUBCELLULAR2SPID}
\alias{#PKGNAME#SUBCELLULAR2SPID}
\title{Map SUBCELLULAR Location to SwissProt Identifier}
\description{
  #PKGNAME#SUBCELLULAR2SPID is an R environment that provides mappings between a
  protein SUBCELLULAR Location and the SwissProt Identifier. 
}
\details{
  Each Subcellular Location is mapped to a vector of SwissProt protein identifier. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#SUBCELLULAR2SPID)
        # randomly display 2 subcellular location
        sample(xxx, 2)
}
\keyword{datasets}

