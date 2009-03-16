\name{#PKGNAME#SUBCELLULAR2SPAC}
\alias{#PKGNAME#SUBCELLULAR2SPAC}
\title{Map SUBCELLULAR Location to SwissProt Acession Number}
\description{
  #PKGNAME#SUBCELLULAR2SPAC is an R environment that provides mappings between a
  protein SUBCELLULAR Location and the SwissProt Acession Number. 
}
\details{
  Each Subcellular Location is mapped to a vector of SwissProt protein acession number. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#SUBCELLULAR2SPAC)
        # randomly display 2 subcellular location
        sample(xxx, 2)
}
\keyword{datasets}

