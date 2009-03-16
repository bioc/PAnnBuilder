\name{#PKGNAME#EC}
\alias{#PKGNAME#EC}
\title{Map Protein IDs to EC number}
\description{
  #PKGNAME#EC is an R environment that provides mappings between a
 protein identifier and the Enzyme Commission number (EC number). 
}
\details{
  Each protein identifier maps to a vector of EC number. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#EC)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

