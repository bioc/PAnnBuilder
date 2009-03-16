\name{#PKGNAME#FUNCTION}
\alias{#PKGNAME#FUNCTION}
\title{Map Protein IDs to the description of general description of the Function(s) of a protein}
\description{
  #PKGNAME#FUNCTION is an R environment that provides mappings between a
 protein identifier and general description of the Function(s) of a protein. 
}
\details{
  Each protein identifier maps to the general description of the Function(s) of a protein. 
  This information is extractd from "FUNCTION" topic in SwissProt "comments" lines. 
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built: #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#FUNCTION)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

