\name{#PKGNAME#PFAM}
\alias{#PKGNAME#PFAM}
\title{Map Protein IDs to Pfam IDs}
\description{
  #PKGNAME#PFAM is an R environment that provides mappings between a
 protein identifier and the associated Pfam identifiers.
}
\details{
  Each protein identifier maps to a vector of Pfam identifiers.  
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#  
}
\references{
  \url{http://pfam.janelia.org/}
}
\examples{
        # Convert to a list
        xx <- as.list(#PKGNAME#PFAM)
        if(length(xx) > 0){
         # randomly display 1 proteins
         sample(xx, 1)
        }
}
\keyword{datasets}

