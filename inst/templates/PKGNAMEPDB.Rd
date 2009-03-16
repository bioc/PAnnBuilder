\name{#PKGNAME#PDB}
\alias{#PKGNAME#PDB}
\title{Map Protein IDs to PDB IDs}
\description{
  #PKGNAME#PDB is an R environment that provides mappings between a
 protein identifier and the associated Protein Data Bank (PDB) identifiers.
}
\details{
  Each protein identifier maps to a named vector of PDB identifiers.  
  
  Mappings were derived from data provided by:
  #SOURCE#

  Package built #DATE# 
}
\references{
  \url{http://www.rcsb.org/pdb/home/home.do}
}
\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#PDB)
        # randomly display 10 proteins
        sample(xxx, 10)
}
\keyword{datasets}

