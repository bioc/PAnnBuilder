\name{#PKGNAME#PDB2SCOP}
\alias{#PKGNAME#PDB2SCOP}
\title{Map PDB Identifiers to SCOP Identifiers}
\description{
   #PKGNAME#PDB2SCOP is an R environment that provides mappings between
   PDB identifiers and SCOP identifiers.
}
\details{
  PDB: Protein Data Bank, \url{http://www.rcsb.org/pdb/home/home.do}. 
  SCOP: Structural Classification of Proteins, \url{http://scop.mrc-lmb.cam.ac.uk/scop/}.
   
  Mappings were based on data provided by:
  #SOURCE#
  #GO#

  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#PDB2SCOP)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

