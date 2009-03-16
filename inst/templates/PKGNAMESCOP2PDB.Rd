\name{#PKGNAME#SCOP2PDB}
\alias{#PKGNAME#SCOP2PDB}
\title{Map SCOP Identifiers to PDB Identifiers}
\description{
   #PKGNAME#SCOP2PDB is an R environment that provides mappings between
   SCOP identifiers and PDB identifiers.
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
    xx <- as.list(#PKGNAME#SCOP2PDB)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
\keyword{datasets}

