\name{#PREFIX#PDB2SP}
\alias{#PREFIX#PDB2SP}
\title{Map PDB identifier to SCOP species identifier}
\description{
   #PREFIX#PDB2SP mapps a PDB identifier to a vetor of SCOP species identifier.
   The vector name corresponds to the PDB chains.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PDB2SP)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

