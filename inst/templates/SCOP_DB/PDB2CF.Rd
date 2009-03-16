\name{#PREFIX#PDB2CF}
\alias{#PREFIX#PDB2CF}
\title{Map PDB identifier to SCOP fold entry}
\description{
   #PREFIX#PDB2CF mapps a PDB identifier to a vetor of SCOP fold entry. The 
   vector name corresponds to the PDB chains.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PDB2CF)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

