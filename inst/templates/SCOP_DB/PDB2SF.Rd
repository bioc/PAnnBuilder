\name{#PREFIX#PDB2SF}
\alias{#PREFIX#PDB2SF}
\title{Map PDB identifier to SCOP superfamily identifier}
\description{
   #PREFIX#PDB2SF mapps a PDB identifier to a vetor of SCOP superfamily identifier.
   The vector name corresponds to the PDB chains.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PDB2SF)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

