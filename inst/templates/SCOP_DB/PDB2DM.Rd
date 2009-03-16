\name{#PREFIX#PDB2DM}
\alias{#PREFIX#PDB2DM}
\title{Map PDB identifier to SCOP protein domain identifier}
\description{
   #PREFIX#PDB2DM mapps a PDB identifier to a vetor of SCOP protein domain identifier.
   The vector name corresponds to the PDB chains.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PDB2DM)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

