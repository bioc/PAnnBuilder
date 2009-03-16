\name{#PREFIX#PDB2PX}
\alias{#PREFIX#PDB2PX}
\title{Map PDB identifier to SCOP domain entry identifier}
\description{
   #PREFIX#PDB2PX mapps a PDB identifier to a vetor of SCP domain entry identifier.
   The vector name corresponds to the PDB chains.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PDB2PX)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

