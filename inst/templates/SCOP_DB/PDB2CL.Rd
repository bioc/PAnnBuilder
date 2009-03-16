\name{#PREFIX#PDB2CL}
\alias{#PREFIX#PDB2CL}
\title{Map PDB identifier to SCOP class identifier}
\description{
   #PREFIX#PDB2CL mapps a PDB identifier to a vetor of SCOP class identifier. The 
   vector name corresponds to the PDB chains.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PDB2CL)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

