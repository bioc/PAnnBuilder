\name{#PREFIX#PDB2FA}
\alias{#PREFIX#PDB2FA}
\title{Map PDB identifier to SCOP family identifier}
\description{
   #PREFIX#PDB2FA mapps a PDB identifier to a vetor of SCOP family identifier.
   The vector name corresponds to the PDB chains.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PDB2FA)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

