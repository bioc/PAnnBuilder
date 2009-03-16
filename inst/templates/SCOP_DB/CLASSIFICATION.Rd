\name{#PREFIX#CLASSIFICATION}
\alias{#PREFIX#CLASSIFICATION}
\title{Map SCOP identifiers to domain classification}
\description{
   #PREFIX#CLASSIFICATION mapps SCOP entry identifier to a compact representation 
   of a SCOP domain classification.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#CLASSIFICATION)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

