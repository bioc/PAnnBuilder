\name{#PREFIX#NAME}
\alias{#PREFIX#NAME}
\title{Map SCOP identifiers to name}
\description{
   #PREFIX#NAME mapps SCOP entry identifier to a "short name"
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#NAME)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

