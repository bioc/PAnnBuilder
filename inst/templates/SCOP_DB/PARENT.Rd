\name{#PREFIX#PARENT}
\alias{#PREFIX#PARENT}
\title{Map SCOP identifiers to their parents}
\description{
   #PREFIX#PARENT mapps SCOP entry identifier to their parents.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PARENT)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

