\name{#PREFIX#CHILDREN}
\alias{#PREFIX#CHILDREN}
\title{Map SCOP identifiers to their children}
\description{
   #PREFIX#CHILDREN mapps SCOP entry identifier to their children.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#CHILDREN)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

