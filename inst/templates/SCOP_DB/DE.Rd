\name{#PREFIX#DE}
\alias{#PREFIX#DE}
\title{Map SCOP identifiers to their description}
\description{
   #PREFIX#DE mapps SCOP entry identifier to their description.
}
\details{
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#DE)
    if(length(xx) > 0){        
        xx[2:3]
    }
}

