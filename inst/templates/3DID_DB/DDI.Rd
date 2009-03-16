\name{#PREFIX#DDI}
\alias{#PREFIX#DDI}
\title{Domain-Domain interaction}
\description{
  #PREFIX#DDI gives domain-domain interactions from 3DID database.
}
\details{
  Domain identifier is given as Pfam identifier.
  
  Mappings were based on data provided by: 3DID (\url{#DIDSOURCEURL#}) on
  #DIDSOURCEDATE# 
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#DDI)     
     if(length(xx) > 0){
         # Gets the first five interaction list
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}
