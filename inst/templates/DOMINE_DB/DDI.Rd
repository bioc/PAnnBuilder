\name{#PREFIX#DDI}
\alias{#PREFIX#DDI}
\title{Domain-Domain interaction}
\description{
  #PREFIX#DDI gives known and predicted domain-domain interactions from DOMINE 
  database.
}
\details{
  Domain identifier is given as Pfam identifier. Each domain identifier maps to 
  a named vector containing interacted domins, The vector name corresponds to 
  the abbreviation of the source of domain-domain interaction.
  
  Mappings were based on data provided by: DOMINE (\url{#DOMINESOURCEURL#}) on
  #DOMINESOURCEDATE# 
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
