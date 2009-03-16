\name{#PREFIX#PPI}
\alias{#PREFIX#PPI}
\title{Protein-Protein interaction}
\description{
  #PREFIX#PPI gives protein-protein interactions from MPPI database.
}
\details{
  Protein is is given as Uniprot protein accession number.
  
  Mappings were based on data provided by: MPPI (\url{#MPPISOURCEURL#}) on
  #MPPISOURCEDATE# 
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#PPI)     
     if(length(xx) > 0){
         # Gets the first five interaction list
         xx[1:5]
     }
}
