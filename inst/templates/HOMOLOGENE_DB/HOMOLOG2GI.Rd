\name{#PREFIX#HOMOLOG2GI}
\alias{#PREFIX#HOMOLOG2GI}
\title{Tax identifier}
\description{
  #PREFIX#HOMOLOG2GI map homolog group identifier to NCBI protein GI.
}
\details{
  Mappings were based on data provided by:HomoloGene (\url{#HOMOLOGENESOURCEURL#}) on
  #HOMOLOGENESOURCEDATE# 
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#HOMOLOG2GI)     
     if(length(xx) > 0){
         xx[1:5]
     }
}
