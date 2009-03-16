\name{#PREFIX#HOMOLOG}
\alias{#PREFIX#HOMOLOG}
\alias{#PREFIX#HOMOLOG2GENEID}
\title{Tax identifier}
\description{
  #PREFIX#HOMOLOG map Entrez gene identifier to homolog group identifier.
}
\details{
  Mappings were based on data provided by:HomoloGene (\url{#HOMOLOGENESOURCEURL#}) on
  #HOMOLOGENESOURCEDATE# 
}

\examples{
     ## Convert to a list
     xx <- as.list(#PREFIX#HOMOLOG)     
     if(length(xx) > 0){
         xx[1:5]
     }
     
     ## For the reverse map #PREFIX#HOMOLOG2GENEID
     ## Convert to a list
     xx <- as.list(#PREFIX#HOMOLOG2GENEID)     
     if(length(xx) > 0){
         xx[1:5]
     }     
}
