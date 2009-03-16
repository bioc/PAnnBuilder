\name{#PREFIX#SYMBOL}
\alias{#PREFIX#SYMBOL}
\title{Gene symbol}
\description{
  #PREFIX#SYMBOL map Entrez gene identifier to its symbol.
}
\details{
  Mappings were based on data provided by:HomoloGene (\url{#HOMOLOGENESOURCEURL#}) on
  #HOMOLOGENESOURCEDATE# 
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#SYMBOL)     
     if(length(xx) > 0){
         xx[1:5]
     }
}
