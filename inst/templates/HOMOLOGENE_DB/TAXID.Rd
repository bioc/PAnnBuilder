\name{#PREFIX#TAXID}
\alias{#PREFIX#TAXID}
\title{Taxonomy identifier}
\description{
  #PREFIX#TAXID map Entrez gene identifier to its taxonomy identifer.
}
\details{
  Mappings were based on data provided by:HomoloGene (\url{#HOMOLOGENESOURCEURL#}) on
  #HOMOLOGENESOURCEDATE# 
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#TAXID)     
     if(length(xx) > 0){
         xx[1:5]
     }
}
