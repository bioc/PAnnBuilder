\name{#PREFIX#HOMOLOG2AC}
\alias{#PREFIX#HOMOLOG2AC}
\title{Tax identifier}
\description{
  #PREFIX#HOMOLOG2AC map homolog group identifier to NCBI protein accession number.
}
\details{
  Mappings were based on data provided by:HomoloGene (\url{#HOMOLOGENESOURCEURL#}) on
  #HOMOLOGENESOURCEDATE# 
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#HOMOLOG2AC)     
     if(length(xx) > 0){
         xx[1:5]
     }
}
