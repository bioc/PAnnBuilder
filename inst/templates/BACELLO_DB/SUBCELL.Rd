\name{#PREFIX#SUBCELL}
\alias{#PREFIX#SUBCELL}
\alias{#PREFIX#SUBCELL2SPID}
\title{Map protein identifier to the subcellular location}
\description{
  #PREFIX#SUBCELL provides mappings between a Swss-Prot
  protein identifier and the protein subcellular locations. 
}
\details{
  Mappings were based on data provided by: BaCelLo (\url{#BACELLOSOURCEURL#}) on
  #BACELLOSOURCEDATE#  
}

\examples{
        ## Convert to a list
        xx <- as.list(#PREFIX#SUBCELL)
        ## Get the first one
        xx[[1]]
        
        ## For the reverse map #PREFIX#SUBCELL2SPID
        ## Convert to a list
        xx <- as.list(#PREFIX#SUBCELL2SPID)
        ## Get the first one
        xx[[1]]
}
