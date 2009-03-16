\name{#PREFIX#SUBCELL}
\alias{#PREFIX#SUBCELL}
\alias{#PREFIX#SUBCELL2SPAC}
\title{Map protein to the subcellular location}
\description{
  #PREFIX#SUBCELL provides mappings between Swss-Prot
  protein accession number and the protein subcellular locations. 
}
\details{
  Mappings were based on data provided by: DBSubLoc (\url{#DBSUBLOCSOURCEURL#}) on
  #DBSUBLOCSOURCEDATE#  
}

\examples{
        ## Convert to a list
        xx <- as.list(#PREFIX#SUBCELL)
        ## Get the first one
        xx[[1]]
        
        ## For the reverse map #PREFIX#SUBCELL2SPAC
        ## Convert to a list
        xx <- as.list(#PREFIX#SUBCELL2SPAC)
        ## Get the first one
        xx[[1]]
}
