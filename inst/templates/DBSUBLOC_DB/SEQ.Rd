\name{#PREFIX#SEQ}
\alias{#PREFIX#SEQ}
\title{Map protein to the protein sequence}
\description{
  #PREFIX#SEQ provides mappings between Swss-Prot
  protein accession number and the protein Sequence. 
}
\details{
  Mappings were based on data provided by: DBSubLoc (\url{#DBSUBLOCSOURCEURL#}) on
  #DBSUBLOCSOURCEDATE#  
}

\examples{
        # Convert to a list
        xx <- as.list(#PREFIX#SEQ)
        # Get the first one
        xx[[1]]
}
