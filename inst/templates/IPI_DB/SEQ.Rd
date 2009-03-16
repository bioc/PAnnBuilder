\name{#PREFIX#SEQ}
\alias{#PREFIX#SEQ}
\title{Map protein identifier to the protein sequence}
\description{
  #PREFIX#SEQ provides mappings between a 
  protein identifier and the protein Sequence. 
}
\details{
  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE# 
}

\examples{
  x <- #PREFIX#SEQ
  # Get the protein identifiers that are mapped to protein sequence.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])	
  # randomly display 10 proteins
  sample(xx, 10)
}

