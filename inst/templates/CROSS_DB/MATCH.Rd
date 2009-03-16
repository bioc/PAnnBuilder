\name{#PREFIX#MATCH}
\alias{#PREFIX#MATCH}
\title{Map protein identifier to the matched proteins}
\description{
  #PREFIX#MATCH provides mappings between a 
  protein identifier and the similar proteins based on BLAST program. 
}
\details{
  Mappings were based on data provided by: 
  \url{#CROSSSOURCEURL#}) on #CROSSSOURCEDATE#   
}

\examples{
  x <- #PREFIX#MATCH
  # Get the protein identifiers that has matched proteins.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])	
  # randomly display 10 proteins
  sample(xx, 10)
}

