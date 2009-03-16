\name{#PREFIX#MW}
\alias{#PREFIX#MW}
\title{Map protein identifier to its molecular weight} 
\description{
  #PREFIX#MW maps protein identifiers to its molecular weight.
}
\details{
  Each protein identifier maps to its molecular weight.

  Mappings were based on data provided by: IPI (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
	x <- #PREFIX#MW
  # Get the protein identifiers that are mapped to its molecular weight.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

