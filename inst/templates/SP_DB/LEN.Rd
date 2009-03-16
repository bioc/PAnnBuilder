\name{#PREFIX#LEN}
\alias{#PREFIX#LEN}
\title{Map protein identifier to the length of protein sequence}
\description{
  #PREFIX#LEN maps protein identifiers to the length of protein sequence.
}
\details{
  Each protein identifier maps to the length of protein sequence.

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
  x <- #PREFIX#LEN
  # Get the protein identifiers that are mapped to the length of protein sequence.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

