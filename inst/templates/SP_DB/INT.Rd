\name{#PREFIX#INT}
\alias{#PREFIX#INT}
\title{Map protein identifier to interacted proteins} 
\description{
  #PREFIX#INT maps protein identifiers to interacted proteins.
}
\details{
  Each protein identifier is mapped to a vector of protein identifiers which
  may have interaction between them.

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
  x <- #PREFIX#INT
  # Get the protein identifiers that are mapped to interacted proteins.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])	
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

