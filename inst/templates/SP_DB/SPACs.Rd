\name{#PREFIX#SPACs}
\alias{#PREFIX#SPACs}
\title{Map protein identifier to SwissProt accession numbers} 
\description{
  #PREFIX#SPACs maps protein identifiers to SwissProt accession numbers.
}
\details{
  Each protein identifier maps to a vector of SwissProt accession numbers.

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
	x <- #PREFIX#SPACs
  # Get the protein identifiers that are mapped to SwissProt accession numbers.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])	
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

