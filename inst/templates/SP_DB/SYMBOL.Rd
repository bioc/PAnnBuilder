\name{#PREFIX#SYMBOL}
\alias{#PREFIX#SYMBOL}
\title{Map protein identifier to gene symbols} 
\description{
  #PREFIX#SYMBOL maps protein identifiers to gene symbols.
}
\details{
  Each protein identifier maps to an abbreviation for the
  corresponding gene. .

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#   
}

\examples{
	x <- #PREFIX#SYMBOL
  # Get the protein identifiers that are mapped to gene symbols.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

