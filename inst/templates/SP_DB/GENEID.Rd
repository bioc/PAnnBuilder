\name{#PREFIX#GENEID}
\alias{#PREFIX#GENEID}
\title{Map protein identifier to Entrez gene identifier} 
\description{
  #PREFIX#GENEID maps protein identifiers to Entrez Gene identifiers.
}
\details{
  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#   
}

\examples{
  x <- #PREFIX#GENEID
  # Get the protein identifiers that are mapped to Entrez gene identifier.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
		# Get the values for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
	}
}

