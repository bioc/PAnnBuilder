\name{#PREFIX#SPAC}
\alias{#PREFIX#SPAC}
\alias{#PREFIX#SPAC2IPIID}
\title{Map protein identifier to SwissProt primary accession number} 
\description{
  #PREFIX#SPAC maps protein identifiers to SwissProt primary accession number.
}
\details{
  Each protein identifier maps to SwissProt primary accession number.

  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{	
	x <- #PREFIX#SPAC
  # Get the protein identifiers that are mapped to SwissProt primary accession number.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])	
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
	
	# For the reverse map:
  xx <- as.list(#PREFIX#SPAC2IPIID)
  if(length(xx) > 0){
      goids <- xx[2:3]
  }
}

