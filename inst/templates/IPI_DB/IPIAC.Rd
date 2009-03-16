\name{#PREFIX#IPIAC}
\alias{#PREFIX#IPIAC}
\alias{#PREFIX#IPIAC2IPIID}
\title{Map protein identifier to IPI Primary accession number} 
\description{
  #PREFIX#IPIAC maps protein identifiers to IPI Primary Accession Number.
}
\details{
  Each protein identifier maps to a IPI primary accession number (eg: IPI00000045).

  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{
	x <- #PREFIX#IPIAC
  # Get the protein identifiers that are mapped to IPI Primary accession number.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])	
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
	
  # For the reverse map:
  xx <- as.list(#PREFIX#IPIAC2IPIID)
  if(length(xx) > 0){
      goids <- xx[2:3]
  }
}

