\name{#PREFIX#SPID}
\alias{#PREFIX#SPID}
\alias{#PREFIX#SPID2IPIID}
\title{Map protein identifier to SwissProt identifiers} 
\description{
  #PREFIX#SPID maps protein identifiers to SwissProt identifiers.
}
\details{
  Each protein identifier maps to SwissProt identifiers.

  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{
	x <- #PREFIX#SPID
  # Get the protein identifiers that are mapped to SwissProt identifiers.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])	
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
	
	# For the reverse map:
  xx <- as.list(#PREFIX#SPID2IPIID)
  if(length(xx) > 0){
      goids <- xx[2:3]
  }
}

