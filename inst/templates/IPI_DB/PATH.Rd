\name{#PREFIX#PATH}
\alias{#PREFIX#PATH}
\alias{#PREFIX#PATH2IPIID}
\title{Map protein identifier to KEGG pathway} 
\description{
  #PREFIX#PATH maps protein identifiers to KEGG pathway identifiers.
}
\details{
  Each protein identifier maps to KEGG pathway identifiers.

  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{	
	x <- #PREFIX#PATH
  # Get the protein identifiers that are mapped to KEGG pathway.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
	
	# For the reverse map:
  xx <- as.list(#PREFIX#PATH2IPIID)
  if(length(xx) > 0){
      goids <- xx[2:3]
  }
}

