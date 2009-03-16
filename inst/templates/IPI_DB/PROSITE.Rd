\name{#PREFIX#PROSITE}
\alias{#PREFIX#PROSITE}
\title{Map protein identifier to PROSITE domain} 
\description{
  #PREFIX#PROSITE maps protein identifiers to PROSITE domain identifiers.
}
\details{
  Each protein identifier maps to a vector of PROSITE domain identifiers.

  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{
	x <- #PREFIX#PROSITE
  # Get the protein identifiers that are mapped to PROSITE domain.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

