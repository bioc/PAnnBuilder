\name{#PREFIX#DE}
\alias{#PREFIX#DE}
\title{Maps protein identifier to textural descriptions}
\description{
  #PREFIX#DE maps protein identifiers to their descriptive information.
}
\details{
  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{
	x <- #PREFIX#DE
  # Get the protein identifiers that are mapped to textural descriptions.
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
