\name{#PREFIX#INTERPRO}
\alias{#PREFIX#INTERPRO}
\title{Map protein identifier to Interpro doamins} 
\description{
  #PREFIX#INTERPRO maps protein identifiers to Interpro identifiers .
}
\details{
  Each protein identifier maps to a vector of InterPro identifiers.  

  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{
	x <- #PREFIX#INTERPRO
  # Get the protein identifiers that are mapped to Interpro doamins.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

