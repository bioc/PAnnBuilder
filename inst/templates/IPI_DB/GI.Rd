\name{#PREFIX#GI}
\alias{#PREFIX#GI}
\title{Map protein identifier to the NCBI Protein GI}
\description{
  #PREFIX#GI maps protein identifiers to the NCBI Protein GI identifiers.
}
\details{
  Each protein identifier maps to the NCBI Protein GI identifiers.

  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{	
  x <- #PREFIX#GI
  # Get the protein identifiers that are mapped to the NCBI Protein GI.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

