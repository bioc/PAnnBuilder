\name{#PREFIX#SUBCELL}
\alias{#PREFIX#SUBCELL}
\title{Map protein identifier to its subcellular location} 
\description{
  #PREFIX#SUBCELL maps protein identifiers to its subcellular location.
}
\details{
  Each protein identifier maps to the description of the Subcellular Location 
  of the chain/peptide/isoform. This information is extractd from 
  "SUBCELLULAR LOCATION" topic in SwissProt "comments" lines. 

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
	x <- #PREFIX#SUBCELL
  # Get the protein identifiers that are mapped to subcellular location.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])	
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
	
	
}

