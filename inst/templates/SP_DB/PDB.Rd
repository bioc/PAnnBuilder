\name{#PREFIX#PDB}
\alias{#PREFIX#PDB}
\title{Map protein identifier to PDB structure} 
\description{
  #PREFIX#PDB maps protein identifiers to PDB structure.
}
\details{
  Each protein identifier is mapped to a vector of PDB 3D structure identifiers.

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
	x <- #PREFIX#PDB
  # Get the protein identifiers that are mapped to PDB structure.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

