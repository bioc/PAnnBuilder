\name{#PREFIX#IPIACs}
\alias{#PREFIX#IPIACs}
\title{Map protein identifier to IPI accession numbers} 
\description{
  #PREFIX#IPIACs maps protein identifiers to IPI accession numbers.
}
\details{
  Each protein identifier maps to a vector of IPI accession numbers 
  (eg: IPI00000045, IPI00004985).

  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{
	x <- #PREFIX#IPIACs
  # Get the protein identifiers that are mapped to IPI accession numbers.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

