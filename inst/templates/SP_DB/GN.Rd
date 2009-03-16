\name{#PREFIX#GN}
\alias{#PREFIX#GN}
\title{Map SysPTM protein identifier to gene names}
\description{
  #PREFIX#GN maps protein identifier to the corresponding gene names.
}
\details{
  Each protein identifier maps to a vector containing the gene
  name or alias.

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
  x <- #PREFIX#GN
  # Get the protein identifiers that are mapped to gene names.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

