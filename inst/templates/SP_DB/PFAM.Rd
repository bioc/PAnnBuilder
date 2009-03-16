\name{#PREFIX#PFAM}
\alias{#PREFIX#PFAM}
\title{Map protein identifier to Pfam domain} 
\description{
  #PREFIX#PFAM maps protein identifiers to Pfam domain identifiers.
}
\details{
  Each protein identifier maps to a vector of Pfam domain identifiers.

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
	x <- #PREFIX#PFAM
  # Get the protein identifiers that are mapped to Pfam domain.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

