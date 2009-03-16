\name{#PREFIX#KEGG}
\alias{#PREFIX#KEGG}
\title{Map protein identifier to KEGG gene identifiers} 
\description{
  #PREFIX#KEGG maps protein identifiers to KEGG gene identifiers.
}
\details{
  Each protein identifier maps to KEGG gene identifiers.

  Mappings were based on data provided by: KEGG (\url{#KEGGSOURCEURL#}) on
  #KEGGSOURCEDATE#  
}

\examples{
	x <- #PREFIX#KEGG
  # Get the protein identifiers that are mapped to KEGG gene identifiers.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

