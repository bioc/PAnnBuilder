\name{#PREFIX#TISSUE}
\alias{#PREFIX#TISSUE}
\title{Map protein identifier to tissue} 
\description{
  #PREFIX#TISSUE maps protein identifiers to tissue.
}
\details{
  Each protein identifier maps to the description of the tissue-specific 
  expression of mRNA or protein. This information is extractd from 
  "TISSUE SPECIFICITY" topic in UniProt "comments" lines. 

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
	x <- #PREFIX#TISSUE
  # Get the protein identifiers that are mapped to tissue.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

