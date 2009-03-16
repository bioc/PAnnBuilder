\name{#PREFIX#PATH}
\alias{#PREFIX#PATH}
\alias{#PREFIX#PATH2GI}
\title{Map protein identifier to KEGG pathway} 
\description{
  #PREFIX#PATH maps protein identifiers to KEGG pathway identifiers.
}
\details{
  Each protein identifier maps to KEGG pathway identifiers.

  Mappings were based on data provided by: KEGG (\url{#KEGGSOURCEURL#}) on
  #KEGGSOURCEDATE#  
}

\examples{
	x <- #PREFIX#PATH
  # Get the protein identifiers that are mapped to KEGG pathways.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
	
	## For the reverse map #PREFIX#PATH2GI
  xx <- as.list(#PREFIX#PATH2GI)
  if(length(xx) > 0){         
      ## Get the first one
      xx[[1]]
  } 
}

