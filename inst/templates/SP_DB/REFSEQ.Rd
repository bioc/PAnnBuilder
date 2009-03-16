\name{#PREFIX#REFSEQ}
\alias{#PREFIX#REFSEQ}
\alias{#PREFIX#REFSEQ2SPID}
\title{Map protein identifier to RefSeq identifiers} 
\description{
  #PREFIX#REFSEQ maps protein identifiers to RefSeq identifiers.
}
\details{
  Each protein identifier maps to RefSeq identifiers.

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#   
}

\examples{
	x <- #PREFIX#REFSEQ
  # Get the protein identifiers that are mapped to the RefSeq identifiers.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])	
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}

	## For the reverse map #PREFIX#REFSEQ2SPID
  xx <- as.list(#PREFIX#REFSEQ2SPID)
  if(length(xx) > 0){         
      ## Get the first one
      xx[[1]]
  } 
}

