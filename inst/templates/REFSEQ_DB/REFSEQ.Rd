\name{#PREFIX#REFSEQ}
\alias{#PREFIX#REFSEQ}
\alias{#PREFIX#REFSEQ2GI}
\title{Map protein identifier to RefSeq identifiers} 
\description{
  #PREFIX#REFSEQ maps protein identifiers to RefSeq identifiers.
}
\details{
  Each protein identifier maps to RefSeq identifiers.

  Mappings were based on data provided by: NCBI (\url{#REFSEQSOURCEURL#}) on
  #REFSEQSOURCEDATE#  
}

\examples{
	x <- #PREFIX#REFSEQ
  # Get the protein identifiers that are mapped to RefSeq identifiers.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
	
	## For the reverse map #PREFIX#REFSEQ2GI
  xx <- as.list(#PREFIX#REFSEQ2GI)
  if(length(xx) > 0){         
      ## Get the first one
      xx[[1]]
  } 
}

