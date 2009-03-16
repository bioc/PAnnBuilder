\name{#PREFIX#UNIGENE}
\alias{#PREFIX#UNIGENE}
\title{Map protein identifier to UniGene cluster identifiers} 
\description{
  #PREFIX#UNIGENE maps protein identifiers to UniGene cluster identifiers.
}
\details{
  Each protein identifier maps to UniGene cluster identifiers.
  A UniGene identifier represents a cluster of sequences of a gene. Using
  UniGene identifiers one can query the UniGene database for information about
  the sequences or the Entrez Gene database for information about the
  genes.  

  Mappings were based on data provided by: IPI (\url{#IPISOURCEURL#}) on
  #IPISOURCEDATE#  
}

\examples{
	x <- #PREFIX#UNIGENE
  # Get the protein identifiers that are mapped to UniGene cluster identifiers.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

