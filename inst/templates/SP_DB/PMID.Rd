\name{#PREFIX#PMID}
\alias{#PREFIX#PMID}
\title{Map protein identifier to PubMed Identifiers} 
\description{
  #PREFIX#PMID maps protein identifiers to PubMed Identifiers.
}
\details{
  Each protein identifier is mapped to a vector of PubMed identifiers.
  The length of the vector may be one or greater, depending on how
  many PubMed identifiers a given manufacturer identifier is mapped to.

  Titles, abstracts, and possibly full texts of articles can be obtained
  from PubMed by providing a valid PubMed identifier. 

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
	x <- #PREFIX#PMID
  # Get the protein identifiers that are mapped to PubMed Identifiers.
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

