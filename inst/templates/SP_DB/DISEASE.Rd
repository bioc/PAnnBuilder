\name{#PREFIX#DISEASE}
\alias{#PREFIX#DISEASE}
\title{Map SysPTM protein identifier to disease(s)}
\description{
  #PREFIX#DISEASE maps protein identifier to the description of the disease(s)
  associated with a deficiency of a protein. 
}
\details{
  Each protein identifier maps to the description of the disease(s) 
  associated with a deficiency of a protein. This information is extractd 
  from "DISEASE" topic in SwissProt "comments" lines. 

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
  x <- #PREFIX#DISEASE
  # Get the protein identifiers that are mapped to disease(s).
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

