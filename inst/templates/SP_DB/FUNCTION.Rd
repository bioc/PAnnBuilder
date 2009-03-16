\name{#PREFIX#FUNCTION}
\alias{#PREFIX#FUNCTION}
\title{Map SysPTM protein identifier to function(s)}
\description{
  #PREFIX#FUNCTION maps protein identifier to the general description of the 
  Function(s) of a protein.
}
\details{
  Each protein identifier maps to the general description of the Function(s) of  
  a protein. This information is extractd from "FUNCTION" topic in SwissProt 
  "comments" lines. 

  Mappings were based on data provided by: SwissProt (\url{#SPSOURCEURL#}) on
  #SPSOURCEDATE#  
}

\examples{
  x <- #PREFIX#FUNCTION
  # Get the protein identifiers that are mapped to function(s).
  mapped_proteins <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_proteins])
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
	}
}

