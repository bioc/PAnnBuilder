\name{#PREFIX#DE}
\alias{#PREFIX#DE}
\title{Maps protein to textural descriptions}
\description{
  #PREFIX#DE maps Swiss-Prot protein accession number to their descriptive 
  information.
}
\details{
  Mappings were based on data provided by: GOA (\url{#GOASOURCEURL#}) on
  #GOASOURCEDATE#  
}

\examples{
	xx <- as.list(#PREFIX#DE)
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
		# Get the values for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
	}
}
