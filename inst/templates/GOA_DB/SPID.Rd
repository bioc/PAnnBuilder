\name{#PREFIX#SPID}
\alias{#PREFIX#SPID}
\alias{#PREFIX#SPID2SPAC}
\title{Maps protein to Swiss-Prot identifiers}
\description{
  #PREFIX#SPID maps Swiss-Prot protein accession number to Swiss-Prot protein
  identifiers.
}
\details{
  Mappings were based on data provided by: GOA (\url{#GOASOURCEURL#}) on
  #GOASOURCEDATE#  
}

\examples{
	xx <- as.list(#PREFIX#SPID)
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
		# Get the values for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
	}
	
	## For the reverse map #PREFIX#SPID2SPAC
	xx <- as.list(#PREFIX#SPID2SPAC)
	xx[[1]]
}
