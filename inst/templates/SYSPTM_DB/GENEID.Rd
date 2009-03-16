\name{#PREFIX#GENEID}
\alias{#PREFIX#GENEID}
\title{Map SysPTM protein identifier to Entrez gene identifier} 
\description{
  #PREFIX#GENEID maps SysPTM protein identifiers to Entrez Gene identifiers.
}
\details{
  Mappings were based on data provided by: SysPTM (\url{#SYSPTMSOURCEURL#}) on
  #SYSPTMSOURCEDATE#  
}

\examples{
	xx <- as.list(#PREFIX#GENEID)
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
		# Get the values for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
	}
}

