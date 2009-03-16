\name{#PREFIX#DE}
\alias{#PREFIX#DE}
\title{Maps SysPTM protein identifier to textural descriptions}
\description{
  #PREFIX#DE maps SysPTM protein identifiers to their descriptive information.
}
\details{
  Mappings were based on data provided by: SysPTM (\url{#SYSPTMSOURCEURL#}) on
  #SYSPTMSOURCEDATE#  
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
