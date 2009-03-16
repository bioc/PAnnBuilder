\name{#PREFIX#SEQ}
\alias{#PREFIX#SEQ}
\title{Map protein identifier to the protein sequence}
\description{
  #PREFIX#SEQ provides mappings between a SysPTM
  protein identifier and the protein Sequence. 
}
\details{
  Mappings were based on data provided by: SysPTM (\url{#SYSPTMSOURCEURL#}) on
  #SYSPTMSOURCEDATE#  
}

\examples{
        # Convert to a list
        xx <- as.list(#PREFIX#SEQ)
	 # Get the values for a few keys
	 if(length(xx) >= 3){
	   xx[1:3]
	 }
}

