\name{#PREFIX#SEQ}
\alias{#PREFIX#SEQ}
\title{Map peptide identifier to the its sequence}
\description{
  #PREFIX#SEQ provides mappings between a PeptideAtlas
  peptide identifier and the its sequence. 
}
\details{
  Mappings were based on data provided by: PeptideAtlas (\url{#PEPTIDEATLASSOURCEURL#})
  on #PEPTIDEATLASSOURCEDATE#  
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#SEQ)
		# Get the values for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
}

