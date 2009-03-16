\name{#PREFIX#PEP2PROTEIN}
\alias{#PREFIX#PEP2PROTEIN}
\title{Map peptide identifier to the its portien location}
\description{
  #PREFIX#PEP2PROTEIN provides mappings between a PeptideAtlas
  peptide identifier and the its location on the protein, including protien 
  identifier, start position on the protein, end position on the protein.  
}
\details{
  Mappings were based on data provided by: PeptideAtlas (\url{#PEPTIDEATLASSOURCEURL#})
  on #PEPTIDEATLASSOURCEDATE#  
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PEP2PROTEIN)
		# Get the values for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
}

