\name{#PREFIX#PEP2CHR}
\alias{#PREFIX#PEP2CHR}
\title{Map peptide identifier to the its chromosome location}
\description{
  #PREFIX#PEP2CHR provides mappings between a PeptideAtlas
  peptide identifier and the its location on the chromosome, including chromosome, 
  strand, start position on the chromosome, end position on the chromosome.  
}
\details{
  Mappings were based on data provided by: PeptideAtlas (\url{#PEPTIDEATLASSOURCEURL#})
  on #PEPTIDEATLASSOURCEDATE#  
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#PEP2CHR)
		# Get the values for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
}

