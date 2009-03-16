\name{#PREFIX#GO}
\alias{#PREFIX#GO}
\alias{#PREFIX#GO2SPAC}
\title{Maps protein to gene ontology}
\description{
  #PREFIX#GO maps Swiss-Prot protein accession number to their gene ontology
  annotation.
}
\details{

  The Evidence element contains a code indicating what kind of
  evidence supports the association of the GO id to the protein
  id. The evidence codes in use include:
  IMP: inferred from mutant phenotype  
  IGI: inferred from genetic interaction
  IPI: inferred from physical interaction  
  ISS: inferred from sequence similarity 
  IDA: inferred from direct assay  
  IEP: inferred from expression pattern  
  IEA: inferred from electronic annotation  
  TAS: traceable author statement  
  NAS: non-traceable author statement  
  ND: no biological data available  
  IC: inferred by curator
  
  Mappings were based on data provided by: GOA (\url{#GOASOURCEURL#}) on
  #GOASOURCEDATE#  
}

\examples{
	xx <- as.list(#PREFIX#GO)
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
		# Get the values for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
	}
		
  # For the reverse map #PREFIX#GO2SPAC:
  xx <- as.list(#PREFIX#GO2SPAC)
  if(length(xx) > 0){
      xx[2:3]
  }
}
