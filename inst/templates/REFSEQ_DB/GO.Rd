\name{#PREFIX#GO}
\alias{#PREFIX#GO}
\alias{#PREFIX#GO2GI}
\title{Map protein identifier to GO} 
\description{
  #PREFIX#GO maps protein identifiers to Gene Ontology identifiers .
}
\details{
  Each Protein identifier is mapped to a list. Each component contain 
  : GO ID, Evidence and Ontology (C,F,P).  NAs are assigned to probe 
  identifiers that can not be mapped to any Gene Ontology information. 

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

  Mappings were based on data provided by: NCBI (\url{#GOSOURCEURL#}) on
  #GOSOURCEDATE#  
}

\examples{
  x <- #PREFIX#GO
  # Get the protein identifiers that are mapped to a GO ID
  mapped_genes <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_genes])
  if(length(xx) > 0) {
      # Try the first one
      got <- xx[[1]]           
      got[[1]][["GOID"]]
      got[[1]][["Ontology"]]
      got[[1]][["Evidence"]]
  }
  # For the reverse map:
  xx <- as.list(#PREFIX#GO2GI)
  if(length(xx) > 0){
      goids <- xx[2:3]
  }
	
	
	
}

