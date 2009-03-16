\name{#PKGNAME#GO}
\alias{#PKGNAME#GO}
\title{Map Protein identifiers to Gene Ontology (GO)}
\description{
  #PKGNAME#GO is an R environment that provides mappings between
  protein identifiers and the GO identifiers that they are 
  directly associated with. 
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
 
  Mappings were based on data provided by: 
  #SOURCE#
  #GO#
  
  Package built #DATE#
}

\examples{
    # Convert to a list
    xx <- as.list(#PKGNAME#GO)
    if(length(xx) > 0){
        # Get the first ten ones
        xx[1:10]
    }
}
\keyword{datasets}

