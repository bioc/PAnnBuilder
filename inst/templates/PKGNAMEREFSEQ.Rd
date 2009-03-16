\name{#PKGNAME#REFSEQ}
\alias{#PKGNAME#REFSEQ}
\title{Map Protein Identifiers to RefSeq Identifiers}
\description{
   #PKGNAME#REFSEQ is an R environment that provides mappings between
   protein identifiers and RefSeq identifiers.
}
\details{
  Each protein identifier is mapped to RefSeq identifier.    
  NCBI \url{http://www.ncbi.nlm.nih.gov/RefSeq/} allows users to query 
  the RefSeq database using RefSeq identifiers.
  An \code{NA} is reported for any
  protein identifier that cannot be mapped to a RefSeq identifier at this time.

  RefSeq identifiers differ in format according to the type of record the identifiers
  are for as shown below:
  NG\_XXXXX: RefSeq accessions for genomic region (nucleotide) records
  NM\_XXXXX: RefSeq accessions for mRNA records
  NC\_XXXXX: RefSeq accessions for chromosome records
  NP\_XXXXX: RefSeq accessions for protein records
  XR\_XXXXX: RefSeq accessions for model RNAs that are not associated with
  protein products
  XM\_XXXXX: RefSeq accessions for model mRNA records
  XP\_XXXXX: RefSeq accessions for model protein records
  Where XXXXX is a sequence of integers.

  Mappings were based on data provided by:
  #SOURCE#

  Package built #DATE#
}
\references{
    \url{http://www.ncbi.nlm.nih.gov}
    \url{http://www.ncbi.nlm.nih.gov/RefSeq/} 
}
\examples{
    # Convert the environment to a list
    xx <- as.list(#PKGNAME#REFSEQ)
    # Remove protein identifiers that do not map to any RefSeq
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
    # The RefSeq for the first two elements of XX
    xx[1:2]
    # Get the first one
    xx[[1]]
}
}

\keyword{datasets}

