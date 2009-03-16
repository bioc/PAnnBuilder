\name{#PKGNAME#PARAMETER}
\alias{#PKGNAME#PARAMETER}
\title{Give the parameters of sequence matching}
\description{
  #PKGNAME#PARAMETER is an R environment that provides the parameters of 
  sequence matching
}
\details{
  The parameters for blast include:
  p:  Program Name [String]
  e:  Expectation value (E) [Real]
  M:  Matrix [String]
  W:  World Size, default if zero (blastn 11, megablast 28, all    
       others 3) [Integer] default = 0
  G:  Cost to open a gap (-1 invokes default behavior) [Integer]
  E:  Cost to open a gap (-1 invokes default behavior) [Integer]
  U:  Use lower case filtering of FASTA sequence [T/F]  Optional
  F:  Filter query sequence (DUST with blastn, SEG with others) 
       [String]
 
 The parameters for sequence matching include:
  e:  Expectation value of two sequence matching [Real]
  c:  Coverage of the longest High-scoring Segment Pair (HSP) to the
       whole protein sequence. (range: 0~1)
  i:  Indentity of the longest High-scoring Segment Pair (HSP). 
       (range: 0~1)
  
  Mappings were derived from data provided by:
  #SOURCE#
  
  Package built #DATE#
}

\examples{
        # Convert to a list
        xxx <- as.list(#PKGNAME#PARAMETER)
        # display parameters for blast
        xxx$blast
        # display parameters for sequence matching
        xxx$match
}
\keyword{datasets}

