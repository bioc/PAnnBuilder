\name{#PREFIX#ORGANISM}
\alias{#PREFIX#ORGANISM}
\title{Map tax identifiers to organism}
\description{
  #PREFIX#ORGANISM map tax identifier to the name the organism.
}
\details{
  Mappings were based on data provided by:HomoloGene (#HOMOLOGENESOURCEURL#) on
  #HOMOLOGENESOURCEDATE#  
}
\examples{
        # Convert to a list
        xxx <- as.list(#PREFIX#ORGANISM)
        # randomly display 2 proteins
        sample(xxx, 2)
}

