\name{#PREFIX#ORGANISM}
\alias{#PREFIX#ORGANISM}
\title{Map SysPTM protein to organism}
\description{
  #PREFIX#ORGANISM map Swss-Prot
  protein accession number to the name the organism.
}
\details{
  Mappings were based on data provided by: DBSubLoc (#DBSUBLOCSOURCEURL#) on
  #DBSUBLOCSOURCEDATE#  
}
\examples{
         # Convert to a list
        xxx <- as.list(#PREFIX#ORGANISM)
        # randomly display 10 proteins
        sample(xxx, 10)
}

