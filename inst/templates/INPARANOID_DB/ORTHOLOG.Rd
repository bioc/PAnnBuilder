\name{#PREFIX#ORTHOLOG}
\alias{#PREFIX#ORTHOLOG}
\alias{#PREFIX#ORTHOLOG2PROTEIN}
\title{Map protein identifier to its ortholog}
\description{
  #PREFIX#ORTHOLOG map protein identifier to its ortholog identifier.
}
\details{
  Mappings were based on data provided by: Inparanoid (\url{#INPARANOIDSOURCEURL#})
  on #INPARANOIDSOURCEDATE# 
}
\examples{
        ## Convert to a list
        xxx <- as.list(#PREFIX#ORTHOLOG)
        ## randomly display 10 proteins
        sample(xxx, 10)
        
        ## For the reverse map #PREFIX#ORTHOLOG2PROTEIN
        ## Convert to a list
        xxx <- as.list(#PREFIX#ORTHOLOG2PROTEIN)
        ## randomly display 10 proteins
        sample(xxx, 10)
}

