\name{#PREFIX#SEQ}
\alias{#PREFIX#SEQ}
\title{Map protein identifier to the protein sequence}
\description{
  #PREFIX#SEQ provides mappings between a
  protein identifier and the protein Sequence. 
}
\details{
  Mappings were based on data provided by: Inparanoid (\url{#INPARANOIDSOURCEURL#})
  on #INPARANOIDSOURCEDATE# 
}

\examples{
        # Convert to a list
        xxx <- as.list(#PREFIX#SEQ)
        # randomly display 10 proteins
        sample(xxx, 10)
}

