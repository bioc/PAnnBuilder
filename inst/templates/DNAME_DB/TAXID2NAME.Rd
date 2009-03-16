\name{#PREFIX#TAXID2NAME}
\alias{#PREFIX#TAXID2NAME}
\title{Map taxonomy identifiers to name}
\description{
   #PREFIX#TAXID2NAME provides mappings of taxonomy
   id to its name.
}
\details{ 
  Mappings were based on data provided by NCBI: 
  \url{#TAXSOURCEURL#}) on #TAXSOURCEDATE#  
}

\examples{
  x <- #PREFIX#TAXID2NAME
  # Get the taxonomy identifiers that are mapped to name.
  mapped <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped])	
  if(length(xx) > 0){        
    xx[2:3]
  }
}

