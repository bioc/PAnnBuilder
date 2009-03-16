\name{#PREFIX#GO2NAME}
\alias{#PREFIX#GO2NAME}
\title{Map GO identifiers to name}
\description{
   #PREFIX#GO2NAME provides mappings of GO identifiers to its name.
}
\details{ 
  Mappings were based on data provided by GO: 
  \url{#GOSOURCEURL#}) on #GOSOURCEDATE#  
}

\examples{
  x <- #PREFIX#GO2NAME
  # Get the GO identifiers that are mapped to descriptions.
  mapped <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped])	
  if(length(xx) > 0){        
    xx[2:3]
  }
}

