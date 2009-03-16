\name{#PREFIX#INTERPRO2NAME}
\alias{#PREFIX#INTERPRO2NAME}
\title{Map Interpro domain identifiers to name}
\description{
   #PREFIX#INTERPRO2NAME provides mappings of Interpro domain identifiers to its name.
}
\details{ 
  Mappings were based on data provided by Interpro: 
  \url{#INTERPROSOURCEURL#}) on #INTERPROSOURCEDATE#  
}

\examples{
  x <- #PREFIX#INTERPRO2NAME
  # Get the Interpro domain identifiers that are mapped to names.
  mapped <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped])	
  if(length(xx) > 0){        
    xx[2:3]
  }
}

