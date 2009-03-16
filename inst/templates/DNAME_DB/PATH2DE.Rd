\name{#PREFIX#PATH2DE}
\alias{#PREFIX#PATH2DE}
\title{Map KEGG pathway identifiers to decsription}
\description{
   #PREFIX#PATH2DE provides mappings of KEGG pathway identifiers to its decsription.
}
\details{ 
  Mappings were based on data provided by KEGG: 
  \url{#KEGGSOURCEURL#}) on #KEGGSOURCEDATE#  
}

\examples{
  x <- #PREFIX#PATH2DE
  # Get the KEGG pathway identifiers that are mapped to descriptions.
  mapped <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped])	
  if(length(xx) > 0){        
    xx[2:3]
  }
}

