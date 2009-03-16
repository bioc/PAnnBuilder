\name{#PREFIX#PFAM2DE}
\alias{#PREFIX#PFAM2DE}
\title{Map Pfam domain identifiers to decsription}
\description{
   #PREFIX#PFAM2DE provides mappings of Pfam domain identifiers to its decsription.
}
\details{ 
  Mappings were based on data provided by Pfam: 
  \url{#PFAMSOURCEURL#}) on #PFAMSOURCEDATE#  
}

\examples{
  x <- #PREFIX#PFAM2DE
  # Get the Pfam domain identifiers that are mapped to descriptions.
  mapped <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped])	
  if(length(xx) > 0){        
    xx[2:3]
  }
}

