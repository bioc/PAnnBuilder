\name{#PREFIX#PFAM2NAME}
\alias{#PREFIX#PFAM2NAME}
\title{Map Pfam domain identifiers to name}
\description{
   #PREFIX#PFAM2NAME provides mappings of Pfam domain identifiers to its name.
}
\details{ 
  Mappings were based on data provided by Pfam: 
  \url{#PFAMSOURCEURL#}) on #PFAMSOURCEDATE#  
}

\examples{
  x <- #PREFIX#PFAM2NAME
  # Get the Pfam domain identifiers that are mapped to names.
  mapped <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped])	
  if(length(xx) > 0){        
    xx[2:3]
  }
}

