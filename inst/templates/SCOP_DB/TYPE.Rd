\name{#PREFIX#TYPE}
\alias{#PREFIX#TYPE}
\title{Map SCOP identifiers to their types}
\description{
   #PREFIX#TYPE provides mappings between
   SCOP entry identifiers and their types.
}
\details{
	Possible entry types are:
	cl - class 
	
	cf - fold
	
	sf - superfamily
	
	fa - family
	
	dm - protein domain
	
	sp - species
	
	px - domain entry
   
  Mappings were based on data provided by: SCOP (\url{#SCOPSOURCEURL#}) on
  #SCOPSOURCEDATE# 
}

\examples{
    # Convert to a list
    xx <- as.list(#PREFIX#TYPE)
    if(length(xx) > 0){        
        xx[2:3]
    }
}
