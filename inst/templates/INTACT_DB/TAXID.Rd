\name{#PREFIX#TAXID}
\alias{#PREFIX#TAXID}
\title{Taxonomy identifier}
\description{
  #PREFIX#TAXID map Uniprot protein accession number to its taxonomy identifer.
}
\details{
  Mappings were based on data provided by: IntAct (\url{#INTACTSOURCEURL#}) on
  #INTACTSOURCEDATE# 
}

\examples{
     # Convert to a list
     xx <- as.list(#PREFIX#TAXID)     
     if(length(xx) > 0){
         xx[1:5]
     }
}
