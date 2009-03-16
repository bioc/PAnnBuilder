\name{pBase}
\alias{pBase}
\title{Define Class and Object}
\description{
    This function define class and create new object.
}
\usage{
pBase(srcUrl, parser, built="", fromWeb=TRUE, organism)
}
\arguments{
  \item{srcUrl}{a character string for the url where data file will be 
   retained.}
  \item{parser}{a character string for the path of the parser file.}
  \item{built}{a character string for the release/version information of
    source data. }
  \item{fromWeb}{a boolean to indicate whether the source
    data will be downloaded from the web or read from a local file}
  \item{organism}{a character string for the name of the organism of
    concern. (eg: "Homo sapiens")}  
}
\details{
  Bioconductor "AnnBuilder" package define a class "pubRepo". Here we 
  define a subclass of "pubRepo" called "pBase". "pBase" is used to process 
  data from SwissProt, TREMBL, IPI, NCBI RefSeq databases.
}
\value{
  \code{\link{pBase}} return a object of "pBase" class.
}
\author{Hong Li}


