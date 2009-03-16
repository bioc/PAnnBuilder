\name{GOABuilder_DB}
\alias{GOABuilder_DB}
\title{Build Data Packages for Proteomics Gene Ontology}
\description{
    Given the name of organism, this function creates a data
    package mapping proteins of UniProt to their Gene Ontolgy.
}
\usage{
GOABuilder_DB(organism = "Homo sapiens",
           prefix, pkgPath, version, author)
}
\arguments{ 
  \item{organism}{a character string for the name of the organism of
    concern. (eg: "Homo sapiens")}
  \item{prefix}{the prefix of the name of the data package to be built. (e.g. 
    "hsaSP"). The name of builded package is prefix+".db". }
  \item{pkgPath}{a character string for the full path of an existing
    directory where the built backage will be stored.}  
  \item{version}{a character string for the version number.}
  \item{author}{a list with named elements "authors" containing a character
    vector of author names and "maintainer" containing the complete
    character string for the maintainer field, for example, "Jane
    Doe <jdoe@doe.com>".}
}
\details{
  Build gene ontology annotation data packages for proteins in Uniprot.
  \code{\link{GOABuilder_DB}} employes functions \code{\link{writeGOAData_DB}} to 
    parse and write data.
    
  Data files in the database will be automatically downloaded to the tmp
  directory, so enough space is needed for the data files. After
  downloading, files are parsed by perl, so perl must be installed.  It may 
  take a long time to parse database and build R package. Alternatively, we have 
  produced diverse R packages by PAnnBuilder, and you can download appropriate 
  package via  \url{http://biosino.org/} .
}
\value{
  This function does not return any value.
}
\author{Hong Li}
\examples{
# Set path, version and author for the package.
pkgPath <- tempdir()
version <- "1.0.0"
author <- list()
author[["authors"]] <- "Hong Li"
author[["maintainer"]] <- "Hong Li <sysptm@gmail.com>"

## It may take a long time to parse database and build R package.
# Build annotation data packages "org.Hs.goa" for Homo sapiens proteomics gene 
# ontology. 
if(interactive()){
    GOABuilder_DB(organism="Homo sapiens",                       
               prefix="org.Hs.goa", pkgPath, version, author)
}
} 

