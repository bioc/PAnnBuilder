\name{InParanoidBuilder_DB}
\alias{InParanoidBuilder_DB}
\title{Build Data Packages for Ortholog Protein Group}
\description{
    This function creates a data package containing ortholog protein group 
    between two given organisms from InParanoid.
}
\usage{
InParanoidBuilder_DB(organism = c("Arabidopsis thaliana","Apis mellifera"),
                  prefix, pkgPath, version, author)
}
\arguments{ 
  \item{organism}{a string vector with two elements for the name of two 
    organisms. }
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
  Build annotation data packages for ortholog protein gropus from InParanoid.
  InParanoid is a database of Eukaryotic Ortholog Groups: \url{
    http://inparanoid.sbc.su.se/cgi-bin/index.cgi}.
  \code{\link{InParanoidBuilder_DB}} employes functions 
    \code{\link{writeInParanoidData_DB}} to parse and write data.
   
  Data files in the database will be automatically downloaded to the tmp
  directory, so enough space is needed for the data files. After
  downloading, files are parsed by perl, so perl must be installed.  It may 
  take a long time to parse database and build R package. Alternatively, we have 
  produced diverse R packages by PAnnBuilder, and you can download appropriate 
  package via \url{http://www.biosino.org/PAnnBuilder}.
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
# Build annotation data packages "org.HsMm.ortholog.db" for orthologs between 
# Homo sapiens and Mus musculus.
if(interactive()){
    InParanoidBuilder_DB(organism = c("Homo sapiens","Mus musculus"), 
                      prefix = "org.HsMm.ortholog", pkgPath, version, author)
}
}


