\name{PeptideAtlasBuilder_DB}
\alias{PeptideAtlasBuilder_DB}
\title{Build data package for experimentally identified peptides}
\description{
    This function creates a data package for peptides identified in a large 
    set of tandem mass spectrometry proteomics experiments.
}
\usage{
PeptideAtlasBuilder_DB(name = c("Human","Human Plasma","Saccharomyces cerevisiae",
                    "Drosophila Melanogaster","Mouse Plasma","Halobacterium"),
                    prefix, pkgPath, version, author) 
}
\arguments{ 
  \item{name}{a character string to indicate which organisms or important 
   sample groups will be built. }
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
  Build annotation data packages for peptides identified in a large set of 
  tandem mass spectrometry proteomics experiments. Data are from PeptideAtlas
  database: \url{http://www.peptideatlas.org}, including high quality peptide 
  sequences, their locations relative to the protein start (CDS coordinates), 
  and peptide locations in chromosomal coordinates.

  \code{\link{PeptideAtlasBuilder_DB}} employes functions 
    \code{\link{writePeptideAtlasData_DB}} to parse and write data.

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
# Build annotation data packages "org.Hs.pep.db" for Human PeptideAtlas.
if(interactive()){
    PeptideAtlasBuilder_DB(name = "Human",
                        prefix = "org.Hs.pep", pkgPath, version, author) 
}
}


