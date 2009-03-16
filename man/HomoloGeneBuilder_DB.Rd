\name{HomoloGeneBuilder_DB}
\alias{HomoloGeneBuilder_DB}
\title{Build Data Packages for Homolog Protein Group}
\description{
    This function creates a data package of homologs among the annotated genes 
    of several completely sequenced eukaryotic genomes from NCBI HomoloGene.
}
\usage{
HomoloGeneBuilder_DB(prefix, pkgPath, version, author)
}
\arguments{ 
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
  \code{\link{HomoloGeneBuilder_DB}} employes functions 
  \code{\link{writeHomoloGeneData_DB}} to parse and write data.
  Only several completely sequenced eukaryotic genomes are involved: 
  Homo sapiens, Pan troglodytes, Canis lupus familiaris, Bos taurus, 
  Mus musculus, Rattus norvegicus, Gallus gallus, Danio rerio, 
  Drosophila melanogaster, Anopheles gambiae, Caenorhabditis elegans, 
  Schizosaccharomyces pombe, Saccharomyces cerevisiae, Kluyveromyces lactis,
  Eremothecium gossypii, Magnaporthe grisea, Neurospora crassa, 
  Arabidopsis thaliana, Oryza sativa, Plasmodium falciparum.
  
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
# Build annotation data packages "homolog.db" for Homo sapiens proteomics gene 
# ontology.
if(interactive()){
    HomoloGeneBuilder_DB(prefix = "homolog", pkgPath, version, author)
}
}

