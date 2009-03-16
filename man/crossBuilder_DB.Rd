\name{crossBuilder_DB}
\alias{crossBuilder_DB}
\alias{fasta2list}
\alias{idBlast}
\title{Build Data Packages for Protein ID Mapping}
\description{
    This function creates a data package with the protein id mapping stored 
    as R environment objects in the data directory.
}
\usage{
crossBuilder_DB(src = c("sp","ipi","gi"), organism, 
             blast, match, 
             prefix, pkgPath, version, author       
             ) 
fasta2list(type, srcUrl,organism="")
idBlast(query, subject, blast, match)
}
\arguments{ 
  \item{src}{a character vector that can be "sp", "trembl", "ipi" or "gi" 
    to indicate which protein sequence databases will be used.}    
  \item{organism}{a character string for the name of the organism of
    concern. (eg: "Homo sapiens")} 
  \item{blast}{a named character vector defining the parameters of blastall.}  
  \item{match}{a named character vector defining the parameters of two sequence 
    matching.}  
  \item{prefix}{the prefix of the name of the data package to be built. (e.g. 
    "hsaSP"). The name of builded package is prefix+".db". }  
  \item{pkgPath}{a character string for the full path of an existing
    directory where the built backage will be stored.}  
  \item{version}{a character string for the version number.}
  \item{author}{a list with named elements "authors" containing a character
    vector of author names and "maintainer" containing the complete
    character string for the maintainer field, for example, "Jane
    Doe <jdoe@doe.com>".}
  \item{type}{a character string for the type of sequence data file, can be 
    "sp", "trembl", "ipi" or "gi"}
  \item{srcUrl}{a character string for the url where sequence data file with 
    fasta format will be retained.}
  \item{query}{a named vector of query sequences}
  \item{subject}{a named vector of subject sequences}
}
\details{
  Build annotation data packages for protein id mapping. formatdb and blastall 
  are need to be installed.
  
  Parameter "blast" is a named character vector defining the parameters 
  of blastall. Possible names and their meaning are listed as follows:
  p:  Program Name [String].
  e:  Expectation value (E) [Real].
  M:  Matrix [String].
  W:  World Size, default if zero (blastn 11, megablast 28, all others 3) 
        [Integer] default = 0.
  G:  Cost to open a gap (-1 invokes default behavior) [Integer].
  E:  Cost to open a gap (-1 invokes default behavior) [Integer].
  U:  Use lower case filtering of FASTA sequence [T/F]  Optional.
  F:  Filter query sequence (DUST with blastn, SEG with others) [String].
      
  Parameter "match" a named character vector defining the parameters of 
  two sequence matching. Possible names and their meaning are listed as follows:
  e:  Expectation value of two sequence matching [Real].
  c:  Coverage of the longest High-scoring Segment Pair (HSP) to the whole 
        protein sequence. (range: 0~1)
  i:  Identity of the longest High-scoring Segment Pair (HSP). (range: 0~1)
    
  Data files in the database will be automatically downloaded to the tmp
  directory, so enough space is needed for the data files. After
  downloading, files are parsed by perl, so perl must be installed.  It may 
  take a long time to parse database and build R package. Alternatively, we have 
  produced diverse R packages by PAnnBuilder, and you can download appropriate 
  package via \url{http://www.biosino.org/PAnnBuilder}.
}
\author{Hong Li}
\examples{
# Set path, version and author for the package.
pkgPath <- tempdir()
version <- "1.0.0"
author <- list()
author[["authors"]] <- "Hong Li"
author[["maintainer"]] <- "Hong Li <sysptm@gmail.com>"

# Set parameters for sequence similarity.
blast <- c("blastp", "10.0", "BLOSUM62", "0", "-1", "-1", "T", "F")
names(blast) <- c("p","e","M","W","G","E","U","F")
match <- c(0.00001, 0.95, 0.95)
names(match) <- c("e","c","i")

## It may take a long time to parse database and build R package.
# Build annotation data packages "org.Hs.cross" for id mapping of three major 
# protein sequence databases.
if(interactive()){
    crossBuilder_DB(src=c("sp","ipi","gi"), organism="Homo sapiens", 
                    blast, match, 
                    prefix="org.Hs.cross", pkgPath, version, author)
}
}


