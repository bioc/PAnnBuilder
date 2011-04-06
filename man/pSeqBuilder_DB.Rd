\name{pSeqBuilder_DB}
\alias{pSeqBuilder_DB}
\title{Build Data Packages for Query Sequences}
\description{
    This function use previous data annotation packages and employ blast program
    to creates a new data package for query sequences.
}
\usage{
pSeqBuilder_DB(query, annPkgs, seqName, blast, match,
            prefix, pkgPath, version, author) 
}
\arguments{
  \item{query}{a named string vector to be used as query sequences. Blast 
    will be called to map between query sequences and sequences from the 
    given protein sequence package, and then get corresponding annotation 
    data from the given annotation package.}
  \item{annPkgs}{a string vector containing the name of annotation packages.
    In annotation package, data is saved as R 
    environment or SQLite object. The Key is protein, 
    and the value is its annotation.}
  \item{seqName}{a string vector which has the same length with parameter "annPkgs",
    and indicating the name of protein-sequence mapping in the package.}
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
}
\details{
  Build annotation data packages for query protein sequences. formatdb and 
  blastall are need to be installed.

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
  package via \url{http://www.biosino.org/PAnnBuilder/example.jsp}.
}
\author{Hong Li}
\examples{
## Set path, version and author for the package.
pkgPath <- tempdir()                                       
version <- "1.0.0"                                     
author <- list()                                       
author[["authors"]] <- "Hong Li"                       
author[["maintainer"]] <- "Hong Li <sysptm@gmail.com>"

## Set query sequences.
tmp = system.file("extdata", "query.example", package="PAnnBuilder")
tmp = readLines(tmp)
tag = grep("^>",tmp)
query <- sapply(1:(length(tag)-1), function(x){ 
     paste(tmp[(tag[x]+1):(tag[x+1]-1)], collapse="") })
query <- c(query, paste(tmp[(tag[length(tag)]+1):length(tmp)], collapse="") )
names(query) = sub(">","",tmp[tag])

## Set parameters for sequence similarity.
blast <- c("blastp", "10.0", "BLOSUM62", "0", "-1", "-1", "T", "F")
names(blast) <- c("p","e","M","W","G","E","U","F")
match <- c(0.00001, 0.95, 0.95)
names(match) <- c("e","c","i")
      
if(FALSE){
    ## NOTE: THESE PACKAGES ARE NO LONGER AVAILABLE, YOU NEED TO GENERATE
    ##       THEM FOLLOWING THE INSTRUCTIONS IN THE VIGNETTE

    ## Use packages "org.Hs.sp.db", "org.Hs.ipi.db" to produce annotation R
    ## package for query sequence. Packages "org.Hs.sp.db", "org.Hs.ipi.db"
    ## can be downloaded from http://www.biosino.org/PAnnBuilder/example.jsp. 
    annPkgs = c("org.Hs.sp.db","org.Hs.ipi.db")  
    seqName = c("org.Hs.spSEQ","org.Hs.ipiSEQ")  
    pSeqBuilder_DB(query, annPkgs, seqName, blast, match, 
    prefix="test1", pkgPath, version, author)    
}
}


