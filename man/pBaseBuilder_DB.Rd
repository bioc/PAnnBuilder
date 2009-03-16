\name{pBaseBuilder_DB}
\alias{pBaseBuilder_DB}
\title{Build Data Packages for Primary Protein Database}
\description{
  Given the URL to SwissProt, TREMBL, IPI or NCBI RefSeq protein data, this
  function creates a SQLite-based annotation data package.
}
\usage{
pBaseBuilder_DB(baseMapType = c("sp","trembl","ipi","refseq"), organism, 
               prefix, pkgPath, version, author)
}
\arguments{
  \item{baseMapType}{a character string that can be either "sp", "trembl",
    "ipi" or "refseq" to indicate which protein database will be used.}
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
  Build annotation data packages for proteins in primary protein database,
  including SwissProt, TREMBL, IPI or NCBI RefSeq Database. Basic annotation 
  information will be integrated with protein entries, including protein   
  sequence, description, coding gene, structure, Gene Ontology, KEGG pathway, 
  Pfam doamin and so on.

  When baseMapType = "sp", protein data are from UniProtKB/Swiss-Prot. 
  (\url{http://expasy.org/sprot/})

  When baseMapType = "trembl", protein data are from UniProtKB/TrEMBL.
  (\url{http://expasy.org/sprot/})

  When baseMapType = "ipi", protein data are from International Protein Index 
  (IPI),and seven organisms are supported: Homo sapiens, Mus musculus, Rattus 
  norvegicus, Danio rerio, Bos taurus, Gallus gallus, Arabidopsis thaliana. 
  (\url{http://http://www.ebi.ac.uk/IPI/IPIhelp.html/})

  When baseMapType = "refseq", protein data are from NCBI Reference Sequence, 
  and six organisms are supported: Homo sapiens, Mus musculus, Rattus norvegicus, 
  Danio rerio, Bos taurus, Xenopus tropicalis. 
  (\url{http://www.ncbi.nlm.nih.gov/RefSeq/})

  Data files in the database will be automatically downloaded to the tmp
  directory, so enough space is needed for the data files. After
  downloading, files are parsed by perl, so perl must be installed.  It may 
  take a long time to parse database and build R package. Alternatively, we have 
  produced diverse R packages by PAnnBuilder, and you can download appropriate 
  package via \url{http://www.biosino.org/PAnnBuilder} .
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
if(interactive()){
    # Build annotation data packages "org.Hs.sp.db" for Homo sapiens
    # proteins in SwissProt.
    pBaseBuilder_DB(baseMapType = "sp", organism = "Homo sapiens",  
                    prefix = "org.Hs.sp", pkgPath = pkgPath, version = version, 
                    author = author)                 
    
    # Build annotation data packages "org.Mm.ipi.db" for Mus musculus
    # proteins in IPI.
    pBaseBuilder_DB(baseMapType = "ipi", organism = "Mus musculus",  
                    prefix = "org.Mm.ipi", pkgPath = pkgPath, version = version, 
                    author = author)
                            
    # Build annotation data packages "org.Rn.ref.db" for Rattus norvegicus
    # proteins in NCBI RefSeq.
    pBaseBuilder_DB(baseMapType = "refseq", organism = "Rattus norvegicus",
                    prefix = "org.Rn.ref", pkgPath = pkgPath, version = version,
                    author = author)
}
}

