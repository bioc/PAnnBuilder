intBuilder_DB <- function(src=c("geneint","intact","mppi","3DID","DOMINE"), 
              prefix, pkgPath, version, author
              ) {

  ## Build annotation data packages for protein-protein or domain-domain 
  ## interaction.
  ##
  ## src - a character string of source data, possible values are:                         
  ##       "geneint": Gene Gene Interaction data from NCBI.
  ##       "intact": Protein Protein Interactions from IntAct database.
  ##       "mppi": Protein Protein Interactions from Munich Information Center 
  ##               for Protein Sequences (MIPS).
  ##       "3DID": Interacting Protein Domains of known three-dimensional 
  ##               structure.
  ##       "DOMINE": Database of known and predicted protein domain 
  ##                 (domain-domain) interactions.
  ##
  ## prefix - the prefix of the data package to be built (e.g. "hsaSP"). The name
  ##          of builded package is prefix+".db".  
  ##
  ## pkgPath - a character string for the full path of an existing
  ##           directory where the built backage will be stored.  
  ##
  ## version - a character string for the version number.
  ##
  ## author - a list with named elements "authors" containing a character
  ##          vector of author names and "maintainer" containing the complete
  ##          character string for the maintainer field, for example, "Jane
  ##          Doe <jdoe@doe.com>".
  ##  
  ## Copyright 2008, Hong Li, all rights reserved.
  ##  
  
  if(any(c(missing(src), missing(prefix), missing(pkgPath),
      missing(version), missing(author),
    is.null(src), is.null(prefix), is.null(pkgPath),
    is.null(version), is.null(author)))){
      stop(paste("Parameters src, prefix, pkgPath, ",
        "version, or author can not be missing or NULL",
        sep = ""))
  }

  src <- match.arg(src)
  if( !any(src==c("geneint","intact","mppi","3DID","DOMINE")) ){
      stop("Parameter src is not correct, must be geneint, intact, mppi, 3DID or
      DOMINE")
  } 

  pkgName = paste(prefix,"db",sep=".")
  srcUrls <- getSrcUrl(src)  
  built <- getSrcBuilt(src)  

  # create annotation package, and write data into "*.sqlite" file     
  createEmptyDPkg(pkgName, pkgPath, force = TRUE, 
      folders = c("man", "R",  paste("inst","extdata",sep=.Platform$file.sep) ))
  drv <- dbDriver("SQLite")
  outputFile <- file.path(pkgPath, pkgName, paste("inst",
      "extdata", sep=.Platform$file.sep), paste(prefix, "sqlite", sep="."))	
  db <- dbConnect(drv, outputFile)
  
  # write data
  writeData_DB(src, srcUrls, db)  
  repList <- getRepList_DB(organism="", src, srcUrls, built, pkgName)
  writeMeta_DB(db, repList)
  dbDisconnect(db)
  writeManAnno_DB(pkgName, pkgPath, version, author, repList)
}



