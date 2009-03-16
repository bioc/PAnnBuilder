GOABuilder_DB <- function(organism="Homo sapiens",
              prefix, pkgPath, version, author
              ) {
  ## Build annotation data packages for mapping between protein and gene ontology.
  ##
  ## organism - a character string for the name of the organism of
  ##            concern. [eg: Homo sapiens]
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
  if(any(c(missing(organism), 
    missing(prefix), missing(pkgPath), missing(version), missing(author),
    is.null(organism), 
    is.null(prefix), is.null(pkgPath), is.null(version), is.null(author)))){
      stop(paste("Parameters organism, prefix, pkgPath, ",
        "version, or author can not be missing or NULL",
        sep = ""))
  }
 
  pkgName = paste(prefix,"db",sep=".")
  srcUrls <- getSrcUrl("GOA",organism)
  built <- getSrcBuilt("GOA",organism)
  
  # create annotation package, and write data into "*.sqlite" file     
  createEmptyDPkg(pkgName, pkgPath, force = TRUE, 
      folders = c("man", "R",  paste("inst","extdata",sep=.Platform$file.sep) ))
  drv <- dbDriver("SQLite")
  outputFile <- file.path(pkgPath, pkgName, paste("inst",
      "extdata", sep=.Platform$file.sep), paste(prefix, "sqlite", sep="."))	
  db <- dbConnect(drv, outputFile)
  
  # write data
  writeData_DB("GOA", srcUrls, db)  
  repList <- getRepList_DB(organism=organism, "GOA", srcUrls, built, pkgName)
  writeMeta_DB(db, repList)
  dbDisconnect(db)
  writeManAnno_DB(pkgName, pkgPath, version, author, repList)
}


