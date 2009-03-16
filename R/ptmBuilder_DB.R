ptmBuilder_DB <- function(src="SysPTM",
              prefix, pkgPath, version, author
              ) {
  ## Build annotation data packages for post-translational modification sites 
  ## of protein.
  ##
  ## src - a character string that can be "SysPTM" to indicate which 
  ##       post-translational modification database will be used.
  ##       "SysPTM": http://www.biosino.org/SysPTM
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
  if( !any(src==c("SysPTM")) ){
      stop("Parameter src is not correct, must be SysPTM")
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

