dNameBuilder_DB<- function(
    prefix, pkgPath, version, author
    ) {
  ## Build annotation data packages for mapping between id and name.
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
  ## lazyLoad - a boolean indicating whether a lazy load database will be created
  ##  
  ## Copyright 2008, Hong Li, all rights reserved.
  ##
  if(any(c(missing(prefix), missing(pkgPath),
    missing(version), missing(author),
   is.null(prefix), is.null(pkgPath),
    is.null(version), is.null(author)))){
      stop(paste("Parameters prefix, pkgPath, ",
        "version, or author can not be missing or NULL",
        sep = ""))
  }

  type <- c("GO","KEGG","PFAM","INTERPRO","TAX")
  type <- paste(type,"NAME",sep="")
  
  pkgName = paste(prefix,"db",sep=".")
  srcUrls <- sapply(type,function(x){getSrcUrl(x)})
  built <- sapply(type,function(x){getSrcBuilt(x)})
  names(srcUrls) <- type
  names(built) <- type

  # create annotation package, and write data into "*.sqlite" file     
  createEmptyDPkg(pkgName, pkgPath, force = TRUE, 
      folders = c("man", "R",  paste("inst","extdata",sep=.Platform$file.sep) ))
  drv <- dbDriver("SQLite")
  outputFile <- file.path(pkgPath, pkgName, paste("inst",
      "extdata", sep=.Platform$file.sep), paste(prefix, "sqlite", sep="."))	
  db <- dbConnect(drv, outputFile)

  sapply(type,function(x){writeName_DB(x, srcUrls[x], db)})    
  repList <- getRepList_DB(organism="", type="dName", srcUrls, built, pkgName)
  writeMeta_DB(db, repList)
  writeManAnno_DB(pkgName, pkgPath, version, author, repList)  
  dbDisconnect(db)  
}
