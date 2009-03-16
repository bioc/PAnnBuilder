PeptideAtlasBuilder_DB <- function(name=c("Human","Human Plasma",
                       "Saccharomyces cerevisiae","Drosophila Melanogaster",
                       "Mouse Plasma","Halobacterium"),
                       prefix, pkgPath, version, author
                       ) {
  ## Build annotation data packages for PeptideAtlas
  ##
  ##  name - a character string that indicates the interested species.
  ##         "Human"
  ##         "Human Plasma"
  ##         "Saccharomyces cerevisiae"
  ##         "Drosophila Melanogaster"
  ##         "Mouse Plasma"
  ##         "Halobacterium"
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
  
  if(any(c(missing(name), missing(prefix), missing(pkgPath),
      missing(version), missing(author),
    is.null(name), is.null(prefix), is.null(pkgPath),
    is.null(version), is.null(author)))){
      stop(paste("Parameters name, prefix, pkgPath, ",
        "version, or author can not be missing or NULL",
        sep = ""))
  }
  
  name <- match.arg(name)
  if( !any(name==c("Human","Human Plasma","Saccharomyces cerevisiae",
      "Drosophila Melanogaster","Mouse Plasma","Halobacterium") ) ){
      stop("Parameter name is not correct, must be 'Human','Human Plasma',
          'Saccharomyces cerevisiae','Drosophila Melanogaster','Mouse Plasma' or 
          'Halobacterium'")
  }
 
  pkgName = paste(prefix,"db",sep=".") 
  srcUrls <- getSrcUrl("PeptideAtlas",name)
  built <- getSrcBuilt("PeptideAtlas",name)
  
  # create annotation package, and write data into "*.sqlite" file     
  createEmptyDPkg(pkgName, pkgPath, force = TRUE, 
      folders = c("man", "R",  paste("inst","extdata",sep=.Platform$file.sep) ))
  drv <- dbDriver("SQLite")
  outputFile <- file.path(pkgPath, pkgName, paste("inst",
      "extdata", sep=.Platform$file.sep), paste(prefix, "sqlite", sep="."))	
  db <- dbConnect(drv, outputFile)
  
  # write data
  writeData_DB("PeptideAtlas", srcUrls, db)  
  repList <- getRepList_DB(organism=name, "PeptideAtlas", srcUrls, built, pkgName)
  writeMeta_DB(db, repList)
  dbDisconnect(db)
  writeManAnno_DB(pkgName, pkgPath, version, author, repList)   
}  
                          
                          

    