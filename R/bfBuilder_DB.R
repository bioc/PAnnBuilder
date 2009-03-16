bfBuilder_DB <- function(src="SysBodyFluid",
             prefix, pkgPath, version, author
             ) {
  if(any(c(missing(src), missing(prefix), missing(pkgPath),
      missing(version), missing(author),
    is.null(src), is.null(prefix), is.null(pkgPath),
    is.null(version), is.null(author)))){
      stop(paste("Parameters src, prefix, pkgPath, ",
        "version, or author can not be missing or NULL",
        sep = ""))
  }
  
  src <- match.arg(src)
  if( !any(src==c("SysBodyFluid")) ){
      stop("Parameter src is not correct, must be SysBodyFluid")
  }  
  pkgName <- paste(prefix,"db",sep=".")  
  srcUrl <- getSrcUrl(src)  
  built <- getSrcBuilt(src) 
  
  # create annotation package, and write data into "*.sqlite" file     
  createEmptyDPkg(pkgName, pkgPath, force = TRUE, 
      folders = c("man", "R",  paste("inst","extdata",sep=.Platform$file.sep) ))
  drv <- dbDriver("SQLite")
  outputFile <- file.path(pkgPath, pkgName, paste("inst",
      "extdata", sep=.Platform$file.sep), paste(prefix, "sqlite", sep="."))	
  db <- dbConnect(drv, outputFile)

  # write data
  writeData_DB(src, srcUrl, db)  
  repList <- getRepList_DB(organism="", src, srcUrl, built, pkgName)
  writeMeta_DB(db, repList)
  dbDisconnect(db)
  writeManAnno_DB(pkgName, pkgPath, version, author, repList)             
}
