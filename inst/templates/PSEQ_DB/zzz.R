datacache <- new.env(hash=TRUE, parent=emptyenv())

#PREFIX#_dbconn <- function() dbconn(datacache)
#PREFIX#_dbfile <- function() dbfile(datacache)
#PREFIX#_dbInfo <- function() dbInfo(datacache)

.onLoad <- function(libname, pkgname)
{
    require("methods", quietly=TRUE)
    require("PAnnBuilder", quietly=TRUE)
    ## Connect to the SQLite DB
    dbfile <- system.file("extdata", "#PREFIX#.sqlite", package="#PKGNAME#", 
        lib.loc=libname)
    assign("dbfile", dbfile, envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)   
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(#PREFIX#_dbconn())
}

