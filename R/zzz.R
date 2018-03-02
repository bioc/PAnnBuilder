.onLoad <- function(libname, pkgname) {
    if(.Platform$OS.type == "windows" && require(Biobase) && interactive()
        && .Platform$GUI == "Rgui"){
            addVigs2WinMenu("PAnnBuilder")
    }   
    ## Get the URL stored in the "sourceURLs"
    tmpEnv <- new.env()
    data("taxLists","sourceURLs", package="PAnnBuilder", envir=tmpEnv)
    options(PAnnBuilderTaxLists=tmpEnv$taxLists)
    options(PAnnBuilderSourceUrls=tmpEnv$sourceURLs)
    rm(tmpEnv)
}
.onAttach <- function(libname, pkgname) {
    msg <- sprintf(
        "Package '%s' is deprecated and will be removed from Bioconductor
         version %s", pkgname, "3.7")
    .Deprecated(msg=paste(strwrap(msg, exdent=2), collapse="\n"))
}
