\name{createAnnObjs}
\alias{createAnnObjs}
\alias{createSeeds}
\alias{dbschema}
\alias{dbschema,DBIConnection-method}
\alias{dbschema,environment-method}
\alias{dbschema,AnnDbObj-method}
\title{Creates the AnnObj instances}
\description{
  These functions creates the AnnObj instances for the SQLite-based annotation 
  data packages.
}
\usage{
createSeeds(type) 

createAnnObjs(type, prefix, datacache, dbconn)
}
\arguments{
  \item{type}{character string, giving the name of concerned database. Possible 
    values of type are : "sp", "trembl", "ipi", "refseq", "geneint", "intact", "mppi", 
    "3DID", "DOMINE", "BaCelLo", "DBSubLoc", "SCOP", "HomoloGene", "InParanoid", 
    "PeptideAtlas", "SysPTM", "SysBodyFluid", "GOA", "GO", "KEGGNAME", "PFAMNAME", 
    "INTERPRONAME", "TAXNAME", "dName", "cross", "pSeq"}
  \item{prefix}{the prefix of the name of the data package to be built. (e.g. 
    "hsaSP"). The name of builded package is prefix+".db". }
  \item{datacache}{a R environment, giving the name of "*.sqlite" database file, 
    and a object which extends DBIConnection to the database.}
  \item{dbconn}{a connection object to the package.}
}
\details{
  Given the abbreviation of databases, \code{\link{createSeeds}} returns a list 
    which describes the set of AnnObj objects.
    
  \code{\link{createAnnObjs}} creates the AnnObj instances based on the values 
    of \code{\link{createSeeds}}.
    
  \code{dbschema(x, file="", show.indices=FALSE)}: Print the schema definition
  of the SQLite DB. Also works if \code{x} is a DBIConnection object. The 
  \code{file} argument must be a connection, or a character string naming the 
  file to print to (see the \code{file} argument of the \code{\link[base]{cat}} 
  function for the details). The CREATE INDEX statements are not shown by default.
  Use \code{show.indices=TRUE} to get them.
  
}
\author{Hong Li}


