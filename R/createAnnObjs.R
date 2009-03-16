## dbschema is from "AnnotationDbi" package.
setMethod("dbschema", "DBIConnection",
    function(x, file="", show.indices=FALSE)
    {
        schema <- dbmeta(x, "DBSCHEMA")
        version <- dbmeta(x, "DBSCHEMAVERSION")
        file <- system.file("DBschemas",
                            paste("schemas_", version, sep=""),
                            paste(schema, ".sql", sep=""),
                            package="PAnnBuilder")
        lines <- readLines(file)
        if (!show.indices) {
            ## Remove the CREATE INDEX lines
            createIndexStart <- "CREATE INDEX"
            createIndexEnd <- ";"            
            ii <- which(substr(lines, 1, nchar(createIndexStart)) == createIndexStart
                      & substr(lines, nchar(lines)-nchar(createIndexEnd)+1, nchar(lines)) == createIndexEnd)
            ii <- setdiff(ii, grep(createIndexEnd, substr(lines, 1, nchar(lines)-nchar(createIndexEnd)), fixed=TRUE))
            if( length(ii)>0 ){
              ## Remove comments preceding the CREATE INDEX blocks
              beforeLastBlock <- function(ii, i)
              {
                  while ((i >= 1) && !(i %in% ii))
                      i <- i - 1
                  while ((i >= 1) && (i %in% ii))
                      i <- i - 1
                  i
              }
              i <- max(ii)
              while (i >= 1) {
                  i <- beforeLastBlock(ii, i)
                  while (i >= 1) {
                      if (substr(lines[i], 1, 2) != "--")
                          break
                      ii <- c(i, ii)
                      i <- i - 1
                  }
              }
              lines <- lines[-ii]
            }
        }
        ## Remove empty trailing lines
        ii <- integer(0)
        i <- length(lines)
        while ((i >= 1) && (lines[i] == "")) {
            ii <- c(i, ii)
            i <- i - 1
        }
        lines <- lines[-ii]
        cat(lines, sep="\n")
    }
)

setMethod("dbschema", "environment",
    function(x, file="", show.indices=FALSE)
        dbschema(dbconn(x), file=file, show.indices=show.indices))

setMethod("dbschema", "AnnDbObj",
    function(x, file="", show.indices=FALSE)
        dbschema(x@datacache, file=file, show.indices=show.indices))

## dbGetTable is from "AnnotationDbi" package, and modified by Hong Li.
dbGetTable <- function (conn, tablename, extra.SQL = NULL) 
{
    SQL <- paste("SELECT * FROM ", tablename, sep = "")
    if (!is.null(extra.SQL)) 
        SQL <- paste(SQL, extra.SQL)
    dbGetQuery(conn, SQL)
}

## createMAPCOUNTS is from "AnnotationDbi" package, and modified by Hong Li.
createMAPCOUNTS <- function (dbconn, prefix){
    data <- dbGetTable(dbconn, "map_counts", "WHERE map_name != 'TOTAL' ORDER BY map_name")
    MAPCOUNTS <- data[[2]]
    names(MAPCOUNTS) <- paste(prefix, data[["map_name"]], sep = "")
    MAPCOUNTS
}

createSeeds <- function(type){    
    type = toupper(type)
    switch(type,
           SP = return(list(
                            list(
                                objName="SPAC",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="sp_id",
                                        Rcolname="sp_ac"
                                    )
                                )
                            ),                            
                            list(
                                objName="LEN",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="sp_id",
                                        Rcolname="len"
                                    )
                                )
                            ),
                            list(
                                objName="MW",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="sp_id",
                                        Rcolname="mw"
                                    )
                                )
                            ),
                            list(
                                objName="DE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="de",
                                        Lcolname="sp_id",
                                        Rcolname="de"
                                    )
                                )
                            ),
                            list(
                                objName="SYMBOL",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="sp_id",
                                        Rcolname="symbol"
                                    )
                                )
                            ),
                            list(
                                objName="UNIGENE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="sp_id",
                                        Rcolname="unigene_id"
                                    )
                                )
                            ),
                            list(
                                objName="KEGG",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="kegg",
                                        Lcolname="sp_id",
                                        Rcolname="kegg_id"
                                    )
                                )
                            ),
                            list(
                                objName="FUNCTION",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="sp_id",
                                        Rcolname="functions"
                                    )
                                )
                            ),                            
                            list(
                                objName="SUBCELL",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="sp_id",
                                        Rcolname="subcelluar"
                                    )
                                )
                            ),
                            list(
                                objName="TISSUE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="sp_id",
                                        Rcolname="tissue"
                                    )
                                )
                            ),                                
                            list(
                                objName="DISEASE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="sp_id",
                                        Rcolname="disease"
                                    )
                                )
                            ),
                            list(
                                objName="SPACs",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="spac",
                                        Lcolname="sp_id",
                                        Rcolname="sp_acs"
                                    )
                                )
                            ),
                            list(
                                objName="PMID",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="pubmed",
                                        Lcolname="sp_id",
                                        Rcolname="pubmed_id"
                                    )
                                )
                            ),
                            list(
                                objName="REFSEQ",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="refseq",
                                        Lcolname="sp_id",
                                        Rcolname="ref_id"
                                    )
                                )
                            ),                               
                            list(
                                objName="GENEID",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="geneid",
                                        Lcolname="sp_id",
                                        Rcolname="gene_id"
                                    )
                                )
                            ),                               
                            list(
                                objName="PDB",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="pdb",
                                        Lcolname="sp_id",
                                        Rcolname="pdb_id"
                                    )
                                )
                            ),                             
                            list(
                                objName="GN",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="alias",
                                        Lcolname="sp_id",
                                        Rcolname="alias_symbol"
                                    )
                                )
                            ),                                                           
                            list(
                                objName="INT",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="int",
                                        Lcolname="sp_id",
                                        Rcolname="sp_ac_b"
                                    )
                                )
                            ),                              
                            list(
                                objName="PATH",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="path",
                                        Lcolname="sp_id",
                                        Rcolname="path_id"
                                    )
                                )
                            ),
                            list(
                                objName="INTERPRO",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="interpro",
                                        Lcolname="sp_id",
                                        Rcolname="interpro_id"
                                    )
                                )
                            ),
                            list(
                                objName="PFAM",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="pfam",
                                        Lcolname="sp_id",
                                        Rcolname="pfam_id"
                                    )
                                )
                            ),
                            list(
                                objName="PROSITE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="prosite",
                                        Lcolname="sp_id",
                                        Rcolname="prosite_id"
                                    )
                                )
                            ),
                            list(
                                objName="GO",
                                Class="GoAnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="go", # no rightmost table for a Go3AnnDbBimap
                                        Lcolname="sp_id",
                                        tagname=c(Evidence="{evidence}"),
                                        Rcolname="go_id",
                                        Rattribnames=c(Ontology="{ontology}")
                                    )
                                )                            
                            ),
                            list(
                                objName="SEQ",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="seq",
                                        Lcolname="sp_id",
                                        Rcolname="seq"
                                    )
                                )
                            )       
                     )),
           IPI = return(list(
                            list(
                                objName="IPIAC",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="ipi_ac"
                                    )
                                )
                            ),
                            list(
                                objName="IPIACs",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="ipiac",
                                        Lcolname="ipi_id",
                                        Rcolname="ipi_acs"
                                    )
                                )
                            ),                            
                            list(
                                objName="LEN",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="len"
                                    )
                                )
                            ),
                            list(
                                objName="MW",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="mw"
                                    )
                                )
                            ),
                            list(
                                objName="DE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="de"
                                    )
                                )
                            ),                            
                            list(
                                objName="SYMBOL",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="symbol"
                                    )
                                )
                            ),
                            list(
                                objName="SPAC",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="sp_ac"
                                    )
                                )
                            ),
                            list(
                                objName="SPID",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="sp_id"
                                    )
                                )
                            ),
                            list(
                                objName="REFSEQ",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="ref_id"
                                    )
                                )
                            ),
                            list(
                                objName="GI",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="gi"
                                    )
                                )
                            ),
                            list(
                                objName="GENEID",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="gene_id"
                                    )
                                )
                            ),
                            list(
                                objName="UNIGENE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="unigene_id"
                                    )
                                )
                            ),
                            list(
                                objName="KEGG",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="ipi_id",
                                        Rcolname="kegg_id"
                                    )
                                )
                            ),
                            list(
                                objName="PATH",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="path",
                                        Lcolname="ipi_id",
                                        Rcolname="path_id"
                                    )
                                )
                            ),
                            list(
                                objName="INTERPRO",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="interpro",
                                        Lcolname="ipi_id",
                                        Rcolname="interpro_id"
                                    )
                                )
                            ),
                            list(
                                objName="PFAM",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="pfam",
                                        Lcolname="ipi_id",
                                        Rcolname="pfam_id"
                                    )
                                )
                            ),
                            list(
                                objName="PROSITE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="prosite",
                                        Lcolname="ipi_id",
                                        Rcolname="prosite_id"
                                    )
                                )
                            ),
                            list(
                                objName="GO",
                                Class="GoAnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="go", # no rightmost table for a Go3AnnDbBimap
                                        Lcolname="ipi_id",
                                        tagname=c(Evidence="{evidence}"),
                                        Rcolname="go_id",
                                        Rattribnames=c(Ontology="{ontology}")
                                    )
                                )                            
                            ),
                            list(
                                objName="SEQ",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="seq",
                                        Lcolname="ipi_id",
                                        Rcolname="seq"
                                    )
                                )
                            )   
                     )),
           REFSEQ = return(list(
                            list(
                                objName="REFSEQ",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="gi",
                                        Rcolname="ref_id"
                                    )
                                )
                            ),
                            list(
                                objName="GENEID",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="gi",
                                        Rcolname="gene_id"
                                    )
                                )
                            ),
                            list(
                                objName="SYMBOL",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="gi",
                                        Rcolname="symbol"
                                    )
                                )
                            ),                               
                            list(
                                objName="DE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="gi",
                                        Rcolname="de"
                                    )
                                )
                            ),
                            list(
                                objName="KEGG",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="basic",
                                        Lcolname="gi",
                                        Rcolname="kegg_id"
                                    )
                                )
                            ),
                            list(
                                objName="PATH",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="path",
                                        Lcolname="gi",
                                        Rcolname="path_id"
                                    )
                                )
                            ),                            
                            list(
                                objName="GO",
                                Class="GoAnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="go", # no rightmost table for a Go3AnnDbBimap
                                        Lcolname="gi",
                                        tagname=c(Evidence="{evidence}"),
                                        Rcolname="go_id",
                                        Rattribnames=c(Ontology="{ontology}")
                                    )
                                )                            
                            ),
                            list(
                                objName="SEQ",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="seq",
                                        Lcolname="gi",
                                        Rcolname="seq"
                                    )
                                )
                            )    
                     )),    
           SYSBODYFLUID = return(list(  
                            list(
                                objName="BF",
                                Class="IpiAnnDbMap",
                                L2Rchain=list(
                                    list(
                                        tablename="sysbodyfluid", 
                                        Lcolname="ipi_id",
                                        Rcolname="pubmed_id",
                                        Rattribnames=c(BF="{body_fluid}")
                                    )
                                )                            
                            ),
                            list(
                                objName="BF2IPI",
                                Class="IpiAnnDbMap",
                                L2Rchain=list(
                                    list(
                                        tablename="sysbodyfluid", 
                                        Lcolname="body_fluid",
                                        Rcolname="pubmed_id",
                                        Rattribnames=c(IPIID="{ipi_id}")
                                    )
                                )                            
                            ),
                            list(
                                objName="PMID2TITLE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="paper", 
                                        Lcolname="pubmed_id",
                                        Rcolname="title"                                      
                                    )
                                )                            
                            ),
                            list(
                                objName="PMID2PLATFORM",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="paper", 
                                        Lcolname="pubmed_id",
                                        Rcolname="platform"                                       
                                    )
                                )                            
                            ),
                            list(
                                objName="PMID2BF",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="paper", 
                                        Lcolname="pubmed_id",
                                        Rcolname="body_fluid"                                       
                                    )
                                )                            
                            ),
                            list(
                                objName="PMID2ENGINE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="paper", 
                                        Lcolname="pubmed_id",
                                        Rcolname="search_engine"                                       
                                    )
                                )                            
                            ),
                            list(
                                objName="PMID2SAMPLE",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="paper", 
                                        Lcolname="pubmed_id",
                                        Rcolname="sample"                                       
                                    )
                                )                            
                            )               
                          )), 
           SYSPTM = return(list(  
                      list(
                           objName="PTMTYPE",
                           Class="AnnDbBimap",
                           L2Rchain=list(
                               list(
                                   tablename="sysptm", 
                                   Lcolname="ptm_site",
                                   Rcolname="ptm_type"                                 
                               )
                           )                            
                       ),
                       list(
                           objName="ORGANISM",
                           Class="AnnDbBimap",
                           L2Rchain=list(
                               list(
                                   tablename="basic",
                                   Lcolname="sysptm_id",
                                   Rcolname="organism"
                               )
                           )
                       ),
                       list(
                           objName="SEQ",
                           Class="AnnDbBimap",
                           L2Rchain=list(
                               list(
                                   tablename="basic",
                                   Lcolname="sysptm_id",
                                   Rcolname="seq"
                               )
                           )
                       ), 
                       list(
                           objName="GENEID",
                           Class="AnnDbBimap",
                           L2Rchain=list(
                               list(
                                   tablename="basic",
                                   Lcolname="sysptm_id",
                                   Rcolname="gene_id"
                               )
                           )
                       ),
                       list(
                           objName="DE",
                           Class="AnnDbBimap",
                           L2Rchain=list(
                               list(
                                   tablename="de",
                                   Lcolname="sysptm_id",
                                   Rcolname="de"
                               )
                           )
                       ),
                       list(
                           objName="GN",
                           Class="AnnDbBimap",
                           L2Rchain=list(
                               list(
                                   tablename="alias",
                                   Lcolname="sysptm_id",
                                   Rcolname="alias_symbol"
                               )
                           )
                       ),
                       list(
                           objName="IDs",
                           Class="IpiAnnDbMap",
                           L2Rchain=list(
                               list(
                                   tablename="xref",
                                   Lcolname="sysptm_id",
                                   Rcolname="database",                                   
                                   Rattribnames=c(IDs="{protein_id}")
                               )
                           )
                       ) 
                    )),
           SCOP = return(list(
                    list(
                         objName="TYPE",
                         Class="AnnDbBimap",
                         L2Rchain=list(
                             list(
                                 tablename="des", 
                                 Lcolname="scop_id",
                                 Rcolname="type"              
                             )
                         )                            
                    ),
                    list(
                        objName="CLASSIFICATION",
                        Class="AnnDbBimap",
                        L2Rchain=list(
                            list(
                                tablename="des", 
                                Lcolname="scop_id",
                                Rcolname="class"                          
                            )
                        )
                    ),
                    list(
                         objName="NAME",
                         Class="AnnDbBimap",
                         L2Rchain=list(
                             list(
                                 tablename="des", 
                                 Lcolname="scop_id",
                                 Rcolname="name"                            
                             )
                         )                            
                    ),
                    list(
                        objName="DE",
                        Class="AnnDbBimap",
                        L2Rchain=list(
                            list(
                                tablename="des", 
                                Lcolname="scop_id",
                                Rcolname="de"
                            )
                        )
                    ),
                    list(
                         objName="PARENT",
                         Class="AnnDbBimap",
                         L2Rchain=list(
                             list(
                                 tablename="parent", 
                                 Lcolname="scop_id",
                                 Rcolname="parent_id"                          
                             )
                         )                            
                    ),
                    list(
                        objName="CHILDREN",
                        Class="AnnDbBimap",
                        L2Rchain=list(
                            list(
                                tablename="children", 
                                Lcolname="scop_id",
                                Rcolname="children_id"
                            )
                        )
                    ),
                    list(
                        objName="PDB2CL",
                        Class="IpiAnnDbMap",
                        L2Rchain=list(
                            list(
                                tablename="pdb",
                                Lcolname="pdb_id",
                                Rcolname="pdb_chain",                                   
                                Rattribnames=c(cl="{cl}")
                            )
                        )
                    ),
                    list(
                        objName="PDB2CF",
                        Class="IpiAnnDbMap",
                        L2Rchain=list(
                            list(
                                tablename="pdb",
                                Lcolname="pdb_id",
                                Rcolname="pdb_chain",                                   
                                Rattribnames=c(cf="{cf}")
                            )
                        )
                    ),
                    list(
                        objName="PDB2SF",
                        Class="IpiAnnDbMap",
                        L2Rchain=list(
                            list(
                                tablename="pdb",
                                Lcolname="pdb_id",
                                Rcolname="pdb_chain",                                   
                                Rattribnames=c(sf="{sf}")
                            )
                        )
                    ),
                    list(
                        objName="PDB2FA",
                        Class="IpiAnnDbMap",
                        L2Rchain=list(
                            list(
                                tablename="pdb",
                                Lcolname="pdb_id",
                                Rcolname="pdb_chain",                                   
                                Rattribnames=c(fa="{fa}")
                            )
                        )
                    ),
                    list(
                        objName="PDB2DM",
                        Class="IpiAnnDbMap",
                        L2Rchain=list(
                            list(
                                tablename="pdb",
                                Lcolname="pdb_id",
                                Rcolname="pdb_chain",                                   
                                Rattribnames=c(dm="{dm}")
                            )
                        )
                    ),
                    list(
                        objName="PDB2SP",
                        Class="IpiAnnDbMap",
                        L2Rchain=list(
                            list(
                                tablename="pdb",
                                Lcolname="pdb_id",
                                Rcolname="pdb_chain",                                   
                                Rattribnames=c(sp="{sp}")
                            )
                        )
                    ),
                    list(
                        objName="PDB2PX",
                        Class="IpiAnnDbMap",
                        L2Rchain=list(
                            list(
                                tablename="pdb",
                                Lcolname="pdb_id",
                                Rcolname="pdb_chain",                                   
                                Rattribnames=c(px="{px}")
                            )
                        )
                    )                    
                  )), 
           GENEINT = return(list(   
                       list(
                            objName="GGI",
                            Class="AnnDbBimap",
                            L2Rchain=list(
                                list(
                                    tablename="geneint", 
                                    Lcolname="gene_id_a",
                                    Rcolname="gene_id_b"                                   
                                )
                            )                            
                       ),                       
                       list(
                            objName="TAXID",
                            Class="AnnDbBimap",
                            L2Rchain=list(
                                list(
                                    tablename="tax", 
                                    Lcolname="gene_id",
                                    Rcolname="tax_id"                                  
                                )
                            )                            
                       )
                     )),
           INTACT = return(list(   
                       list(
                            objName="PPI",
                            Class="AnnDbBimap",
                            L2Rchain=list(
                                list(
                                    tablename="intact", 
                                    Lcolname="up_ac_a",
                                    Rcolname="up_ac_b"                                   
                                )
                            )                            
                       ),                       
                       list(
                            objName="TAXID",
                            Class="AnnDbBimap",
                            L2Rchain=list(
                                list(
                                    tablename="tax", 
                                    Lcolname="up_ac",
                                    Rcolname="tax_id"                                   
                                )
                            )                            
                       )
                     )), 
           MPPI  = return(list(  
                    list(
                         objName="PPI",
                         Class="AnnDbBimap",
                         L2Rchain=list(
                             list(
                                 tablename="mppi", 
                                 Lcolname="up_ac_a",
                                 Rcolname="up_ac_b"                                   
                             )
                         )                            
                    )
                  )),                                    
           "3DID" = return(list(  
                    list(
                         objName="DDI",
                         Class="AnnDbBimap",
                         L2Rchain=list(
                             list(
                                 tablename="did", 
                                 Lcolname="pfam_a",
                                 Rcolname="pfam_b"                                   
                             )
                         )                            
                    )
                  )),                          
           DOMINE = return(list(  
                    list(
                         objName="DDI",
                         Class="IpiAnnDbMap",
                         L2Rchain=list(
                             list(
                                 tablename="domine", 
                                 Lcolname="pfam_a",
                                 Rcolname="method",
                                 Rattribnames=c(pfam_b="{pfam_b}")
                             )
                         )                            
                    ),                       
                    list(
                         objName="METHOD",
                         Class="AnnDbBimap",
                         L2Rchain=list(
                             list(
                                 tablename="method", 
                                 Lcolname="method",
                                 Rcolname="method_de"                                
                             )
                         )                            
                    )
                  )),
           BACELLO = return(list(  
                       list(
                           objName="SEQ",
                           Class="AnnDbBimap",
                           L2Rchain=list(
                               list(
                                   tablename="seq",
                                   Lcolname="sp_id",
                                   Rcolname="seq"
                               )
                           )
                       ),                     
                       list(
                            objName="SUBCELL",
                            Class="AnnDbBimap",
                            L2Rchain=list(
                                list(
                                    tablename="bacello", 
                                    Lcolname="sp_id",
                                    Rcolname="subcell"                                   
                                )
                            )                            
                       )
                     )),
           DBSUBLOC = return(list(  
                        list(
                            objName="SEQ",
                            Class="AnnDbBimap",
                            L2Rchain=list(
                                list(
                                    tablename="dbsubloc",
                                    Lcolname="sp_ac",
                                    Rcolname="seq"
                                )
                            )
                        ),                     
                        list(
                             objName="SUBCELL",
                             Class="AnnDbBimap",
                             L2Rchain=list(
                                 list(
                                     tablename="dbsubloc", 
                                     Lcolname="sp_ac",
                                     Rcolname="subcell"                                  
                                 )
                             )                            
                        ),
                        list(
                            objName="ORGANISM",
                            Class="AnnDbBimap",
                            L2Rchain=list(
                                list(
                                    tablename="dbsubloc",
                                    Lcolname="sp_ac",
                                    Rcolname="organism"
                                )
                            )
                        ),                     
                        list(
                             objName="DE",
                             Class="AnnDbBimap",
                             L2Rchain=list(
                                 list(
                                     tablename="dbsubloc", 
                                     Lcolname="sp_ac",
                                     Rcolname="de"                                  
                                 )
                             )                            
                        )
                     )),
           GOA = return(list( 
                   list(
                       objName="GO",
                       Class="GoAnnDbBimap",
                       L2Rchain=list(
                           list(
                               tablename="go", 
                               Lcolname="sp_ac",
                               tagname=c(Evidence="{evidence}"),
                               Rcolname="go_id",
                               Rattribnames=c(Ontology="{ontology}")
                           )
                       )                       
                   ), 
                   list(
                        objName="DE",
                        Class="AnnDbBimap",
                        L2Rchain=list(
                            list(
                                tablename="de", 
                                Lcolname="sp_ac",
                                Rcolname="de"                             
                            )
                        )                            
                   ), 
                   list(
                        objName="SPID",
                        Class="AnnDbBimap",
                        L2Rchain=list(
                            list(
                                tablename="id", 
                                Lcolname="sp_ac",
                                Rcolname="sp_id"                               
                            )
                        )                            
                   )
                 )), 
           HOMOLOGENE = return(list( 
                          list(
                              objName="HOMOLOG",
                              Class="AnnDbBimap",
                              L2Rchain=list(
                                  list(
                                      tablename="homolog",
                                      Lcolname="gene_id",
                                      Rcolname="homolog_id"
                                  )
                              )
                          ),
                          list(
                               objName="HOMOLOG2GI",
                               Class="AnnDbBimap",
                               L2Rchain=list(
                                   list(
                                       tablename="homolog", 
                                       Lcolname="homolog_id",
                                       Rcolname="gi"                                 
                                   )
                               )                            
                          ),
                          list(
                               objName="HOMOLOG2AC",
                               Class="AnnDbBimap",
                               L2Rchain=list(
                                   list(
                                       tablename="homolog", 
                                       Lcolname="homolog_id",
                                       Rcolname="ncbi_ac"                                 
                                   )
                               )                            
                          ),
                          list(
                               objName="SYMBOL",
                               Class="AnnDbBimap",
                               L2Rchain=list(
                                   list(
                                       tablename="homolog", 
                                       Lcolname="gene_id",
                                       Rcolname="symbol"                              
                                   )
                               )                            
                          ),
                          list(
                               objName="TAXID",
                               Class="AnnDbBimap",
                               L2Rchain=list(
                                   list(
                                       tablename="homolog", 
                                       Lcolname="gene_id",
                                       Rcolname="tax_id"                             
                                   )
                               )                            
                          ),
                          list(
                               objName="ORGANISM",
                               Class="AnnDbBimap",
                               L2Rchain=list(
                                   list(
                                       tablename="tax", 
                                       Lcolname="tax_id",
                                       Rcolname="organism"                                
                                   )
                               )                            
                          )
                        )), 
           INPARANOID = return(list( 
                          list(
                              objName="ORTHOLOG",
                              Class="AnnDbBimap",
                              L2Rchain=list(
                                  list(
                                      tablename="ortholog",
                                      Lcolname="protein_id",
                                      Rcolname="ortholog_id"
                                  )
                              )
                          ),
                          list(
                               objName="ORGANISM",
                               Class="AnnDbBimap",
                               L2Rchain=list(
                                   list(
                                       tablename="ortholog", 
                                       Lcolname="protein_id",
                                       Rcolname="organism"                               
                                   )
                               )                            
                          ),
                          list(
                               objName="SEQ",
                               Class="AnnDbBimap",
                               L2Rchain=list(
                                   list(
                                       tablename="seq", 
                                       Lcolname="protein_id",
                                       Rcolname="seq"                             
                                   )
                               )                            
                          )
                        )), 
           PEPTIDEATLAS = return(list( 
                            list(
                                objName="PEP2PROTEIN",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="pep2protein",
                                        Lcolname="peptide_id",
                                        Rcolname="protein_position"
                                        )
                                 )                            
                            ),
                            list(
                                objName="PEP2CHR",
                                Class="AnnDbBimap",
                                L2Rchain=list(
                                    list(
                                        tablename="pep2chr",
                                        Lcolname="peptide_id",
                                        Rcolname="chr_position"
                                        )
                                 )                            
                            ),                        
                            list(
                                 objName="SEQ",
                                 Class="AnnDbBimap",
                                 L2Rchain=list(
                                     list(
                                         tablename="seq", 
                                         Lcolname="peptide_id",
                                         Rcolname="seq"                            
                                     )
                                 )                            
                            )
                          )), 
           DNAME = return(list( 
                     list(
                          objName="GO2NAME",
                          Class="AnnDbBimap",
                          L2Rchain=list(
                              list(
                                  tablename="go",
                                  Lcolname="go_id",
                                  Rcolname="go_name"
                              )
                          )
                     ),                          
                     list(
                          objName="PATH2DE",
                          Class="AnnDbBimap",
                          L2Rchain=list(
                              list(
                                  tablename="kegg", 
                                  Lcolname="path_id",
                                  Rcolname="path_de"                            
                              )
                          )                            
                     ),
                     list(
                          objName="PFAM2DE",
                          Class="AnnDbBimap",
                          L2Rchain=list(
                              list(
                                  tablename="pfam",
                                  Lcolname="pfam_id",
                                  Rcolname="pfam_de"
                              )
                          )
                     ),                          
                     list(
                          objName="PFAM2NAME",
                          Class="AnnDbBimap",
                          L2Rchain=list(
                              list(
                                  tablename="pfam", 
                                  Lcolname="pfam_id",
                                  Rcolname="pfam_name"                            
                              )
                          )                            
                     ),
                     list(
                          objName="INTERPRO2NAME",
                          Class="AnnDbBimap",
                          L2Rchain=list(
                              list(
                                  tablename="interpro",
                                  Lcolname="interpro_id",
                                  Rcolname="interpro_name"
                              )
                          )
                     ),                          
                     list(
                          objName="TAXID2NAME",
                          Class="AnnDbBimap",
                          L2Rchain=list(
                              list(
                                  tablename="tax", 
                                  Lcolname="tax_id",
                                  Rcolname="tax_name"                            
                              )
                          )                            
                     )
                   )), 
           CROSS = return(list( 
                     list(
                          objName="SEQ",
                          Class="AnnDbBimap",
                          L2Rchain=list(
                              list(
                                  tablename="seq",
                                  Lcolname="protein_id",
                                  Rcolname="seq"
                              )
                          )
                     ),             
                     list(
                         objName="MATCH",
                         Class="AnnDbBimap",
                         L2Rchain=list(
                             list(
                                 tablename="match", 
                                 Lcolname="query",
                                 Rcolname="subject"                            
                             )
                         )                            
                     )
                   ))
    )
}

createAnnObjs <- function(type, prefix, datacache, dbconn)
{   
    seeds <- createSeeds(type)
    ## AnnDbBimap objects
    seed0 <- list(
        datacache=datacache
    )
    ann_objs <- AnnotationDbi:::createAnnDbBimaps(seeds, seed0)        
    ann_objs <- switch(type,  
      "SP" = createAnnObjs.SP(ann_objs),
      "IPI" = createAnnObjs.IPI(ann_objs),
      "REFSEQ" = createAnnObjs.REFSEQ(ann_objs),
      "BACELLO" = createAnnObjs.BACELLO(ann_objs),
      "DBSUBLOC" = createAnnObjs.DBSUBLOC(ann_objs),
      "GOA" = createAnnObjs.GOA(ann_objs),
      "HOMOLOGENE" = createAnnObjs.HOMOLOGENE(ann_objs),
      "SYSPTM" = createAnnObjs.SYSPTM(ann_objs),
      "SYSBODYFLUID" = createAnnObjs.SYSBODYFLUID(ann_objs),
      "INPARANOID" = createAnnObjs.INPARANOID(ann_objs),
      ann_objs
    )
    if( !dbExistsTable(dbconn,"map_counts") ){
      tmp <- sapply(ls(ann_objs), function(x){count.mappedkeys(base:::get(x,envir=ann_objs))} )  
      tmp <- data.frame(names(tmp),tmp)
      colnames(tmp) <- c("map_name","count")
      dbWriteTable(dbconn, "map_counts", tmp, row.names=F)
    }
    ann_objs$MAPCOUNTS <- createMAPCOUNTS(dbconn, prefix)
    AnnotationDbi:::prefixAnnObjNames(ann_objs, prefix)    
}

createAnnObjs.SP <- function(ann_objs){ 
 ann_objs$GO2SPID <- revmap(ann_objs$GO, objName="GO2SPID")
 ann_objs$PATH2SPID <- revmap(ann_objs$PATH, objName="PATH2SPID")
 ann_objs$REFSEQ2SPID <- revmap(ann_objs$REFSEQ, objName="REFSEQ2SPID")
 ann_objs
}

createAnnObjs.REFSEQ <- function(ann_objs){ 
 ann_objs$GO2GI <- revmap(ann_objs$GO, objName="GO2GI")
 ann_objs$PATH2GI <- revmap(ann_objs$PATH, objName="PATH2GI")
 ann_objs$REFSEQ2GI <- revmap(ann_objs$REFSEQ, objName="REFSEQ2GI")
 ann_objs
}

createAnnObjs.IPI <- function(ann_objs){ 
 ann_objs$GO2IPIID <- revmap(ann_objs$GO, objName="GO2IPIID")
 ann_objs$PATH2IPIID <- revmap(ann_objs$PATH, objName="PATH2IPIID")
 ann_objs$IPIAC2IPIID <- revmap(ann_objs$IPIACs, objName="IPIAC2IPIID")
 ann_objs$SPID2IPIID <- revmap(ann_objs$SPID, objName="SPID2IPIID")
 ann_objs$SPAC2IPIID <- revmap(ann_objs$SPAC, objName="SPAC2IPIID")
 ann_objs$REFSEQ2IPIID <- revmap(ann_objs$REFSEQ, objName="REFSEQ2IPIID")
 ann_objs
}

createAnnObjs.BACELLO <- function(ann_objs){ 
 ann_objs$SUBCELL2SPID <- revmap(ann_objs$SUBCELL, objName="SUBCELL2SPID")
 ann_objs
}

createAnnObjs.DBSUBLOC <- function(ann_objs){ 
 ann_objs$SUBCELL2SPAC <- revmap(ann_objs$SUBCELL, objName="SUBCELL2SPAC")
 ann_objs
}

createAnnObjs.GOA <- function(ann_objs){ 
 ann_objs$GO2SPAC <- revmap(ann_objs$GO, objName="GO2SPAC")
 ann_objs$SPID2SPAC <- revmap(ann_objs$SPID, objName="SPID2SPAC")
 ann_objs
}

createAnnObjs.SYSBODYFLUID <- function(ann_objs){ 
 ann_objs$BF2PMID <- revmap(ann_objs$PMID2BF, objName="BF2PMID")
 ann_objs
}

createAnnObjs.HOMOLOGENE <- function(ann_objs){ 
 ann_objs$HOMOLOG2GENEID <- revmap(ann_objs$HOMOLOG, objName="HOMOLOG2GENEID")
 ann_objs
}

createAnnObjs.INPARANOID <- function(ann_objs){ 
 ann_objs$ORTHOLOG2PROTEIN <- revmap(ann_objs$ORTHOLOG, objName="ORTHOLOG2PROTEIN")
 ann_objs
}

createAnnObjs.SYSPTM <- function(ann_objs){ 
 ann_objs$PTMTYPE2SITE <- revmap(ann_objs$PTMTYPE, objName="PTMTYPE2SITE")
 ann_objs
}
