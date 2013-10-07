## Some functions are from Bioconductor "AnnBuilder" package, and modified by 
## Hong Li, 2008.

writeManAnno <- function (pkgName, pkgPath, version, author, repList, pattern) 
{
    writeDescription(pkgName, pkgPath, version, author)
    copyTemplates(repList, pattern, pkgName, pkgPath)
    copySubstitute(file.path(path.package("PAnnBuilder"), "templates", 
        "Anno.Rd"), file.path(pkgPath, pkgName, "man", paste(pkgName, 
        ".Rd", sep = "")), repList, "#")    
}  

writeManAnno_DB <- function (pkgName, pkgPath, version, author, repList)
{
    writeDescription_DB(pkgName, pkgPath, version, author, repList[["ORGANISM"]])
    repList[["PREFIX"]]= sub(".db$","",repList[["PKGNAME"]])
    repList[["TYPE"]]= sub("_DB$","",repList[["DBSCHEMA"]])       
    copyTemplates_DB(repList, pkgName, pkgPath) 
} 

writeManSQ <- function (pkgName, pkgPath, version, author, repList) 
{
  writeDescription(pkgName, pkgPath, version, author)
  copySubstitute(file.path(path.package("PAnnBuilder"), "templates", 
        "PKGNAMESQ.Rd"), file.path(pkgPath, pkgName, "man", paste(pkgName, 
        ".Rd", sep = "")), repList, "#")
        
}  

writeManMerge <- function (pkgName, pkgPath, version, author, repList, pattern) 
{
    writeDescription(pkgName, pkgPath, version, author)
    copyTemplates(repList, pattern, pkgName, pkgPath)
    copySubstitute(file.path(path.package("PAnnBuilder"), "templates", 
        "Merge.Rd"), file.path(pkgPath, pkgName, "man", paste(pkgName, 
        ".Rd", sep = "")), repList, "#") 
}  

writeDescription <- function(pkgName, pkgPath, version, author,
                             dataSrc = "public data repositories",
                             license = "The Artistic License, Version 2.0") {
  ## write descrption file for the package
  ## file name : "DESCRIPTION". file path : a subdirectory of pkgPath, called 
  ## pkgName
  ##
  ## author = list()
  ## author$authors = c("Hong Li <lihong@sibs.ac.cn>")
  ## author$maintainer = "Hong Li <lihong@sibs.ac.cn>"
  ## writeDescription("test","D:","1.0",author)
  ##
    path <- file.path(pkgPath, pkgName)
    fileName <- file.path(path, "DESCRIPTION")
    if(file.exists(fileName)){
        unlink(fileName)
    }
    rVersion <- paste(R.Version()[c("major", "minor")], collapse=".")
    ## read destriptionInfo.txt
    ## destriptionInfo.txt has the record about pkgName, 
    ## corresponding information like organism, species, biocViews will be wrote 
    ## to the descrption file
    data("descriptionInfo")
    descriptionInfo <- get("descriptionInfo")
    slots <- descriptionInfo[which(descriptionInfo[,"biocPkgName"]==pkgName),]
    
    write(paste("Package:", pkgName), file=fileName, append=FALSE)   
    write(paste("Title: A data package containing",
                "annotation data for", pkgName),
          file=fileName, append=TRUE)
    write(paste("Description: Annotation data file for",
                pkgName, "assembled using data\n   from",
                dataSrc),
          file=fileName, append=TRUE)
    write(paste("Version:", version,                
                "\nCreated:", date(),
                "\nAuthor:", paste(author[["authors"]], collapse=", "),
                "\nMaintainer:", author[["maintainer"]],
                "\nLazyData: yes",
                paste("\nDepends: R(>= ", rVersion, ")", sep=""),
                "\nLicense:", license),
          file=fileName, append=TRUE)
    ## nrow(slots)==0 means that there is no such pkgName in the descriptionInfo
    if(nrow(slots)!=0) {
        slots <- slots[,-which(colnames(slots)=="biocPkgName")]
        slots[is.na(slots)] <- ""
        slotNames <- names(slots)
        for(i in 1:length(slots)) {
            write(paste(slotNames[i], ": ", slots[1,i], sep=""),
            file=fileName, append=TRUE)
        }
    }  
}

writeDescription_DB <- function(pkgName, pkgPath, version, author, organism,
                             dataSrc = "public data repositories",
                             license = "The Artistic License, Version 2.0") {
  ## write descrption file for the package
  ## file name : "DESCRIPTION". file path : a subdirectory of pkgPath, called 
  ## pkgName
  ##
  ## author = list()
  ## author$authors = c("Hong Li <lihong@sibs.ac.cn>")
  ## author$maintainer = "Hong Li <lihong@sibs.ac.cn>"
  ## writeDescription("test","D:","1.0",author)
  ##
    path <- file.path(pkgPath, pkgName)
    fileName <- file.path(path, "DESCRIPTION")
    if(file.exists(fileName)){
        unlink(fileName)
    }
    rVersion <- paste(R.Version()[c("major", "minor")], collapse=".")
    ## read destriptionInfo.txt
    ## destriptionInfo.txt has the record about pkgName, 
    ## corresponding information like organism, species, biocViews will be wrote 
    ## to the descrption file
    data("descriptionInfo")
    descriptionInfo <- get("descriptionInfo")
    slots <- descriptionInfo[which(descriptionInfo[,"biocPkgName"]==pkgName),]
    
    write(paste("Package:", pkgName), file=fileName, append=FALSE)   
    write(paste("Title: A data package containing",
                "annotation data for", pkgName),
          file=fileName, append=TRUE)
    write(paste("Description: Annotation data file for",
                pkgName, "assembled using data\n   from",
                dataSrc),
          file=fileName, append=TRUE)
    
    if( organism=="" ){
      write("biocViews: AnnotationData, Proteomics", file=fileName, append=TRUE)
    }else{
      write(paste("biocViews: AnnotationData, Proteomics", organism, sep=", ")
        , file=fileName, append=TRUE)
    }    
    write(paste("Version:", version,
                "\nCreated:", date(),
                "\nAuthor:", paste(author[["authors"]], collapse=", "),
                "\nMaintainer:", author[["maintainer"]],
                "\nLazyData: yes",
                paste("\nDepends: R(>= ", rVersion, ")", sep=""),                
                ", methods, AnnotationDbi (>= 1.3.12), PAnnBuilder (>= 1.3.0)",
                "\nImports: methods, AnnotationDbi, PAnnBuilder",                
                "\nLicense:", license),
          file=fileName, append=TRUE)
    ## nrow(slots)==0 means that there is no such pkgName in the descriptionInfo
    if(nrow(slots)!=0) {
        slots <- slots[,-which(colnames(slots)=="biocPkgName")]
        slots[is.na(slots)] <- ""
        slotNames <- names(slots)
        for(i in 1:length(slots)) {
            write(paste(slotNames[i], ": ", slots[1,i], sep=""),
            file=fileName, append=TRUE)
        }
    }  
}


getRepList <- function(organism="", type, 
    srcUrls="", built="", pkgName
    ){
  ##  type - Possible values of type are : "sp", "trembl", "ipi", "refseq",
  ##  "geneint", "intact", "mppi", "3DID", "DOMINE", "BaCelLo", "DBSubLoc", 
  ##  "SCOP", "HomoloGene", "InParanoid", "PeptideAtlas", 
  ##  "SysPTM", "SysBodyFluid", "GOA", "GO", "KEGGNAME", "PFAMNAME", 
  ##  "INTERPRONAME", "TAXNAME", "dName", "cross", "pSeq" 
  type = toupper(type)
  switch(type,
         SP = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,                 
                     SOURCE = paste("UniProtKB/Swiss-Prot:",
                              paste("\\url{","}",sep=srcUrls), 
                              ". Built:", built) ,
                     PATH = paste("KEGGPATH:",paste("\\url{","}",sep=
                            paste(.srcUrls("KEGG"), 
                            paste(tolower(getShortSciName(organism)),
                            paste(tolower(getShortSciName(organism)),
                            "pathway.list",sep="_"),sep="/"), 
                            sep="/genes/organisms/"))) ,
                     KEGG = "",
                     GO = "",
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date() 
                     )),
         TREMBL = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste("UniProtKB/TrEMBL:",paste("\\url{","}",
                              sep=srcUrls), ". Built:", built) ,
                     PATH = paste("KEGGPATH:",paste("\\url{","}",sep=paste(
                            .srcUrls("KEGG"), paste(tolower(getShortSciName(
                            organism)),paste(tolower(getShortSciName(organism)),
                            "pathway.list",sep="_"),sep="/"), 
                            sep="/genes/organisms/"))) ,
                     KEGG = "",
                     GO = "",
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date() 
                     )),
          IPI = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste("IPI:",paste("\\url{","}",sep=srcUrls),
                              ". Built:") ,
                     PATH = paste("KEGGPATH:",paste("\\url{","}", 
                            sep=paste(.srcUrls("KEGG"), paste(tolower(
                            getShortSciName(organism)),paste(tolower(
                            getShortSciName(organism)),"pathway.list",sep="_"),
                            sep="/"), sep="/genes/organisms/"))) ,                         
                     KEGG = paste("KEGGID:",paste("\\url{","}",sep=paste(
                            .srcUrls("KEGG"), paste(tolower(getShortSciName(
                            organism)),paste(tolower(getShortSciName(organism)),
                            "ncbi-geneid.list",sep="_"),sep="/"), 
                            sep="/genes/organisms/")) ) ,
                     GO = paste("GO:",paste("\\url{","}",sep=paste(
                          .srcUrls("Gene"), "gene2go.gz", sep="/")) ) ,
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date()
                     )),
          REFSEQ = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste("RefSeq:",paste("\\url{","}",sep=srcUrls),
                              ". Built:",built) ,
                     PATH = paste("KEGGPATH:",paste("\\url{","}",
                            sep=paste(.srcUrls("KEGG"), paste(tolower(
                            getShortSciName(organism)),paste(tolower(
                            getShortSciName(organism)),"pathway.list",sep="_"),
                            sep="/"), sep="/genes/organisms/")) ) ,
                     KEGG = paste("KEGGID:",paste("\\url{","}",sep=paste(
                            .srcUrls("KEGG"), paste(tolower(getShortSciName(
                            organism)),paste(tolower(getShortSciName(organism)),
                            "ncbi-geneid.list",sep="_"),sep="/"), 
                            sep="/genes/organisms/")) ) ,
                     GO = paste("GO:",paste("\\url{","}",sep=paste(
                          .srcUrls("Gene"), "gene2go.gz", sep="/")) ) ,
                     Gene2refseq = paste("Gene2refseq:",paste("\\url{","}",sep=
                                   paste(.srcUrls("Gene"), "gene2refseq.gz", 
                                   sep="/")) ) ,
                     GeneInfo = paste("GeneInfo:",paste("\\url{","}",sep=
                                paste(.srcUrls("Gene"), "gene_info.gz", 
                                sep="/")) ) ,
                     DATE = date()
                     )),         
         GENEINT = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste("NCBI Gene Interaction:",paste("\\url{","}",
                              sep=srcUrls),". Built:",built) ,
                     PATH = "",
                     KEGG = "",
                     GO = "",
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date() 
                     )) ,    
         PEPTIDEATLAS = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste("PeptideAtlas:",paste(sapply(srcUrls,
                              function(x){paste("\\url{","}",sep=x)}),
                              collapse=" ; "),". Built:",built) ,
                     PATH = "",
                     KEGG = "",
                     GO = "",
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date() 
                     )) ,    
          BACELLO = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste("BaCelLo:",sapply(srcUrls, function(x){
                              paste("\\url{","}",sep=x)}),". Built:",built) ,
                     PATH = "",
                     KEGG = "",
                     GO = "",
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date() 
                     )) ,      
          DNAME = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste(sapply(names(srcUrls),function(x){
                              paste(x,":",paste("\\url{","}", sep=srcUrls[x]),
                              ". Built:",built[x])} ), collapse=" . ") ,
                     PATH = "",
                     KEGG = "",
                     GO = "",
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date() 
                     )) , 
          CROSS  = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste(sapply(names(srcUrls),function(x){
                              paste(x,":",paste("\\url{","}", sep=srcUrls[x]),
                              ". Built:",built[x])} ), collapse=" . ") ,
                     PATH = "",
                     KEGG = "",
                     GO = "",
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date() 
                     )) ,
          PSEQ = return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste("Databases:",sapply(srcUrls,function(x){
                              paste("\\url{","}",sep=x)}),". Built:",built) ,
                     PATH = "",
                     KEGG = "",
                     GO = "",
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date() 
                     )) , 
          return(list(TYPE = type,
                     PKGNAME = pkgName,
                     ORGANISM = organism,
                     SOURCE = paste(type, ":",paste("\\url{","}",sep=srcUrls),
                              ". Built:",built) ,
                     PATH = "",
                     KEGG = "",
                     GO = "",
                     Gene2refseq = "",
                     GeneInfo = "",
                     DATE = date() 
                     ))
          )
}

getRepList_DB <- function(organism="", type, 
    srcUrls="", built="", pkgName
){
  type = toupper(type)  
  repList = list()
  repList[["PKGNAME"]] = pkgName
  repList[["DBSCHEMAVERSION"]] = "1.0"
  repList[["DBSCHEMA"]] = paste(type,"DB",sep="_")  
  repList[["ORGANISM"]] = organism  
  tmp <- switch(type,
         SP	= list(SPSOURCENAME = "Swiss-Prot",
                   SPSOURCEDATE = built,
                   SPSOURCEURL = srcUrls,
                   KEGGSOURCENAME = "KEGG",
                   KEGGSOURCEDATE = "",
                   KEGGSOURCEURL = paste(.srcUrls("KEGG"), 
                            paste(tolower(PAnnBuilder:::getShortSciName(organism)),
                            paste(tolower(PAnnBuilder:::getShortSciName(organism)),
                            "pathway.list",sep="_"),sep="/"), 
                            sep="/genes/organisms/")
              ),                                     
         IPI = list(IPISOURCENAME = "IPI",
                    IPISOURCEDATE = built,
                    IPISOURCEURL = srcUrls,
                    KEGGSOURCENAME = "KEGG",
                    KEGGSOURCEDATE = "",
                    KEGGSOURCEURL = paste(paste(paste(.srcUrls("KEGG"),tolower(
                                    getShortSciName(organism)),tolower(
                                    getShortSciName(organism)),sep="/"),c(
                                    "pathway.list","ncbi-geneid.list"),sep="_"),
                                    collapse=" ; "),
                    GOSOURCENAME = "Gene Ontology",
                    GOSOURCEDATE = "",
                    GOSOURCEURL = paste(.srcUrls("Gene"), "gene2go.gz", sep="/")
               ),
         REFSEQ = list(REFSEQSOURCENAME = "NCBI RefSeq",
                       REFSEQSOURCEDATE = built,
                       REFSEQSOURCEURL = srcUrls,
                       KEGGSOURCENAME = "KEGG",
                       KEGGSOURCEDATE = "",
                       KEGGSOURCEURL = paste(paste(paste(.srcUrls("KEGG"),tolower(
                                       getShortSciName(organism)),tolower(
                                       getShortSciName(organism)),sep="/"),c(
                                       "pathway.list","ncbi-geneid.list"),sep="_"),
                                       collapse=" ; "),
                       GOSOURCENAME = "Gene Ontology",
                       GOSOURCEDATE = "",
                       GOSOURCEURL = paste(.srcUrls("Gene"), "gene2go.gz", sep="/"),
                       GENESOURCENAME = "NCBI Gene Information",
                       GENESOURCEDATE = "",
                       GENESOURCEURL = paste(paste(.srcUrls("Gene"), c("gene2refseq.gz",
                                       "gene_info.gz"),sep="/"),collapse=" ; ")
                  ),
         SYSPTM = list(SYSPTMSOURCENAME = "SysPTM",
                       SYSPTMSOURCEDATE = built,
                       SYSPTMSOURCEURL = paste(srcUrls,collapse=" ; ")
                  ),
         SCOP = list(SCOPSOURCENAME = "SCOP: Structural Classification of Proteins",
                     SCOPSOURCEDATE = built,
                     SCOPSOURCEURL = paste(srcUrls,collapse=" ; ")
                ),
         SYSBODYFLUID = list(BFSOURCENAME = "Human Body fluids in Sys-BodyFluid",
                             BFSOURCEDATE = built,
                             BFSOURCEURL = srcUrls["bf"],
                             PAPERSOURCENAME = "Papers collected in Sys-BodyFluid",
                             PAPERSOURCEDATE = built,
                             PAPERSOURCEURL = srcUrls["paper"]
                        ),
         GENEINT = list(GENEINTSOURCENAME = "Gene Interaction in NCBI",
                        GENEINTSOURCEDATE = built,
                        GENEINTSOURCEURL = srcUrls
                   ),
         INTACT = list(INTACTSOURCENAME = "IntAct database",
                       INTACTSOURCEDATE = built,
                       INTACTSOURCEURL = srcUrls
                  ), 
         MPPI = list(MPPISOURCENAME = "MIPS Mammalian Protein-Protein Interaction Database",
                     MPPISOURCEDATE = built,
                     MPPISOURCEURL = srcUrls
                ),               
         "3DID" = list(DIDSOURCENAME = "database of 3D Interacting Domains (3did)",
                       DIDSOURCEDATE = built,
                       DIDSOURCEURL = srcUrls
                  ),
         DOMINE = list(DOMINESOURCENAME = "DOMINE: known and predicted protein domain (domain-domain) interactions",
                       DOMINESOURCEDATE = built,
                       DOMINESOURCEURL = srcUrls
                  ),
         BACELLO = list(BACELLOSOURCENAME = "BaCelLo: subcellular localization of proteins in eukaryotes",
                        BACELLOSOURCEDATE = built,
                        BACELLOSOURCEURL = paste(srcUrls,collapse=" ; ")
                   ),     
         DBSUBLOC = list(DBSUBLOCSOURCENAME = "DBSubLoc",
                         DBSUBLOCSOURCEDATE = built,
                         DBSUBLOCSOURCEURL = srcUrls
                    ), 
         GOA = list(GOASOURCENAME = "Gene Ontology Annotation (GOA) Database",
                    GOASOURCEDATE = built,
                    GOASOURCEURL = srcUrls
               ), 
         HOMOLOGENE = list(HOMOLOGENESOURCENAME = "HomoloGene: homologs among eukaryotic gene sets",
                           HOMOLOGENESOURCEDATE = built,
                           HOMOLOGENESOURCEURL = srcUrls
                      ), 
         INPARANOID = list(INPARANOIDSOURCENAME = "Inparanoid eukaryotic ortholog database",
                           INPARANOIDSOURCEDATE = built,
                           INPARANOIDSOURCEURL = paste(srcUrls,collapse=" ; ")
                      ),
         PEPTIDEATLAS = list(PEPTIDEATLASSOURCENAME = "PeptideAtlas",
                             PEPTIDEATLASSOURCEDATE = built,
                             PEPTIDEATLASSOURCEURL = paste(srcUrls,collapse=" ; ")
                        ),
         DNAME = list(KEGGSOURCENAME = "KEGG pathway",
                      KEGGSOURCEDATE = built["KEGGNAME"],
                      KEGGSOURCEURL = srcUrls["KEGGNAME"],
                      GOSOURCENAME = "Gene Ontology term",
                      GOSOURCEDATE = built["GONAME"],
                      GOSOURCEURL = srcUrls["GONAME"],
                      PFAMSOURCENAME = "Pfam domain",
                      PFAMSOURCEDATE = built["PFAMNAME"],
                      PFAMSOURCEURL = srcUrls["PFAMNAME"],
                      PROSITESOURCENAME = "PROSITE",
                      PROSITESOURCEDATE = built["PROSITENAME"],
                      PROSITESOURCEURL = srcUrls["PROSITENAME"],
                      INTERPROSOURCENAME = "Interpro domain",
                      INTERPROSOURCEDATE = built["INTERPRONAME"],
                      INTERPROSOURCEURL = srcUrls["INTERPRONAME"],
                      TAXSOURCENAME = "NCBI Taxonomy",
                      TAXSOURCEDATE = built["TAXNAME"],                      
                      TAXSOURCEURL = srcUrls["TAXNAME"]
                 ),
         CROSS = list(CROSSSOURCENAME = "BaCelLo: subcellular localization of proteins in eukaryotes",
                      CROSSSOURCEDATE = paste(built,collapse=" ; "),
                      CROSSSOURCEURL = paste(srcUrls,collapse=" ; ")
                 ),
         PSEQ = list(PSEQSOURCENAME = "R annotation packages",
                     PSEQSOURCEDATE = built,
                     PSEQSOURCEURL = paste(srcUrls,collapse=" ; ")
                 ),
  )
  repList<-c(repList, tmp)
  repList
}


copyTemplates <- function (repList, pattern, pkgName, pkgPath, replaceBy = NULL) {
    ## If the name of "*.rda" file in the data subdirectory matchs the pattern, 
    ## a "*.rd" file in the man subdirectory will be produced
    ## template files in the "inst/templates" subdirectory is needed.
    ## eg: if the pkgName is "test", "data/testPATH.rda" and  "inst/templates/
    ## PKGNAMEPATH.Rd" is existed, then "man/testPATH.Rd" will be produced.

    templates <- gsub(pkgName, pattern, gsub("\\.rda$", ".Rd", 
        list.files(file.path(pkgPath, pkgName, "data"))))
    for (i in templates) {
        if (is.null(replaceBy)) {
            rdName <- file.path(pkgPath, pkgName, "man", gsub(pattern, 
                pkgName, i))
        }
        else {
            rdName <- file.path(pkgPath, pkgName, "man", gsub(pattern, 
                replaceBy, i))
        }
        options(show.error.messages = FALSE)
        ## copy template file to the rdName, and the place labed with "#" be 
        ## substituted with value in repList 
        copied <- try(copySubstitute(file.path(path.package("PAnnBuilder"), 
            "templates", i), rdName, repList, "#"))

        options(show.error.messages = TRUE)
        if (inherits(copied, "try-error")) {
            warning(paste("Can't copy", file.path(path.package("PAnnBuilder"), 
                "templates", i)))
        }
    }
}

copyTemplates_DB <- function (repList, pkgName, pkgPath) {
    tmpdir = file.path(path.package("PAnnBuilder"), 
            "templates", sub("_DB\\.sql","\\.DB",repList[["DBSCHEMA"]]) )
    templates <- dir(path=tmpdir,pattern=".Rd$")   
    prefix <- sub("\\.db","",pkgName)
    for (i in templates) {        
        if(i=="Anno.Rd"){
          rdName <- file.path(pkgPath, pkgName, "man", paste(pkgName,".Rd",sep="") )
        }else{
          rdName <- file.path(pkgPath, pkgName, "man", paste(prefix,i,sep="") )
        }
        options(show.error.messages = FALSE)
        ## copy template file to the rdName, and the place labed with "#" be 
        ## substituted with value in repList 
        copied <- try(copySubstitute(file.path(tmpdir,i), rdName, repList, "#"))

        options(show.error.messages = TRUE)
        if (inherits(copied, "try-error")) {
            warning(paste("Can't copy", file.path(tmpdir,i)))
        }
    }
    copySubstitute(file.path(tmpdir,"zzz.R"), 
      file.path(pkgPath, pkgName, "R", "zzz.R"), repList, "#")
    copySubstitute(file.path(tmpdir,"NAMESPACE"), 
      file.path(pkgPath, pkgName, "NAMESPACE"), repList, "#") 
}
