createEmptyDPkg <- function(pkgName, pkgPath,
                            folders = c("man", "R", "data"), force = TRUE){
    if(file.exists(file.path(pkgPath, pkgName))){
        if(!force){
            stop(paste("Package", pkgName, "already exists"))
        }else{
            unlink(file.path(pkgPath, pkgName), TRUE)
        }
    }
    dir.create(file.path(pkgPath, pkgName))
    for(i in folders){
        dir.create(file.path(pkgPath, pkgName, i), recursive=T)
    }
}

writeMeta_DB <- function(db, repList){
  dbSendQuery(db, 
   "CREATE TABLE metadata (name VARCHAR(80) PRIMARY KEY, value VARCHAR(255) );")
  for ( i in names(repList) ) {
    if( repList[[i]]!="" ){
      metadata_sql <- paste("INSERT INTO metadata VALUES ('", i, "', '",
				    repList[i], "');", sep="", collapse="")
      dbSendQuery(db, metadata_sql)
    }
  }
}

writeData_DB <- function(type, srcUrls, db, organism=""){
    type = toupper(type)
    switch(type,
           "SP" = return(writeSPData_DB(srcUrls, db, organism)),
           "IPI" = return(writeIPIData_DB(srcUrls, db, organism)),
           "REFSEQ" = return(writeREFSEQData_DB(srcUrls, db, organism)),
           "GENEINT" = return(writeGENEINTData_DB(srcUrls, db)),
           "INTACT" = return(writeINTACTData_DB(srcUrls, db)),
           "MPPI" = return(writeMPPIData_DB(srcUrls, db)),   
           "3DID" = return(write3DIDData_DB(srcUrls, db)),
           "DOMINE" = return(writeDOMINEData_DB(srcUrls, db)),
           "SYSBODYFLUID" = return(writeSYSBODYFLUIDData_DB(srcUrls, db)),
           "SYSPTM" = return(writeSYSPTMData_DB(srcUrls, db)),
           "SCOP" = return(writeSCOPData_DB(srcUrls, db)),
           "BACELLO" = return(writeBACELLOData_DB(srcUrls, db)),
           "DBSUBLOC" = return(writeDBSUBLOCData_DB(srcUrls, db)),
           "GOA" = return(writeGOAData_DB(srcUrls, db)),
           "HOMOLOGENE" = return(writeHomoloGeneData_DB(srcUrls, db)),
           "INPARANOID" = return(writeInParanoidData_DB(srcUrls, db)),
           "PEPTIDEATLAS" = return(writePeptideAtlasData_DB(srcUrls, db)),
           stop(paste("type", type, "is not supported.", "Has to be sp, trembl, 
           ipi, refseq, go, goa, geneint, intact, mppi, did, domain, dbsubloc, 
           bacello, interpro, pfam, scop, homologene, inparanoid, peptideatlas,  
           sysptm, sysbodyfulid or ALL"))
    )
}

writeSPData_DB <- function(srcUrls, db, organism){
    tmpFile <- loadFromUrl(srcUrls)
    outFiles <- fileMuncher_DB(tmpFile, parser=getBaseParsers("sp",db=T), 
        organism=organism)    
    for (i in names(outFiles) ){ 
      data <- as.matrix(read.csv(outFiles[i],sep="\t",header=T))
      data <- apply(data,2,function(y){y[y==""]=NA; y=gsub("^\\s+","",y); y})
      dbWriteTable(db, i, data.frame(data), row.names=F)
    }
    unlink(tmpFile)
    unlink(outFiles)
}

writeIPIData_DB <- function(srcUrls, db, organism){
    tmpFile <- loadFromUrl(srcUrls)
    outFiles <- fileMuncher_DB(tmpFile, parser=getBaseParsers("ipi",db=T), 
        organism=organism)    
    for (i in names(outFiles) ){
      data <- as.matrix(read.csv(outFiles[i],sep="\t",header=T))
      data <- apply(data,2,function(y){y[y==""]=NA; y=gsub("^\\s+","",y); y})      
      dbWriteTable(db, i, data.frame(data), row.names=F)
    }
    unlink(tmpFile)
    unlink(outFiles)
}

writeREFSEQData_DB <- function(srcUrls, db, organism){
    tmpFile <- loadFromUrl(srcUrls)
    outFiles <- fileMuncher_DB(tmpFile, parser=getBaseParsers("refseq",db=T), 
        organism=organism)    
    for (i in names(outFiles) ){ 
      data <- as.matrix(read.csv(outFiles[i],sep="\t",header=T))
      data <- apply(data,2,function(y){y[y==""]=NA; y=gsub("^\\s+","",y); y})
      dbWriteTable(db, i, data.frame(data), row.names=F)
    }
    unlink(tmpFile)
    unlink(outFiles)
}

writeSYSBODYFLUIDData_DB <- function(srcUrls, db){
    tmpFile1 <- loadFromUrl(srcUrls["bf"])    
    data <- read.csv(tmpFile1,header=T,sep="\t")[,1:3]
    colnames(data) <- c("ipi_id","pubmed_id","body_fluid")
    unlink(tmpFile1)    
    dbWriteTable(db, "sysbodyfluid", data, row.names=F)      
    tmpFile2 <- loadFromUrl(srcUrls["paper"])
    data <- read.csv(tmpFile2,header=T,sep="\t")
    colnames(data) <- c("pubmed_id","title","platform","body_fluid","search_engine","sample")
    unlink(tmpFile2)    
    dbWriteTable(db, "paper", data, row.names=F)
}

writeSYSPTMData_DB <- function(srcUrls, db) {
    tmpFile1 <- loadFromUrl(srcUrls["ptm"])
    data1 <- read.csv(tmpFile1,header=T,sep="\t")
    data1 <- t(apply(data1,1,function(x){y=paste(x[1:3],collapse=":");c(y,x[-(1:3)]) }))
    colnames(data1) <- c("ptm_site","ptm_type","ptm_aa", "source","paper_number")
    dbWriteTable(db, "sysptm", data.frame(data1), row.names=F)
    unlink(tmpFile1)
    tmpFile2 <- loadFromUrl(srcUrls["xref"])
    data2 <- read.csv(tmpFile2,header=T,sep="\t")
    colnames(data2) <- c("sysptm_id","protein_id","database")
    dbWriteTable(db, "xref", data2, row.names=F)
    unlink(tmpFile2)
    tmpFile3 <- loadFromUrl(srcUrls["basic"])
    data3 <- read.csv(tmpFile3,header=T,sep="\t")
    colnames(data3) <- c("sysptm_id","de","alias_symbol","organism","seq", "length",
      "pdb","gene_id")
    dbWriteTable(db, "basic", data3[,c("sysptm_id","organism","seq","gene_id")], row.names=F)
    tmp <- lapply(as.vector(data3[,"de"]),function(x){unlist(strsplit(x,split=" ; "))})
    tmp <- cbind(rep(as.vector(data3[,"sysptm_id"]),sapply(tmp,length)),unlist(tmp))
    colnames(tmp) <- c("sysptm_id","de")
    dbWriteTable(db, "de", data.frame(tmp), row.names=F)
    tmp <- lapply(as.vector(data3[,"alias_symbol"]),function(x){unlist(strsplit(x,split=" ; "))})
    tmp <- cbind(rep(as.vector(data3[,"sysptm_id"]),sapply(tmp,length)),unlist(tmp))
    colnames(tmp) <- c("sysptm_id","alias_symbol")
    dbWriteTable(db, "alias", data.frame(tmp), row.names=F) 
    unlink(tmpFile3)
}

writeSCOPData_DB <- function(srcUrls, db) {
    tmpFile1 <- loadFromUrl(srcUrls["des"])
    tmpFile2 <- loadFromUrl(srcUrls["hie"])
    tmpFile3 <- loadFromUrl(srcUrls["cla"])
    data1 <- read.csv(tmpFile1, header=F, sep="\t",skip=4)
    colnames(data1) <- c("scop_id","type","class","name", "de")
    data2 <- as.matrix(read.csv(tmpFile2, header=F, sep="\t",skip=4))
    colnames(data2) <- c("scop_id","parent_id","children_id")
    tmp <- sapply(data2[,3],function(x){unlist(strsplit(x,split=","))})
    tmp2 <- sapply(tmp,length)
    chi <- data.frame(rep(data2[,1],tmp2), unlist(tmp))
    colnames(chi)<- c("scop_id","children_id")
    data3 <- as.matrix(read.csv(tmpFile3, header=F, sep="\t",skip=4))
    pdb <- data.frame(t(apply(data3,1,function(x){z=sapply(strsplit(x[6],split=","),
      function(y){sub("\\w+=","",y)}); c(x[2:3],z) })))
    colnames(pdb) <- c("pdb_id","pdb_chain","cl","cf","sf","fa","dm","sp","px")
    
    dbWriteTable(db, "des", data1, row.names=F)
    dbWriteTable(db, "parent", data.frame(data2[,1:2]), row.names=F) 
    dbWriteTable(db, "children", chi, row.names=F)
    dbWriteTable(db, "pdb", pdb, row.names=F)
}

writeGENEINTData_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    data <- as.matrix(read.csv(tmpFile,header=T,sep="\t"))
    data <- apply(data,2,function(y){y[y==""]=NA; y=gsub("^\\s+","",y); y})
    colnames(data) <- c("A_ORGANISM", "A_GENEID", "A_AC", "A_DE", "Int_PHRASE", 
     "B_ORGANISM", "B_ID", "B_IDTYPE", "B_AC", "B_DE", "Complex_ID", 
     "Complex_IDTYPE", "Complex_DE", "PMID", "LASTMOD","GeneRIF", "Int_ID", 
     "Int_IDTYPE")
    unlink(tmpFile)
    
    int <- data[data[,"B_IDTYPE"]=="GeneID",]    
    int <- unique(rbind(as.matrix(int[,c("A_GENEID","B_ID")]),as.matrix(int[,c("B_ID","A_GENEID")])))
    rownames(int)=NULL
    colnames(int)=c("gene_id_a","gene_id_b")
    dbWriteTable(db, "geneint", data.frame(int), row.names=F)     
    
    tmp <- data[data[,"B_IDTYPE"]=="GeneID",]
    tax <- unique(rbind(as.matrix(tmp[,c("A_GENEID","A_ORGANISM")]),as.matrix(tmp[,c("B_ID","B_ORGANISM")])))
    rownames(tax)=NULL
    colnames(tax)=c("gene_id","tax_id")
    dbWriteTable(db, "tax", data.frame(tax), row.names=F)          
}

writeINTACTData_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    data <- read.csv(tmpFile,header=T,sep="\t")    
    data <- data[,-ncol(data)]
    colnames(data) <- c("A_UPAC", "B_UPAC", "A_GN",  "B_GN", "A_ALIAS", 
     "B_ALIAS", "INT_METHOD", "AUTHOR1", "PMID", "A_ORGANISM", "B_ORGANISM", 
     "Int_TYPE", "SOURCE", "Int_ID", "CONFIDENCE", "A_ExROLE", "B_ExROLE", 
     "A_BioROLE", "B_BioROLE", "A_PROPERTY", "B_PROPERTY", "A_TYPE", "B_TYPE", 
     "HOST_ORGANISM", "Ex_METHOD", "DATASET")
    unlink(tmpFile)
            
    tmpData <- data[ intersect(grep("uniprotkb:",data[,"A_UPAC"]), 
     grep("uniprotkb:",data[,"B_UPAC"]) )  , c("A_UPAC", "A_ORGANISM", "B_UPAC",
     "B_ORGANISM")]
    tmpData[,"A_UPAC"] <- sub("\\|.*","",
     sub("uniprotkb:","",tmpData[,"A_UPAC"])) ;
    tmpData[,"B_UPAC"] <- sub("\\|.*","",
     sub("uniprotkb:","",tmpData[,"B_UPAC"])) ;
    tmpData[,"A_ORGANISM"] <- sub("\\(.*","",
     sub("taxid:","",tmpData[,"A_ORGANISM"])) ;
    tmpData[,"B_ORGANISM"] <- sub("\\(.*","",
     sub("taxid:","",tmpData[,"B_ORGANISM"])) ;
                 
    int <- unique(rbind(as.matrix(tmpData[,c("A_UPAC","B_UPAC")]),as.matrix(tmpData[,c("B_UPAC","A_UPAC")])))
    rownames(int)=NULL
    colnames(int)=c("up_ac_a","up_ac_b")
    dbWriteTable(db, "intact", data.frame(int), row.names=F)  
    tax <- unique(rbind(as.matrix(tmpData[,c("A_UPAC","A_ORGANISM")]),as.matrix(tmpData[,c("B_UPAC","A_ORGANISM")])))
    rownames(tax)=NULL
    colnames(tax)=c("up_ac","tax_id")
    dbWriteTable(db, "tax", data.frame(tax), row.names=F)  
}

writeMPPIData_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    outName <- tempfile("tempOut")
    data <- unique(as.matrix(read.csv(fileMuncher(outName, tmpFile, parser=
     getBaseParsers("mppi"), organism=""), header=T, sep = "\t" )))
    unlink(tmpFile)
    unlink(outName)

    mppi <- matrix(ncol=2)
    for(i in 1:nrow(data) ){
        x <- data[i,]
        if( x["proteinParticipant"]!="" ){
        proteins <- sub(".*:","",names(PAnnBuilder:::twoStepSplit(x["proteinParticipant"])))
        proteins <- proteins[proteins!=""]
        if(length(proteins)==2){
          mppi<- rbind(mppi,proteins)
          mppi<- rbind(mppi,proteins[2:1])
        }
        }
    }
    mppi <- unique(mppi[-1,])
    rownames(mppi)=NULL
    colnames(mppi)=c("up_ac_a","up_ac_b")
    dbWriteTable(db, "mppi", data.frame(mppi), row.names=F)  
}

write3DIDData_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    data <- readLines(tmpFile)
    unlink(tmpFile)    
    data <- data[grep("^#=ID",data)]
    did <- t(sapply(data,function(x){ 
      gsub("@Pfam","",unlist(strsplit(sub("\\).*","",sub(".*\\(","",x) ), split="\t"))) 
    }))
    did <- unique(rbind( did, did[,2:1] ))
    rownames(did)=NULL
    colnames(did)=c("pfam_a","pfam_b")
    dbWriteTable(db, "did", data.frame(did), row.names=F)   
}

writeDOMINEData_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    data <- as.matrix(read.csv(paste(dirname(tmpFile),"INTERACTION.txt",sep="/"),
     header=F,sep="|") )
    colnames(data) <- c("A_PFAM", "B_PFAM", "iPfam", "3did", "ME", "RCDP", 
     "Pvalue", "Fusion", "NetOpt", "RDFF", "PP", "PredictionConfidence", 
     "SameGO")
    unlink(tmpFile)
    
    name <- colnames(data)[3:11]
    de <- c(
     "Interaction observed in PDB crystal structure(s), as inferred by iPfam", 
     "Interaction observed in PDB crystal structure(s), as inferred by 3did", 
     "Interaction predicted by Lee et al.'s integrated approach", 
     "Interaction predicted by Jothi et al.'s using sequence co-evolution", 
     "Interaction predicted by Nye et al.'s statistical approach", 
     "Interaction predicted using domain fusion hypothesis, as inferred by Interdom",
     "Interaction predicted by either Guimaraes et al.'s parsimony approach or 
      Riley et al.'s domain pair exclusion analysis", 
     "Interaction predicted by Chen and Liu's random forest algorithm", 
     "Interaction predicted by Pagel et al.'s phylogenetic profiling approach")    
    dbWriteTable(db, "method", data.frame(method=name,method_de=de), row.names=F) 
    
    ddi <- matrix(ncol=3)
    for(i in 1:nrow(data)){ 
        x <- data[i,]        
        ddi <- rbind(ddi, rbind(t(sapply(name[x[3:11]=="1"],function(y){c(x[1:2],y)})), 
        t(sapply(name[x[3:11]=="1"],function(y){c(x[2:1],y)}))  ) )
    }
    ddi <- unique(ddi[-1,])
    rownames(ddi)=NULL
    colnames(ddi) <- c("pfam_a","pfam_b","method")
    dbWriteTable(db, "domine", data.frame(ddi), row.names=F) 
}


writeBACELLOData_DB <- function(srcUrls, db){
    subcell <- matrix(ncol=2)
    sequence <- matrix(ncol=2)
    for(x in names(srcUrls)){
        tmpFile <- loadFromUrl(srcUrls[x])
        files <- dir(paste(dirname(tmpFile),x,sep="/"),full.names=T)
        for(y in files){
            data <-  readLines(y)            
            tag <- grep("^>",data)
            protein <- sub("\\s+","",sub("^>","",data[tag]))
            sequence <- rbind(sequence, cbind( protein, data[-tag]))
            subcell <- rbind(subcell, cbind(protein,basename(y)) )
            unlink(y)
        }
        unlink(tmpFile)
    }
   subcell <- unique(subcell[-1,])
   sequence <- unique(sequence[-1,])
   colnames(subcell) <- c("sp_id","subcell")
   rownames(subcell)=NULL
   colnames(sequence) <- c("sp_id","seq")
   rownames(sequence)=NULL
   dbWriteTable(db, "bacello", data.frame(subcell), row.names=F)  
   dbWriteTable(db, "seq", data.frame(sequence), row.names=F)  
}

writeDBSUBLOCData_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    outName <- tempfile("tempOut")
    data <- unique(read.csv(fileMuncher(outName, tmpFile, parser=
     getBaseParsers("DBSubLoc"), organism=""), header=T, sep = "\t" ))    
    colnames(data) <- c("sp_ac","organism","de","subcell","seq")
    data <- data[data[,"subcell"]!="",]
    unlink(tmpFile)
    unlink(outName)
    
    dbWriteTable(db, "dbsubloc", data, row.names=F) 
}

writeGOAData_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    data <- read.csv(tmpFile,header=F,sep="\t")
    unlink(tmpFile)
     
    tmp <- unique(data[,c(2,5,7,9)])
    colnames(tmp) <- c("sp_ac","go_id","evidence","ontology")
    dbWriteTable(db, "go", tmp, row.names=F)
    tmp <- unique(data[,c(2,10)])
    colnames(tmp) <- c("sp_ac","de")
    dbWriteTable(db, "de", tmp, row.names=F)
    tmp <- unique(cbind(as.vector(data[,2]), sub(".*\\|","",data[,11])))
    colnames(tmp) <- c("sp_ac","sp_id")
    dbWriteTable(db, "id", data.frame(tmp), row.names=F)
}

writeHomoloGeneData_DB <- function( srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    data <- read.csv(tmpFile,header=F,sep="\t")
    unlink(tmpFile)
    
    colnames(data) <- c("homolog_id","tax_id","gene_id","symbol","gi","ncbi_ac")
    dbWriteTable(db, "homolog", data, row.names=F) 
    tax <- data.frame(t(sapply(unique(data[,2]),function(x){
      c(x,taxID2organism(as.character(x)))})))
    colnames(tax) <- c("tax_id","organism")
    dbWriteTable(db, "tax", tax, row.names=F) 
}

writeInParanoidData_DB <- function( srcUrls,db){
    organism <- names(srcUrls)[1:2]
    names(organism) <- basename(srcUrls[1:2])
    
    tmpFile <- loadFromUrl(srcUrls[1])
    data <- readLines(tmpFile)
    unlink(tmpFile)
    tag <- grep("^>",data)
    f1 <- sapply(1:(length(tag)-1), function(x){ 
     paste(data[(tag[x]+1):(tag[x+1]-1)], collapse="") })
    f1 <- c(f1, paste(data[(tag[length(tag)]+1):length(data)], collapse="") )    
    seq <- cbind(sub("\\s+","",sub("^>","",data[tag])),sub("\\*$","",f1))
    
    tmpFile <- loadFromUrl(srcUrls[2])
    data <- readLines(tmpFile)
    unlink(tmpFile)
    tag <- grep("^>",data)
    f2 <- sapply(1:(length(tag)-1), function(x){ 
     paste(data[(tag[x]+1):(tag[x+1]-1)], collapse="") })
    f2 <- c(f2, paste(data[(tag[length(tag)]+1):length(data)], collapse="") )
    seq <- rbind(seq,cbind(sub("\\s+","",sub("^>","",data[tag])),sub("\\*$","",f2)))
    rownames(seq)=seq[,1]
           
    tmpFile <- loadFromUrl(srcUrls["org1_org2"])
    data <- read.csv(tmpFile,header=F,sep="\t")
    unlink(tmpFile)
    ortholog <- data.frame(data[,1],organism[data[,3]],data[,5])
    colnames(ortholog) <- c("ortholog_id","organism","protein_id")
    seq <- data.frame(seq[as.character(ortholog[,3]),])
    colnames(seq) <- c("protein_id","seq")
    
    dbWriteTable(db, "ortholog", ortholog, row.names=F) 
    dbWriteTable(db, "seq", seq, row.names=F) 
}

writePeptideAtlasData_DB <- function(srcUrls,db){
    tmpFasta <- loadFromUrl(srcUrls["fasta"])
    tmpMap <- loadFromUrl(srcUrls["map"])
    fasta <- readLines(tmpFasta)
    map <- as.matrix(read.csv(tmpMap,header=F,sep="\t"))
    unlink(tmpFasta)
    unlink(tmpMap) 
    
    tag <- grep("^>",fasta)
    seq <- data.frame(cbind(sub("^>","",fasta[tag]),fasta[-tag]))
    colnames(seq) <- c("peptide_id", "seq")
  
    map <- unique(map[,c(1,3,6,7,9:14)])    
    colnames(map)=c("peptide_id", "protein_id", "start_protein", 
     "end_protein", "chromosome", "strand", "start_chr", "end_chr", 
     "ensembl_transcript", "ensembl_gene")
    pep2protein = apply(map,1,function(x){paste(x[2:4],collapse=":") })
    pep2protein = data.frame(map[,1],pep2protein)
    colnames(pep2protein) = c("peptide_id","protein_position")
    pep2chr = apply(map,1,function(x){ paste(x[5:8],collapse=":") })
    pep2chr = data.frame(map[,1],pep2chr)
    colnames(pep2chr) = c("peptide_id","chr_position")
    
    dbWriteTable(db, "seq", seq, row.names=F) 
    dbWriteTable(db, "pep2protein", pep2protein, row.names=F) 
    dbWriteTable(db, "pep2chr", pep2chr, row.names=F) 
}

writeName_DB <- function(type, srcUrls, db){
    type = toupper(type)
    switch(type,
           "GONAME" = return(writeGOName_DB(srcUrls, db)),
           "KEGGNAME" = return(writeKEGGName_DB(srcUrls, db)),
           "PFAMNAME" = return(writePFAMName_DB(srcUrls, db)),
           "INTERPRONAME" = return(writeINTERPROName_DB(srcUrls, db)),
           "TAXNAME" = return(writeTAXName_DB(srcUrls, db)),
           "PROSITENAME" = return(writePROSITEName_DB(srcUrls, db)),
           stop("Parameter src is not correct, must be GONAME, KEGGNAME, PFAMNAME, 
           INTERPRONAME, PROSITE or TAXNAME.") 
    )
}

writeGOName_DB <- function(srcUrls, db){
  # srcUrl is "http://www.geneontology.org/ontology/gene_ontology_edit.obo"
  tmpFile <- loadFromUrl(srcUrls)
  tmp <- readLines(tmpFile)
  unlink(tmpFile)
  data <- data.frame(gsub("^id: ","",tmp[grep("^id: ",tmp)]), 
    gsub("^name: ","",tmp[grep("^name: ",tmp)]))
  colnames(data) <- c("go_id","go_name")
  dbWriteTable(db, "go", data, row.names=F) 
}

writeKEGGName_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    data <- read.csv(tmpFile,header=F,sep="\t",colClasses="character")
    colnames(data) <- c("path_id","path_de")
    unlink(tmpFile)
    dbWriteTable(db, "kegg", data, row.names=F) 
}

writePFAMName_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)       
    outName <- tempfile("tempOut")
    data <- read.csv(fileMuncher(outName, tmpFile, parser=
     getBaseParsers("pfamname"), organism=""), header=T, sep = "\t" )    
    colnames(data) <- c("pfam_id","pfam_name","pfam_de")
    unlink(tmpFile)
    unlink(outName)  
    
    dbWriteTable(db, "pfam", data, row.names=F) 
}

writeINTERPROName_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    data <- read.csv(tmpFile,header=F,sep="\t",skip=1)
    colnames(data) <- c("interpro_id","interpro_name")
    unlink(tmpFile)
    dbWriteTable(db, "interpro", data, row.names=F) 
}

writePROSITEName_DB <- function(srcUrls, db){
    tmpFile <- loadFromUrl(srcUrls)
    outName <- tempfile("tempOut")
    data <- read.csv(fileMuncher(outName, tmpFile, parser=
     getBaseParsers("prositede"), organism=""), header=T, sep = "\t" )    
    colnames(data) <- c("prosite_id","prosite_de")
    unlink(tmpFile)
    unlink(outName)     
    dbWriteTable(db, "prosite", data, row.names=F) 
}

writeTAXName_DB <- function(srcUrls, db){
    tmpFile <- paste(dirname(loadFromUrl(srcUrls)), "names.dmp", sep="/")
    data <- as.matrix(read.csv( tmpFile, header=F,sep="|"))
    unlink(tmpFile)
    data <- data[data[,4]=="\tscientific name\t",1:2]
    data[,2] <- gsub("\t", "", data[,2])
    colnames(data) <- c("tax_id","tax_name")
    dbWriteTable(db, "tax", data.frame(data), row.names=F) 
}


