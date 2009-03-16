--
-- SP_DB schema
-- ===============
--

CREATE TABLE basic (
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  sp_ac VARCHAR(20) NOT NULL,               -- Swiss-Prot Primary Acession Number  
  len INTEGER NOT NULL,                     --Protein length
  mw INTEGER NOT NULL,
  symbol VARCHAR(80) NOT NULL,              -- gene symbol 
  unigene_id VARCHAR(10) NOT NULL,              -- UniGene ID
  functions VARCHAR(255) NOT NULL, 
  subcelluar VARCHAR(255) NOT NULL,
  tissue VARCHAR(255) NOT NULL,
  disease VARCHAR(255) NOT NULL,
  FOREIGN KEY (sp_id) 
);

CREATE TABLE seq (
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  seq text NOT NULL,               -- Protein Sequence
  FOREIGN KEY (ipi_id) 
);

CREATE TABLE spac (
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  sp_acs VARCHAR(20) NOT NULL,               -- Swiss-Prot Acession Number  
  FOREIGN KEY (sp_id)
);
CREATE TABLE de (
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  de VARCHAR(255) NOT NULL,                 -- Protein Description
  FOREIGN KEY (sp_id)
};
CREATE TABLE pubmed (
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  pubmed_id VARCHAR(10) NOT NULL,           -- PubMed ID
  FOREIGN KEY (sp_id)
);
CREATE TABLE alias (
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  alias_symbol VARCHAR(80) NOT NULL,        -- gene symbol or alias
  FOREIGN KEY (sp_id) 
}
CREATE TABLE geneid (
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  gene_id VARCHAR(10) NOT NULL        -- Entrez Gene ID
  FOREIGN KEY (sp_id) 
);
CREATE TABLE kegg (
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  kegg_id VARCHAR(20) NOT NULL,             -- KEGG gene ID
  FOREIGN KEY (sp_id) 
}
CREATE TABLE path (
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  path_id CHAR(5) NOT NULL,                     -- KEGG pathway short ID
  FOREIGN KEY (sp_id) 
};
CREATE TABLE refseq (
  sp_id VARCHAR(20) NOT NULL,                    -- Swiss-Prot Protein Identifier
  ref_id VARCHAR(20) NOT NULL,               -- RefSeq accession number
  FOREIGN KEY (sp_id) 
);
CREATE TABLE go (
  sp_id VARCHAR(20) NOT NULL,                     -- Swiss-Prot Protein Identifier
  go_id CHAR(10) NOT NULL,                      -- GO ID
  evidence CHAR(3) NOT NULL,                    -- GO evidence code
  ontology CHAR(2) NOT NULL,                    -- GO ontology
  FOREIGN KEY (sp_id) 
);
CREATE TABLE pfam (
  sp_id VARCHAR(20) NOT NULL,                    -- Swiss-Prot Protein Identifier
  pfam_id CHAR(7) NULL,                          -- Pfam ID
  FOREIGN KEY (sp_id) 
);
CREATE TABLE interpro (
  sp_id VARCHAR(20) NOT NULL,                     -- Swiss-Prot Protein Identifier
  interpro_id CHAR(9) NOT NULL,                   -- InterPro ID  
  FOREIGN KEY (sp_id) 
);  
CREATE TABLE prosite (
  sp_id VARCHAR(20) NOT NULL,                     -- Swiss-Prot Protein Identifier
  prosite_id CHAR(7) NULL,                        -- PROSITE ID
  FOREIGN KEY (sp_id) 
);
CREATE TABLE pdb (
  sp_id VARCHAR(20) NOT NULL,                 -- Swiss-Prot Protein Identifier
  pdb_id CHAR(7) NULL,                        -- PDB ID
  FOREIGN KEY (sp_id) 
);
CREATE TABLE ptm (
  sp_id VARCHAR(20) NOT NULL,                   -- Swiss-Prot Protein Identifier
  ptm_mod VARCHAR(20) NOT NULL,                 -- MOD_RES", "LIPID", "CARBOHYD", "DISULFID" or "CROSSLNK" 
  ptm_start INTEGER NOT NULL,                   -- start position of Posttransliational modification (PTM)
  ptm_end INTEGER NOT NULL,                     -- end position of PTM
  ptm_de VARCHAR(255) NOT NULL,                 -- description
  ptm_evidence VARCHAR(20) NOT NULL,            -- evidence("Experimental", "By similarity", "Potential" or "Probable")
  FOREIGN KEY (sp_id) 
);

CREATE TABLE int {
  sp_id VARCHAR(20) NOT NULL,                 -- Swiss-Prot Protein Identifier
  sp_ac_b VARCHAR(20) NOT NULL,                 -- Swiss-Prot Protein Identifier
};

CREATE INDEX Fbasic ON basic (sp_id);
