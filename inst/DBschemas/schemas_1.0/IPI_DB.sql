--
-- IPI_DB schema
-- ===============
--

CREATE TABLE basic (
  ipi_id CHAR(13) NOT NULL,               -- IPI Protein Identifier
  ipi_ac VARCHAR(20) NOT NULL,               -- IPI Acession Number in current version
  len INTEGER NOT NULL,
  mw INTEGER NOT NULL,
  de VARCHAR(255) NOT NULL,                 -- Protein Description
  symbol VARCHAR(80) NOT NULL,   
  sp_ac VARCHAR(20) NOT NULL,               -- Swiss-Prot Primary Acession Number  
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  ref_id VARCHAR(20) NOT NULL,               -- RefSeq accession number
  gi VARCHAR(10) NOT NULL,               -- NCBI Protein GI
  gene_id VARCHAR(10) NOT NULL       -- Entrez Gene ID
  unigene_id VARCHAR(10) NOT NULL,              -- UniGene ID
  kegg_id VARCHAR(20) NOT NULL,             -- KEGG gene ID  
  FOREIGN KEY (ipi_id) 
);

CREATE TABLE seq (
  ipi_id CHAR(13) NOT NULL,               -- IPI Protein Identifier
  seq text NOT NULL,               -- Protein Sequence
  FOREIGN KEY (ipi_id) 
);

CREATE TABLE ipiac (
  ipi_id CHAR(13) NOT NULL,               -- IPI Protein Identifier
  ipi_acs CHAR(11) NOT NULL,               -- IPI Protein Acession Number
  FOREIGN KEY (ipi_id)
);
CREATE TABLE go (
  ipi_id CHAR(13) NOT NULL,               -- IPI Protein Identifier
  go_id CHAR(10) NOT NULL,                      -- GO ID
  evidence CHAR(3) NOT NULL,                    -- GO evidence code
  ontology CHAR(2) NOT NULL,                    -- GO ontology
  FOREIGN KEY (ipi_id) 
);
CREATE TABLE path (
  ipi_id CHAR(13) NOT NULL,               -- IPI Protein Identifier
  path_id CHAR(5) NOT NULL,                     -- KEGG pathway short ID
  FOREIGN KEY (ipi_id) 
};
CREATE TABLE pfam (
  ipi_id CHAR(13) NOT NULL,               -- IPI Protein Identifier
  pfam_id CHAR(7) NULL,                          -- Pfam ID
  FOREIGN KEY (ipi_id) 
);
CREATE TABLE interpro (
  ipi_id CHAR(13) NOT NULL,               -- IPI Protein Identifier
  interpro_id CHAR(9) NOT NULL,                   -- InterPro ID  
  FOREIGN KEY (ipi_id) 
);  
CREATE TABLE prosite (
  ipi_id CHAR(13) NOT NULL,               -- IPI Protein Identifier
  prosite_id CHAR(7) NULL,                        -- PROSITE ID
  FOREIGN KEY (ipi_id) 
);  

CREATE INDEX Fbasic ON basic (ipi_id);
CREATE INDEX Fseq ON seq (ipi_id);
