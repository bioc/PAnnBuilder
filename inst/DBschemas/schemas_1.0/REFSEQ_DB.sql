--
-- REFSEQ_DB schema
-- ===============
--

CREATE TABLE basic (
  gi VARCHAR(10) NOT NULL,               -- NCBI Protein GI
  ref_id VARCHAR(20) NOT NULL,               -- RefSeq accession number
  gene_id VARCHAR(10) NOT NULL       -- Entrez Gene ID
 symbol VARCHAR(80) NOT NULL,   
 de VARCHAR(255) NOT NULL,                 -- Protein Description
  kegg_id VARCHAR(20) NOT NULL,             -- KEGG gene ID
  FOREIGN KEY (gi) 
);
CREATE TABLE seq (
  gi VARCHAR(10) NOT NULL,               -- NCBI Protein GI
  seq text NOT NULL,               -- Protein Sequence
  FOREIGN KEY (ipi_id) 
);
CREATE TABLE alias (
  gi VARCHAR(10) NOT NULL,               -- NCBI Protein GI
  alias_symbol VARCHAR(80) NOT NULL,       -- gene symbol or alias
  FOREIGN KEY (gi) 
}
CREATE TABLE path (
  gi VARCHAR(10) NOT NULL,               -- NCBI Protein GI
  path_id CHAR(5) NOT NULL,                     -- KEGG pathway short ID
  FOREIGN KEY (gi) 
}
CREATE TABLE go (
  gi VARCHAR(10) NOT NULL,               -- NCBI Protein GI
  go_id CHAR(10) NOT NULL,                      -- GO ID
  evidence CHAR(3) NOT NULL,                    -- GO evidence code
  ontology CHAR(2) NOT NULL,                    -- GO ontology
  FOREIGN KEY (gi) 
);
