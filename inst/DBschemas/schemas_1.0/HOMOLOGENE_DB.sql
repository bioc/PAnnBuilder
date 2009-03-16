--
-- HOMOLOGENE_DB schema
-- ===============
--

CREATE TABLE homolog {
  homolog_id VARCHAR(10) NOT NULL       -- Identifier of homolog group
  tax_id VARCHAR(10) NOT NULL       -- Tax ID 
  gene_id VARCHAR(10) NOT NULL       -- Entrez Gene ID  
  symbol VARCHAR(80) NOT NULL,              -- gene symbol 
  gi VARCHAR(10) NOT NULL,               -- NCBI Protein GI
  ncbi_ac VARCHAR(20) NOT NULL,               -- NCBI protein accession number  
};

CREATE TABLE tax {
  tax_id VARCHAR(10) NOT NULL       -- Tax ID 
  organism VARCHAR(80) NOT NULL,        -- organism
};
