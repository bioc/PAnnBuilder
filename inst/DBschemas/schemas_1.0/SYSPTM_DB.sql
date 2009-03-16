--
-- SYSPTM_DB schema
-- ===============
--

CREATE TABLE sysptm (
  sysptm_id VARCHAR(10) NOT NULL,               -- SysPTM Protein Identifier
  ptm_start INTEGER NOT NULL,         -- Start position of PTM
  ptm_end INTEGER NOT NULL,         -- End position of PTM
  ptm_type VARCHAR(100) NOT NULL,   -- Type of PTM
  ptm_aa VARCHAR(255) NOT NULL,     -- Amino acids of PTM region  
  source VARCHAR(100) NOT NULL,   -- Data source of PTM
  paper_number INTEGER NOT NULL,         -- Number of MS/MS papers which identify this PTM in SysPTM-B
  FOREIGN KEY (sysptm_id) 
);

CREATE TABLE xref (
  sysptm_id VARCHAR(10) NOT NULL,               -- SysPTM Protein Identifier
  protein_id VARCHAR(45) NOT NULL,               -- Protein Identifier in public database
  database VARCHAR(45) NOT NULL,               -- public database
  FOREIGN KEY (sysptm_id) 
);

CREATE TABLE basic (
  sysptm_id VARCHAR(10) NOT NULL,               -- SysPTM Protein Identifier
  organism VARCHAR(80) NOT NULL,        -- organism
  seq TEXT  NOT NULL,        -- protein sequence
  gene_id VARCHAR(10) NOT NULL       -- Entrez Gene ID
  FOREIGN KEY (sysptm_id) 
);

CREATE TABLE de (
  sysptm_id VARCHAR(10) NOT NULL,               -- SysPTM Protein Identifier
  de text NOT NULL,                 -- Protein Description
  FOREIGN KEY (sysptm_id)
};

CREATE TABLE alias (
  sysptm_id VARCHAR(10) NOT NULL,               -- SysPTM Protein Identifier
  alias_symbol VARCHAR(80) NOT NULL,        -- gene symbol or alias
  FOREIGN KEY (sysptm_id) 
};

