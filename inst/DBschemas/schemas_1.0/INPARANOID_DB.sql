--
-- INPARANOID_DB schema
-- ===============
--

CREATE TABLE ortholog {
  ortholog_id VARCHAR(10) NOT NULL       -- Identifier of ortholog group
  organism VARCHAR(80) NOT NULL,        -- organism
  protein_id VARCHAR(45) NOT NULL,      -- Protein Identifier in public database
};

CREATE TABLE seq (
  protein_id VARCHAR(45) NOT NULL,      -- Protein Identifier in public database
  seq TEXT  NOT NULL,        -- protein sequence  
);
