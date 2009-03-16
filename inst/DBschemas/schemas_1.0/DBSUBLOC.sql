--
-- DBSUBLOC DB schema
-- ===============
--

CREATE TABLE dbsubloc (
  sp_ac VARCHAR(20) NOT NULL,             -- Swiss-Prot Protein accession
  organism VARCHAR(80) NOT NULL,        -- organism
  de VARCHAR(255) NOT NULL,                 -- Protein Description
  subcell VARCHAR(255) NOT NULL,        -- Subcellular location
  seq TEXT  NOT NULL,        -- protein sequence
  FOREIGN KEY (sp_id) 
);
