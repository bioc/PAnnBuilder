--
-- BACELLO DB schema
-- ===============
--

CREATE TABLE bacello (
  sp_id VARCHAR(20) NOT NULL,             -- Swiss-Prot Protein ID
  subcell VARCHAR(50) NOT NULL,        -- Subcellular location
  FOREIGN KEY (sp_id) 
);

CREATE TABLE seq (
  sp_id VARCHAR(20) NOT NULL,             -- Swiss-Prot Protein ID
  seq TEXT  NOT NULL,        -- protein sequence
  FOREIGN KEY (sp_id) 
);


