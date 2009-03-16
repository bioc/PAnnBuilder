--
-- CROSS_DB schema
-- ===============
--

CREATE TABLE seq (
  protein_id VARCHAR(20) NOT NULL,               -- Protein Identifier
  seq TEXT NOT NULL,               -- Protein Sequence
  FOREIGN KEY (protein_id) 
);

CREATE TABLE match (
  query VARCHAR(20) NOT NULL,               
  subject VARCHAR(20) NOT NULL,             
  FOREIGN KEY (query) 
);
