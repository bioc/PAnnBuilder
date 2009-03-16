--
-- INTACT_DB schema
-- ===============
--

CREATE TABLE intact (
  up_ac_a VARCHAR(10) NOT NULL       -- Uniprot Protein Accession of parter A
  up_ac_b VARCHAR(10) NOT NULL       -- Uniprot Protein Accession of parter B  
);

CREATE TABLE tax(
  up_ac VARCHAR(10) NOT NULL       -- Uniprot Protein Accession
  tax_id VARCHAR(10) NOT NULL       -- Tax ID 
);
