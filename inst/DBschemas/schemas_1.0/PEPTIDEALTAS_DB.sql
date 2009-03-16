--
-- PEPTIDEATLAS_DB schema
-- ===============
--

CREATE TABLE pep2protein {
  peptide_id CHAR(11) NOT NULL,      -- Peptide Identifier in PeptideAtlas 
  protein_position VARCHAR(255) NOT NULL,      -- position of peptide on protein  
  FOREIGN KEY (peptide_id) 
};

CREATE TABLE pep2chr {
  peptide_id CHAR(11) NOT NULL,      -- Peptide Identifier in PeptideAtlas 
  chr_position VARCHAR(255) NOT NULL,      -- position of peptide on chromosome
  FOREIGN KEY (peptide_id) 
};

CREATE TABLE seq (
  peptide_id CHAR(11) NOT NULL,      -- Peptide Identifier in PeptideAtlas
  seq TEXT  NOT NULL,        -- protein sequence  
  FOREIGN KEY (peptide_id) 
);
