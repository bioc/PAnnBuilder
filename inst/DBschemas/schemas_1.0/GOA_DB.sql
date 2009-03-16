--
-- GOA_DB schema
-- ===============
--
CREATE TABLE go {
  sp_ac VARCHAR(20) NOT NULL,               -- Swiss-Prot Primary Acession Number  
  go_id CHAR(10) NOT NULL,                      -- GO ID
  evidence CHAR(3) NOT NULL,                    -- GO evidence code
  ontology CHAR(2) NOT NULL,                    -- GO ontology
  FOREIGN KEY (sysptm_ac)
};

CREATE TABLE de {
  sp_ac VARCHAR(20) NOT NULL,               -- Swiss-Prot Primary Acession Number  
  de VARCHAR(255) NOT NULL,                 -- Protein Description
  FOREIGN KEY (sysptm_ac)
};

CREATE TABLE id (
  sp_ac VARCHAR(20) NOT NULL,               -- Swiss-Prot Primary Acession Number  
  sp_id VARCHAR(20) NOT NULL,               -- Swiss-Prot Protein Identifier
  FOREIGN KEY (sysptm_ac)
};
