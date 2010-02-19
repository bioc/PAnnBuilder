--
-- dName_DB schema
-- ===============
--

CREATE TABLE go (
  go_id CHAR(10) NOT NULL,                      -- GO ID
  go_name VARCHAR(255) NOT NULL                
  FOREIGN KEY (go_id) 
};
CREATE TABLE kegg (
  path_id CHAR(5) NOT NULL,                      -- KEGG pathway ID
  path_de VARCHAR(255) NOT NULL                
  FOREIGN KEY (path_id) 
};
CREATE TABLE pfam (
  pfam_id CHAR(7) NULL,                          -- Pfam ID
  pfam_name VARCHAR(255) NOT NULL                
  pfam_de VARCHAR(255) NOT NULL                
  FOREIGN KEY (pfam_id) 
};
CREATE TABLE interpro (
  interpro_id CHAR(9) NOT NULL,                   -- InterPro ID  
  interpro_name VARCHAR(255) NOT NULL                
  FOREIGN KEY (interpro_id) 
};
CREATE TABLE prosite (
  prosite_id CHAR(9) NOT NULL,                   -- Prosite ID  
  prosite_de VARCHAR(255) NOT NULL                
  FOREIGN KEY (prosite_id) 
};
CREATE TABLE tax (
  tax_id VARCHAR(10) NOT NULL       -- Tax ID 
  tax_name VARCHAR(255) NOT NULL                
  FOREIGN KEY (tax_id) 
};
