--
-- SCOP_DB schema
-- ===============
--

CREATE TABLE des (
  scop_id VARCHAR(10) NOT NULL,               -- SCOP entry ID
  type CHAR(2) NOT NULL,         -- Type of SCOP entry
  class VARCHAR(20) NOT NULL,               -- Classification of SCOP entry
  name VARCHAR(10),        -- Name of SCOP entry
  de VARCHAR(250),         -- Description of SCOP entry
  FOREIGN KEY (scop_id) 
);

CREATE TABLE parent(
  scop_id VARCHAR(10) NOT NULL,               -- SCOP entry ID
  parent_id VARCHAR(10) NOT NULL,               -- Parent ID   
  FOREIGN KEY (scop_id) 
);

CREATE TABLE children(
  scop_id VARCHAR(10) NOT NULL,               -- SCOP entry ID
  children_id VARCHAR(10) NOT NULL,               -- Children ID   
  FOREIGN KEY (scop_id) 
);

CREATE TABLE pdb(
  pdb_id VARCHAR(10) NOT NULL,               -- PDB entry ID 
  pdb_chain VARCHAR(10) NOT NULL,               -- Chain of PDB entry
  cl VARCHAR(10) NOT NULL,               -- class in SCOP        
  cf VARCHAR(10) NOT NULL,               -- fold  in SCOP         
  sf VARCHAR(10) NOT NULL,               -- superfamily in SCOP  
  fa VARCHAR(10) NOT NULL,               -- family in SCOP       
  dm VARCHAR(10) NOT NULL,               -- protein domain in SCOP
  sp VARCHAR(10) NOT NULL,               -- species in SCOP       
  px VARCHAR(10) NOT NULL,               -- domain entry in SCOP  
);
