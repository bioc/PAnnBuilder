--
-- BF_DB schema
-- ===============
--

CREATE TABLE sysbodyfluid (
  ipi_id CHAR(11) NOT NULL,               -- IPI Protein Identifier
  pubmed_id VARCHAR(10) NOT NULL,         -- PubMed ID
  body_fluid VARCHAR(100) NOT NULL,        -- Body Fluid 
  FOREIGN KEY (ipi_id) 
);

CREATE TABLE paper (
  pubmed_id VARCHAR(10) NOT NULL,         -- PubMed ID
  title VARCHAR(255) NOT NULL,        -- Title of the paper
  platform VARCHAR(255) NOT NULL,        -- Experiment platform for body fluid protein identification
  body_fluid VARCHAR(100) NOT NULL,        -- Body Fluid 
  search_engine VARCHAR(100) NOT NULL,        -- Search Engine for protein identification
  sample  VARCHAR(255) NOT NULL,        -- Experimental Sample
  FOREIGN KEY (pubmed_id) 
);

CREATE INDEX Fsysbodyfluid ON sysbodyfluid (ipi_id);
CREATE INDEX Fpaper ON paper (pubmed_id);
