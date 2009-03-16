--
-- GENEINT_DB schema
-- ===============
--

CREATE TABLE geneint (
  gene_id_a VARCHAR(10) NOT NULL       -- Entrez Gene ID of parter A
  gene_id_b VARCHAR(10) NOT NULL       -- Entrez Gene ID of parter B
  
);

CREATE TABLE tax(
  gene_id VARCHAR(10) NOT NULL       -- Entrez Gene ID
  tax_id VARCHAR(10) NOT NULL       -- Tax ID 
);
