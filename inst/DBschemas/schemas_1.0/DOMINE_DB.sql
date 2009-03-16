--
-- DOMINE_DB schema
-- ===============
--

CREATE TABLE domine (
  pfam_a VARCHAR(10) NOT NULL,               -- Pfam domain ID
  pfam_b VARCHAR(10) NOT NULL,         -- Pfam domain ID
  method VARCHAR(10) NOT NULL,         -- Method for domain-domain interaction
);

CREATE TABLE domine (
  method VARCHAR(10) NOT NULL,         -- Method for domain-domain interaction
  method_de VARCHAR(255) NOT NULL,         -- Method description
);
