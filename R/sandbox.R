library(biomaRt)
library(org.Hs.eg.db)

human=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset = "hsapiens_gene_ensembl")
mouse=useMart("ENSEMBL_MART_ENSEMBL", host="www.ensembl.org", dataset = "mmusculus_gene_ensembl")
getLDS(attributes = c("hgnc_symbol"),
       filters = "hgnc_symbol", values = "TP53",mart = human,
       attributesL = c("mgi_symbol", "affy_mouse430a_2"), martL = mouse)

getBM(mart=mouse, attributes = c("mgi_symbol", "hgnc_symbol"), values="Trp53", filters="mgi_symbol")


y <- c(10, 4, 3, NA, 2, 1, 1, 1, 1, 1, 1)
z <- y * 2
list_data <- as.list(data.frame(t(cbind(y,z))))
V(x)$data3 <- list_data


library(SPARQL)
library(biomaRt)
library(org.Hs.eg.db)


query = "
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX aop: <http://aopkb.org/aop_ontology#>
PREFIX bao: <http://www.bioassayontology.org/bao#>

SELECT DISTINCT ?ao ?ke
WHERE {
?ao rdfs:subClassOf* aop:AdverseOutcome.
?ind_ao a ?ao.
?ind_ao aop:has_inactivated_key_event ?ke.
?measure_group bao:BAO_0090012 ?ke.
?bioassay_results bao:BAO_0000209 ?measure_group.
?bioassay_results aop:has_chem_bio_assay_call ?call.
?call aop:has_inactivated_key_event ?ke.
}
"

r <- SPARQL(url="http://localhost:3030/ds", query=query)
r$results
