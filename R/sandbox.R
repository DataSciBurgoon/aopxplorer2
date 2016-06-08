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
