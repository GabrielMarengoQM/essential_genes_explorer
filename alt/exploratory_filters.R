# min(all_protein_coding_genes_annotations$gnomad_lof_oe, na.rm = TRUE) # 0
# max(all_protein_coding_genes_annotations$gnomad_lof_oe, na.rm = TRUE) # 7.7728
#
# data <- all_protein_coding_genes_annotations
# filtered_data <- data[data$gnomad_lof_oe >= 0 & data$gnomad_lof_oe <= 0.5, ]
# filtered_data$gnomad_lof_oe
#
# ucsf_data <- read.delim("~/Desktop/test_gene_lists/UCSF_geneList.tsv")
#
# protein_coding_genes <- subset(data, data$gene_symbol %in% ucsf_data$gene_symbol)

