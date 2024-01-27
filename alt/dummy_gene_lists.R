# Dummy gene list

morphic_gene_list_data <- readRDS("./rda/morphic_gene_list_data.rda")

morphic_gene_list_data[[1]][[2]] <- 'GeneList1'
morphic_gene_list_data[[2]][[2]] <- 'GeneList2'
morphic_gene_list_data[[3]][[2]] <- 'GeneList3'
morphic_gene_list_data[[4]][[2]] <- 'GeneList4'

morphic_gene_list_data[[3]][[1]] <- morphic_gene_list_data[[3]][[1]][1:200]

saveRDS(morphic_gene_list_data, "./rda/prototype_gene_lists.rda", compress = TRUE)

# Dummy min max values
proto_gene_lists <- readRDS("./rda/prototype_gene_lists.rda")

all_protein_coding_genes_annotations <- readRDS('./rda/all_protein_coding_genes_annotations.rda')

y <- c(proto_gene_lists[[1]][[1]], proto_gene_lists[[2]][[1]], proto_gene_lists[[3]][[1]], proto_gene_lists[[4]][[1]])

df <- all_protein_coding_genes_annotations %>%
  filter(all_protein_coding_genes_annotations$gene_symbol %in% y) %>%
  select(gene_symbol, bf_mef, bf_lam, depmap_mean_score_all)

min(df$bf_mef, na.rm = TRUE)
max(df$bf_mef, na.rm = TRUE)

