# Modify table to remove DPC col
library(tidyverse)
genesMetaDataDf_data <- readRDS('./rda/all_genes_with_anot_jan16.rda')

all_protein_coding_genes_annotations <- genesMetaDataDf_data %>%
  select(-dpcs_studying_gene)

saveRDS(all_protein_coding_genes_annotations, "./rda/all_protein_coding_genes_annotations.rda", compress = TRUE)
