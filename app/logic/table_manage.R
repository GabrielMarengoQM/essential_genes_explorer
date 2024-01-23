box::use(
  app/logic/import_rda_data[...]
)

list_of_dpcs <- dpc_gene_list_data()
genesMetaDataDf_data <- meta_data_table_data()

#' @export
getHiddenColumns <- function(selected_sources) {
  source_to_columns <- list(

    'Gene IDs' = 0:8,
    'Mouse data' = 9:15,
    'Disease data' = 16:27,
    'Cell line data' = 28:34,
    'Sequencing data' = 35:52,
    'Pantherdb protein data' = 53:57,
    'Gene Ontology data' = 58:63,
    'Pathway data' = 62:65

  )

  hidden_columns <- unlist(lapply(selected_sources, function(source) {
    source_to_columns[[source]]
  }))

  all_columns <- 0:65
  visible_columns <- setdiff(all_columns, hidden_columns)
  return(visible_columns)
}

#' @export
getGeneListsFromSelect <- function(selected_dpcs) {
gene_lists <- c()
for (i in list_of_dpcs) {
  if (i[[2]] %in% selected_dpcs) {
    gene_lists <- c(gene_lists, i[[1]])
  }
}
return(gene_lists)
}

#' @export
subsetGenesMetaDataDf_rowsOnly <- function(genes_list) {
  gene <- genes_list[1]
  if (grepl("^HGNC:\\d+$", gene)) {
    data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$hgnc_id %in% genes_list, ]
  } else {
    # Check if the search term is an Entrez ID (e.g., ENSG12345)
    if (grepl("^\\d+$", gene)) {
      data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$entrez_id %in% genes_list, ]
    } else {
      # Check if the search term is an MGI ID (e.g., MGI:12345)
      if (grepl("^MGI:\\d+$", gene)) {
        data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$mgi_id %in% genes_list, ]
      } else {
        # Check if the search term is a gene symbol (e.g., Symbol_ABC)
        data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$gene_symbol %in% genes_list, ]
      }
    }
  }
  return(data_subset)
}

#' @export
filterGeneIDs <- function(meta_data_table, gene_symbol_filter, omim_gene_id_filter, mgi_id_filter) {

  table <- meta_data_table[meta_data_table$gene_symbol %in% gene_symbol_filter, ]
  table <- table[table$omim_gene_id %in% omim_gene_id_filter, ]
  table <- table[table$mgi_id %in% mgi_id_filter, ]

  return(table)
}

#' @export
filterCellLineConstraint <- function(meta_data_table, depmap_filter, mef_filter) {

  table <- meta_data_table[meta_data_table$depmap_mean_score_all >= depmap_filter[1] &
                             meta_data_table$depmap_mean_score_all <= depmap_filter[2], ]

  table <- table[table$bf_mef >= mef_filter[1] &
                   table$bf_mef <= mef_filter[2], ]

  # Remove rows that are only NAs
  table <- table[!apply(is.na(table), 1, all), ]

  return(table)
}


