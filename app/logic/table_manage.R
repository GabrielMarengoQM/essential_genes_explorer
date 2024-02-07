box::use(
  utils[...]
)

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
getListOfGeneListInputs <- function() {

}

#' @export
getGeneListsFromSelect2 <- function(all_uploaded_gene_lists, selected_gene_lists) {


  selected_lists <- all_uploaded_gene_lists[selected_gene_lists]
  data <- unlist(selected_lists, use.names = FALSE)


  return(data)
}

#' @export
# userFilesUploadToList <- function(files_data, header_option) {
#   #print(files_data)
#   genelist_names_list <- list()
#   for (i in 1:nrow(files_data)) {
#     file_data <- read.csv(files_data[i, "datapath"], header = header_option)
#     # Remove ".csv" from the name
#     name_without_extension <- sub("\\.csv$", "", files_data[i, "name"])
#
#     genelist_names_list[[name_without_extension]] <- file_data[, 1]  }
#
#   return(genelist_names_list)
# }

#' @export
userFilesUploadToList <- function(files_data, header_option) {
  genelist_names_list <- list()
  for (i in 1:nrow(files_data)) {
    file_path <- files_data[i, "datapath"]
    file_extension <- tools::file_ext(file_path)

    if (file_extension == "csv") {
      file_data <- read.csv(file_path, header = header_option)
    } else if (file_extension %in% c("tsv", "tab")) {
      file_data <- read.delim(file_path, header = header_option)
    } else {
      stop(paste("Unsupported file format for file:", file_path))
    }

    # Remove file extension from the name
    name_without_extension <- sub(sprintf("\\.%s$", file_extension), "", files_data[i, "name"])

    genelist_names_list[[name_without_extension]] <- file_data[, 1]
  }

  return(genelist_names_list)
}

#' @export
noFileUploadHandler <- function(data) {
  return(data)
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
filterMouseModels <- function(meta_data_table,
                              mgi_viability_filter, impc_viability_filter
                              ,
                              impc_phenotypes_homozygote_filter,
                              impc_phenotypes_heterozygote_filter,
                              impc_phenotypes_hemizygote_filter
                              ) {

  table <- meta_data_table[meta_data_table$mgi_viability %in% mgi_viability_filter, ]
  table <- table[table$impc_viability %in% impc_viability_filter, ]
  table <- table[table$impc_phenotypes_homozygote %in% impc_phenotypes_homozygote_filter, ]
  table <- table[table$impc_phenotypes_heterozygote %in% impc_phenotypes_heterozygote_filter, ]
  table <- table[table$impc_phenotypes_hemizygote %in% impc_phenotypes_hemizygote_filter, ]

  return(table)
}

#' @export
filterCellLineConstraint <- function(meta_data_table, depmap_filter, mef_filter, lam_filter) {

  table <- meta_data_table[meta_data_table$depmap_mean_score_all >= depmap_filter[1] &
                             meta_data_table$depmap_mean_score_all <= depmap_filter[2], ]

  table <- table[table$bf_mef >= mef_filter[1] &
                   table$bf_mef <= mef_filter[2], ]

  table <- table[table$bf_lam >= lam_filter[1] &
                   table$bf_lam <= lam_filter[2], ]
  # print("CELL LINE")
  # print(table$bf_lam)
  # Remove rows that are only NAs
  #table <- table[!apply(is.na(table), 1, all), ]
  # print("CELL LINE AFTER REMOVING NA ROWS")
  # print(table$bf_lam)
  return(table)
}

#' @export
filterSequencingConstraint <- function(meta_data_table,
                                       gnomad_lof_oe_filter, gnomad_mis_oe_filter,
                                       shet_rgcme_mean_filter, shet_post_mean_filter,
                                       domino_filter, scones_filter,
                                       mean_am_pathogenicity_filter) {

  table <- meta_data_table[meta_data_table$gnomad_lof_oe >= gnomad_lof_oe_filter[1] &
                             meta_data_table$gnomad_lof_oe <= gnomad_lof_oe_filter[2], ]

  table <- table[table$gnomad_mis_oe >= gnomad_mis_oe_filter[1] &
                   table$gnomad_mis_oe <= gnomad_mis_oe_filter[2], ]

  table <- table[table$shet_rgcme_mean >= shet_rgcme_mean_filter[1] &
                   table$shet_rgcme_mean <= shet_rgcme_mean_filter[2], ]

  table <- table[table$shet_post_mean >= shet_post_mean_filter[1] &
                   table$shet_post_mean <= shet_post_mean_filter[2], ]

  table <- table[table$domino >= domino_filter[1] &
                   table$domino <= domino_filter[2], ]

  table <- table[table$scones >= scones_filter[1] &
                   table$scones <= scones_filter[2], ]

  table <- table[table$mean_am_pathogenicity >= mean_am_pathogenicity_filter[1] &
                   table$mean_am_pathogenicity <= mean_am_pathogenicity_filter[2], ]


  # Remove rows that are only NAs
  #table <- table[!apply(is.na(table), 1, all), ]

  return(table)
}


