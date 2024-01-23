box::use(
  shiny[...],
  rhino,
  DT[...],
  shinycssloaders[...],
  htmltools[...],
)

box::use(
  app/view/table_sidebar,
  app/logic/table_manage,
  app/logic/import_rda_data,
  app/logic/create_table
)


#' @export
ui <- function(id) {
  ns <- NS(id)
    DTOutput(ns("genesMetaDataDf"))
}

#' @export
server <- function(id, table_data, sidebar_data) {
  moduleServer(id, function(input, output, session) {

    output$genesMetaDataDf <- renderDataTable({

      sidebar_data$apply_filters()

      headers <- create_table$table_headers()
      selected_columns <- table_manage$getHiddenColumns(sidebar_data$selected_columns())
      user_chosen_gene_lists <- table_manage$getGeneListsFromSelect(sidebar_data$selected_dpc_gene_list())

      req(length(sidebar_data$selected_dpc_gene_list()) > 0)
      user_chosen_table_data <- table_manage$subsetGenesMetaDataDf_rowsOnly(user_chosen_gene_lists)

      # extra filters
      user_chosen_table_data <- isolate({

        user_chosen_table_data <- table_manage$filterGeneIDs(
        user_chosen_table_data,
        sidebar_data$filter_gene_symbol(),
        sidebar_data$filter_omim_gene_id(),
        sidebar_data$filter_mgi_id()
        )

        table_manage$filterCellLineConstraint(
          user_chosen_table_data,
          sidebar_data$filter_depmap(),
          sidebar_data$filter_mef()
        )

        })


      create_table$data_table(user_chosen_table_data, headers, selected_columns)
    }, server = TRUE)


  })
}

