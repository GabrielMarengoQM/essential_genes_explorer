# meta_data_table module

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

    #user_chosen_table_data_val <- reactiveVal(NULL)

    output$genesMetaDataDf <- renderDataTable({

      #sidebar_data$apply_filters()

      headers <- create_table$table_headers()
      selected_columns <- table_manage$getHiddenColumns(sidebar_data$selected_columns())
      # user_chosen_gene_lists <- table_manage$getGeneListsFromSelect2(import_rda_data$proto_gene_lists(), sidebar_data$selected_gene_list())
      #
      # req(length(sidebar_data$selected_gene_list()) > 0)
      # user_chosen_table_data <- table_manage$subsetGenesMetaDataDf_rowsOnly(user_chosen_gene_lists)

      # extra filters
      # user_chosen_table_data <- isolate({
      #
      #   user_chosen_table_data <- table_manage$filterGeneIDs(
      #   user_chosen_table_data,
      #   sidebar_data$filter_gene_symbol(),
      #   sidebar_data$filter_omim_gene_id(),
      #   sidebar_data$filter_mgi_id()
      #   )
      #
      #   table_manage$filterCellLineConstraint(
      #     user_chosen_table_data,
      #     sidebar_data$filter_depmap(),
      #     sidebar_data$filter_mef(),
      #     sidebar_data$filter_lam()
      #   )
      #
      #   })

      #user_chosen_table_data_val(user_chosen_table_data)
      table <- sidebar_data$filtered_meta_data_table()
      #print(sidebar_data$user_files_upload())

      create_table$data_table(sidebar_data$filtered_meta_data_table(), headers, selected_columns)

    },
    server = TRUE)

    # list(
    #   df_for_sidebar = reactive(user_chosen_table_data_val())
    # )
  })
}

