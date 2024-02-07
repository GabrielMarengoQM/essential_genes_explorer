# meta_data_table module

box::use(
  shiny[...],
  rhino,
  DT[...],
  shinycssloaders[...],
  htmltools[...],
  utils[...]
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

      req(!is.null(sidebar_data$filtered_meta_data_table()))
      headers <- create_table$table_headers()
      selected_columns <- table_manage$getHiddenColumns(sidebar_data$selected_columns())

      table <- sidebar_data$filtered_meta_data_table()
      #print(table)
      create_table$data_table(table, headers, selected_columns)

    },
    server = TRUE)

  })
}

