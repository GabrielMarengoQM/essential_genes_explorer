# main module

## Imports
# Packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...]
)

# Modules
box::use(
  app/view[table_sidebar, meta_data_table, visuals_sidebar, visuals_layout, metadata_info_page]
)

# Rda data
box::use(
  app/logic/import_rda_data
  )

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title = "Essential Genes Explorer",
    bg = "#00703C",
    # Gene List Browser tab
    nav_panel(
      title = "Gene List Browser",
      page_sidebar(
        # Sidebar
        sidebar = sidebar(
          width = "420px",
          open = c('open'),
          table_sidebar$ui(ns("table_sidebar"))
        ),
        # Main content
        meta_data_table$ui(ns("meta_data_table"))
      )
    ),
    # Visualisations tab
    nav_panel(
      title = "Visualisations",
      visuals_layout$ui(ns("visuals_layout"))
      ),
    # Metadata Information tab
    nav_panel(
      title = "Metadata Information",
      metadata_info_page$ui(ns("metadata_info_page"))
    ),
    nav_spacer(),
    # Links
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(tags$a(href = 'https://morphic.bio/', "MorPhiC Home Page"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Gene List Browser tab
    table_data <- import_rda_data$meta_data_table_data()
    sidebar_data <- table_sidebar$server("table_sidebar")
    meta_data_table$server("meta_data_table", table_data, sidebar_data)
    filtered_df <- meta_data_table$server("meta_data_table", table_data, sidebar_data)
    table_sidebar$server("table_sidebar")
    # Visualisations tab
    visuals_layout$server("visuals_layout", sidebar_data)

    # ADD REQS TO VIS TO NOT THROW ERROR BEFORE USER UPLOAD
  })
}
