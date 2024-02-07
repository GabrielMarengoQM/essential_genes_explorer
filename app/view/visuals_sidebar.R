## Imports
# Packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue]
)

# Modules
box::use(
)

# Rda data
box::use(
  app/logic/import_rda_data,
  app/view/table_sidebar
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      ns("select_dpcs_vis"),
      "Select Data Production Center",
      c("JAX", "MSK", "NWU", "UCSF", "All protein coding genes"),
      multiple = TRUE,
      selected = "JAX",
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "300px"
      ),
      inline = FALSE
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # main_sidebar_data <- table_sidebar$server("x")
    #
    # observeEvent(main_sidebar_data$user_files_upload(), {
    #   #updatePickerInput(session, "select_dpcs_vis", choices = names(main_sidebar_data$user_files_upload()), selected = names(main_sidebar_data$user_files_upload()))
    #   print(main_sidebar_data$user_files_upload())
    # })
    visuals_sidebar_data <- reactive(input$select_dpcs_vis)
  })
}
