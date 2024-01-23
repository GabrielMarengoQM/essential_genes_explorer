box::use(
  shiny[...],
  rhino,
  DT,
  htmltools[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue],
  stats[...]
)

box::use(
  app/logic/import_rda_data
)

df <- import_rda_data$meta_data_table_data()
#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    pickerInput(
      ns("select_dpcs"),
      "Select Data Production Center",
      c("JAX", "MSK", "NWU", "UCSF"),
      multiple = TRUE,
      selected = "JAX",
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "300px"
      ),
      inline = FALSE
    ),
    pickerInput(
      ns("show_cols"),
      "Select meta data to display:",
      c('Gene IDs', 'Mouse data', 'Disease data', 'Cell line data',
        'Sequencing data', 'Pantherdb protein data', 'Gene Ontology data', 'Pathway data'),
      selected = c('Gene IDs', 'Mouse data', 'Disease data'),
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "300px"
      ),
      inline = FALSE
    ),
    dropdownButton(
      # Dropdown content
      pickerInput(ns("gene_symbol_filter"),
                  label = 'Gene symbols',
                  multiple = TRUE,
                  # df can be replaced with user inputted list
                  selected = df$gene_symbol,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    showTick = TRUE,
                    width = "300px"
                  ),
                  choices = df$gene_symbol),
      pickerInput(ns("omim_gene_id_filter"),
                  label = 'OMIM gene IDs',
                  multiple = TRUE,
                  selected = df$omim_gene_id,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    showTick = TRUE,
                    width = "300px"
                  ),
                  choices = na.omit(df$omim_gene_id)),
      pickerInput(ns("mgi_id_filter"),
                  label = 'MGI IDs',
                  multiple = TRUE,
                  selected = df$mgi_id,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    showTick = TRUE,
                    width = "300px"
                  ),
                  choices = na.omit(df$mgi_id)),

      # Dropdown options
      circle = FALSE,
      width = "300px",
      label = "Gene ID filters",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    dropdownButton(
      # Dropdown content
      sliderInput(ns('depmap_filter'),
                  label = 'mean score all depmap cancer lines',
                  value = c(min(df$depmap_mean_score_all, na.rm = TRUE), max(df$depmap_mean_score_all, na.rm = TRUE)),
                  min = min(df$depmap_mean_score_all, na.rm = TRUE),
                  max(df$depmap_mean_score_all, na.rm = TRUE)
      ),
      sliderInput(ns('mef_filter'),
                  label = 'bayes factor - mef fed HSPCs',
                  value = c(min(df$bf_mef, na.rm = TRUE), max(df$bf_mef, na.rm = TRUE)),
                  min = min(df$bf_mef, na.rm = TRUE),
                  max(df$bf_mef, na.rm = TRUE)
      ),
      # Dropdown options
      circle = FALSE,
      width = "300px",
      label = "Cell Line Constraint Metrics Filter",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    actionBttn(ns("apply_filters"), label = "Apply filters"),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$current_table_view <- renderUI({
      header <- 'Current view: '
      dpc_selected <- input$select_dpcs
      body <- dpc_selected
      body <- paste(body, collapse = ", ")

      HTML(glue("<p>{header} <br> {body}</p>"))
    })

    output$select_data_table <- renderText('Select Gene list:')

    list(
      selected_columns = reactive(input$show_cols),
      selected_dpc_gene_list = reactive(input$select_dpcs),
      apply_filters = reactive(input$apply_filters),
      # Gene ID filters
      filter_gene_symbol = reactive(input$gene_symbol_filter),
      filter_omim_gene_id = reactive(input$omim_gene_id_filter),
      filter_mgi_id = reactive(input$mgi_id_filter),
      # DepMap filters
      filter_depmap = reactive(input$depmap_filter),
      filter_mef = reactive(input$mef_filter)
    )
  })
}


