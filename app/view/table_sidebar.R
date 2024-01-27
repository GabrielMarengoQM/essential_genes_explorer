# table_sidebar module

box::use(
  shiny[...],
  rhino,
  DT,
  htmltools[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue],
  stats[...],
  dplyr[...]
)

box::use(
  app/logic/import_rda_data,
  app/logic/table_manage
)

#df <- import_rda_data$meta_data_table_data()

proto_gene_lists <- readRDS("./rda/prototype_gene_lists.rda")

all_protein_coding_genes_annotations <- readRDS('./rda/all_protein_coding_genes_annotations.rda')

y <- c(proto_gene_lists[[1]][[1]], proto_gene_lists[[2]][[1]], proto_gene_lists[[3]][[1]], proto_gene_lists[[4]][[1]])

df <- all_protein_coding_genes_annotations %>%
  filter(all_protein_coding_genes_annotations$gene_symbol %in% y)

#table_page_data <- meta_data_table$server("meta_data_table")


#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(
      ns("user_files_upload"),
      "Upload gene list(s) as csv",
      multiple = TRUE,
      accept = NULL,
      width = "100%",
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    awesomeCheckbox(
      ns("header_option_box"),
      label = "Tick if files contain headers",
      value = TRUE
    ),
    pickerInput(
      ns("selected_gene_list"),
      "Select Data Gene List",
      choices = NULL,
      multiple = TRUE,
      selected = NULL,
      options = pickerOptions(
        actionsBox = TRUE,
        showTick = TRUE,
        width = "100%"
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
        width = "100%"
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
                    width = "100%"
                  ),
                  choices = df$gene_symbol),
      pickerInput(ns("omim_gene_id_filter"),
                  label = 'OMIM gene IDs',
                  multiple = TRUE,
                  selected = df$omim_gene_id,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    showTick = TRUE,
                    width = "100%"
                  ),
                  choices = na.omit(df$omim_gene_id)),
      pickerInput(ns("mgi_id_filter"),
                  label = 'MGI IDs',
                  multiple = TRUE,
                  selected = df$mgi_id,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    showTick = TRUE,
                    width = "100%"
                  ),
                  choices = na.omit(df$mgi_id)),

      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Gene ID filters",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    dropdownButton(
      # Dropdown content
      sliderInput(ns('depmap_filter'),
                  label = 'Gene effect score - mean across all DepMap cancer lines',
                  value = c(min(df$depmap_mean_score_all, na.rm = TRUE), max(df$depmap_mean_score_all, na.rm = TRUE)),
                  min = min(df$depmap_mean_score_all, na.rm = TRUE),
                  max(df$depmap_mean_score_all, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      sliderInput(ns('mef_filter'),
                  label = 'Bayes factor - mef fed HSPCs',
                  value = c(min(df$bf_mef, na.rm = TRUE), max(df$bf_mef, na.rm = TRUE)),
                  min = min(df$bf_mef, na.rm = TRUE),
                  max(df$bf_mef, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      sliderInput(ns('lam_filter'),
                  label = 'Bayes factor - laminin fed HSPCs',
                  value = c(min(df$bf_lam, na.rm = TRUE), max(df$bf_lam, na.rm = TRUE)),
                  min = min(df$bf_lam, na.rm = TRUE),
                  max(df$bf_lam, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Constraint Metrics - Cell Lines",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    dropdownButton(
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Constraint Metrics - Population Sequencing",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    dropdownButton(
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Mouse Viability screens & Phenotypes",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    dropdownButton(
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Human Diseases",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    dropdownButton(
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Reactome Pathways",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    dropdownButton(
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Gene Ontologies",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    dropdownButton(
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Proteins",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    actionBttn(ns("apply_filters"), label = "Apply filters"),
    uiOutput(ns("test"))
  )
}

#' @export
server <- function(id, filtered_df) {
  moduleServer(id, function(input, output, session) {


    user_files_upload_data <- reactive({

      input$user_files_upload

      files_data <- input$user_files_upload
      files_data2 <- table_manage$userFilesUploadToList(files_data, input$header_option_box)

    })

    observeEvent(input$user_files_upload, {
      # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
      updatePickerInput(session, "selected_gene_list", choices = names(user_files_upload_data()))
    })

    filtered_table <- reactive({
      # Take dependency on apply filters button
      input$apply_filters

      # Select gene list and columns
      isolate({
        #selected_columns <- table_manage$getHiddenColumns(input$show_cols)

        user_chosen_gene_lists <- table_manage$getGeneListsFromSelect2(user_files_upload_data(), input$selected_gene_list)
        #print(user_chosen_gene_lists)
        req(length(input$selected_gene_list) > 0) # maybe move one up
        user_chosen_table_data <- table_manage$subsetGenesMetaDataDf_rowsOnly(user_chosen_gene_lists)
        #print(user_chosen_table_data)


        # Filters
        user_chosen_table_data1 <- table_manage$filterGeneIDs(
          user_chosen_table_data,
          input$gene_symbol_filter,
          input$omim_gene_id_filter,
          input$mgi_id_filter
        )

        user_chosen_table_data2 <- table_manage$filterCellLineConstraint(
          user_chosen_table_data1,
          input$depmap_filter,
          input$mef_filter,
          input$lam_filter
        )
      })
    })

    # for updating slider values
    observeEvent(input$apply_filters, {

      user_chosen_table_data2 <- filtered_table()

      updateSliderInput(session, "depmap_filter",
                        min = min(user_chosen_table_data2[['depmap_mean_score_all']], na.rm = TRUE),
                        max = max(user_chosen_table_data2[['depmap_mean_score_all']], na.rm = TRUE))
      updateSliderInput(session, "mef_filter",
                        min = min(user_chosen_table_data2[['bf_mef']], na.rm = TRUE),
                        max = max(user_chosen_table_data2[['bf_mef']], na.rm = TRUE))
      updateSliderInput(session, "lam_filter",
                        min = min(user_chosen_table_data2[['bf_lam']], na.rm = TRUE),
                        max = max(user_chosen_table_data2[['bf_lam']], na.rm = TRUE))
    })




    list(
      # File input
      user_files_upload = reactive(user_files_upload_data()),
      # Selected df columns
      selected_columns = reactive(input$show_cols),
      # Selected gene list
      selected_gene_list = reactive(input$selected_gene_list),
      # Filter action button
      apply_filters = reactive(input$apply_filters),
      # Gene ID filters
      filter_gene_symbol = reactive(input$gene_symbol_filter),
      filter_omim_gene_id = reactive(input$omim_gene_id_filter),
      filter_mgi_id = reactive(input$mgi_id_filter),
      # DepMap filters
      filter_depmap = reactive(input$depmap_filter),
      filter_mef = reactive(input$mef_filter),
      filter_lam = reactive(input$lam_filter),
      # Filtered df
      filtered_meta_data_table = reactive(filtered_table())
    )
  })
}


