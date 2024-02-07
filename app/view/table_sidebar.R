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
  dplyr[...],
  utils[...]
)

box::use(
  app/logic/import_rda_data,
  app/logic/table_manage
)

#df <- import_rda_data$meta_data_table_data()

# proto_gene_lists <- readRDS("./rda/prototype_gene_lists.rda")
#
# all_protein_coding_genes_annotations <- readRDS('./rda/all_protein_coding_genes_annotations.rda')
#
# y <- c(proto_gene_lists[[1]][[1]], proto_gene_lists[[2]][[1]], proto_gene_lists[[3]][[1]], proto_gene_lists[[4]][[1]])

# df <- all_protein_coding_genes_annotations %>%
#   filter(all_protein_coding_genes_annotations$gene_symbol %in% y)

#table_page_data <- meta_data_table$server("meta_data_table")

df <- import_rda_data$meta_data_table_data()
#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(
      ns("user_files_upload"),
      "Upload gene list(s) as csv or tsv",
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
    actionButton(
      ns('get_annotations'),
      "Retreive annotations for gene lists"
    ),
    hr(),
    # pickerInput(
    #   ns("selected_gene_list"),
    #   "Select Gene List",
    #   choices = NULL,
    #   multiple = TRUE,
    #   selected = NULL,
    #   options = pickerOptions(
    #     actionsBox = TRUE,
    #     showTick = TRUE,
    #     width = "100%"
    #   ),
    #   inline = FALSE
    # ),
    
    uiOutput(ns("selected_gene_list")),
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
    hr(),
    p("Filters"),
    # dropdownButton(
    #   # uiOutput(
    #   #   ns("impc_viability_filter")
    #   # )
    # ),
    # dropdownButton(
    #   # Dropdown content
    #   pickerInput(ns("gene_symbol_filter"),
    #               label = 'Gene symbols',
    #               multiple = TRUE,
    #               # df can be replaced with user inputted list
    #               selected = df$gene_symbol,
    #               options = pickerOptions(
    #                 actionsBox = TRUE,
    #                 showTick = TRUE,
    #                 width = "100%"
    #               ),
    #               choices = df$gene_symbol),
    #   pickerInput(ns("omim_gene_id_filter"),
    #               label = 'OMIM gene IDs',
    #               multiple = TRUE,
    #               selected = df$omim_gene_id,
    #               options = pickerOptions(
    #                 actionsBox = TRUE,
    #                 showTick = TRUE,
    #                 width = "100%"
    #               ),
    #               choices = na.omit(df$omim_gene_id)
    #               ),
    #   pickerInput(ns("mgi_id_filter"),
    #               label = 'MGI IDs',
    #               multiple = TRUE,
    #               selected = df$mgi_id,
    #               options = pickerOptions(
    #                 actionsBox = TRUE,
    #                 showTick = TRUE,
    #                 width = "100%"
    #               ),
    #               choices = na.omit(df$mgi_id)
    #               ),
    # 
    #   # Dropdown options
    #   circle = FALSE,
    #   width = "100%",
    #   label = "Gene ID filters",
    #   tooltip = tooltipOptions(title = "Click to see filters")
    # ),
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
      sliderInput(ns('gnomad_lof_oe_filter'),
                  label = 'Observed/Expected Loss of Function - gnomAD dataset',
                  value = c(min(df$gnomad_lof_oe, na.rm = TRUE), max(df$gnomad_lof_oe, na.rm = TRUE)),
                  min = min(df$gnomad_lof_oe, na.rm = TRUE),
                  max(df$gnomad_lof_oe, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      sliderInput(ns('gnomad_mis_oe_filter'),
                  label = 'Observed/Expected Missense - gnomAD dataset',
                  value = c(min(df$bf_mef, na.rm = TRUE), max(df$bf_mef, na.rm = TRUE)),
                  min = min(df$bf_mef, na.rm = TRUE),
                  max(df$bf_mef, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      sliderInput(ns('shet_rgcme_mean_filter'),
                  label = 'Shet - RGC-ME dataset ',
                  value = c(min(df$shet_rgcme_mean, na.rm = TRUE), max(df$shet_rgcme_mean, na.rm = TRUE)),
                  min = min(df$shet_rgcme_mean, na.rm = TRUE),
                  max(df$shet_rgcme_mean, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      sliderInput(ns('shet_post_mean_filter'),
                  label = 'Shet posterior estimation',
                  value = c(min(df$shet_post_mean, na.rm = TRUE), max(df$shet_post_mean, na.rm = TRUE)),
                  min = min(df$shet_post_mean, na.rm = TRUE),
                  max(df$shet_post_mean, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      sliderInput(ns('domino_filter'),
                  label = 'DOMINO score',
                  value = c(min(df$domino, na.rm = TRUE), max(df$domino, na.rm = TRUE)),
                  min = min(df$domino, na.rm = TRUE),
                  max(df$domino, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      sliderInput(ns('scones_filter'),
                  label = 'SCoNeS score',
                  value = c(min(df$scones, na.rm = TRUE), max(df$scones, na.rm = TRUE)),
                  min = min(df$scones, na.rm = TRUE),
                  max(df$scones, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      sliderInput(ns('mean_am_pathogenicity_filter'),
                  label = 'AlphaMissense predicted pathogenicity score',
                  value = c(min(df$mean_am_pathogenicity, na.rm = TRUE), max(df$mean_am_pathogenicity, na.rm = TRUE)),
                  min = min(df$mean_am_pathogenicity, na.rm = TRUE),
                  max(df$mean_am_pathogenicity, na.rm = TRUE),
                  round = 2,
                  width = "100%"
      ),
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Constraint Metrics - Population Sequencing",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    dropdownButton(
      pickerInput(ns("mgi_viability_filter"),
                  label = 'MGI preweening viability',
                  multiple = TRUE,
                  selected = NULL,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    showTick = TRUE,
                    width = "100%"
                  ),
                  choices = NULL),
      pickerInput(ns("impc_viability_filter"),
                  label = 'IMPC preweening viability',
                  multiple = TRUE,
                  selected = df$impc_viability,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    showTick = TRUE,
                    width = "100%"
                  ),
                  choices = NULL),
      # pickerInput(ns("impc_phenotypes_homozygote_filter"),
      #             label = 'IMPC homozygote phenotype',
      #             multiple = TRUE,
      #             selected = df$impc_phenotypes_homozygote,
      #             options = pickerOptions(
      #               actionsBox = TRUE,
      #               showTick = TRUE,
      #               width = "100%"
      #             ),
      #             choices = na.omit(df$impc_phenotypes_homozygote)),
      # pickerInput(ns("impc_phenotypes_heterozygote_filter"),
      #             label = 'IMPC heterozygote phenotype',
      #             multiple = TRUE,
      #             selected = df$impc_phenotypes_heterozygote,
      #             options = pickerOptions(
      #               actionsBox = TRUE,
      #               showTick = TRUE,
      #               width = "100%"
      #             ),
      #             choices = na.omit(df$impc_phenotypes_heterozygote)),
      # pickerInput(ns("impc_phenotypes_hemizygote_filter"),
      #             label = 'IMPC hemizygote phenotype',
      #             multiple = TRUE,
      #             selected = df$impc_phenotypes_hemizygote,
      #             options = pickerOptions(
      #               actionsBox = TRUE,
      #               showTick = TRUE,
      #               width = "100%"
      #             ),
      #             choices = na.omit(df$impc_phenotypes_hemizygote)),
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Mouse Viability screens",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    # OMIM ----
    dropdownButton(
      pickerInput(ns("omim_phenotpye_filter"),
                  label = 'Disease phenotype',
                  multiple = TRUE,
                  selected = df$omim_phenotype_name,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    showTick = TRUE,
                    width = "100%"
                  ),
                  choices = na.omit(df$omim_phenotype_name)),
      pickerInput(ns("omim_gene_lethality_filter"),
                  label = 'Lethal Phenotypes',
                  multiple = TRUE,
                  selected = df$omim_gene_lethality,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    showTick = TRUE,
                    width = "100%"
                  ),
                  choices = na.omit(df$omim_gene_lethality)),
      # Dropdown options
      circle = FALSE,
      width = "100%",
      label = "Human Diseases",
      tooltip = tooltipOptions(title = "Click to see filters")
    ),
    # dropdownButton(
    #   # Dropdown options
    #   circle = FALSE,
    #   width = "100%",
    #   label = "Reactome Pathways",
    #   tooltip = tooltipOptions(title = "Click to see filters")
    # ),
    # dropdownButton(
    #   # Dropdown options
    #   circle = FALSE,
    #   width = "100%",
    #   label = "Gene Ontologies",
    #   tooltip = tooltipOptions(title = "Click to see filters")
    # ),
    # dropdownButton(
    #   # Dropdown options
    #   circle = FALSE,
    #   width = "100%",
    #   label = "Proteins",
    #   tooltip = tooltipOptions(title = "Click to see filters")
    # ),
    actionBttn(ns("apply_filters"), label = "Apply filters"),
    uiOutput(ns("test"))
  )
}

#' @export
server <- function(id, filtered_df) {
  moduleServer(id, function(input, output, session) {
    
    # Get gene lists with names
    user_files_upload_data <- reactive({
      
      req(!is.null(input$user_files_upload))
      files_data <- input$user_files_upload
      files_data2 <- table_manage$userFilesUploadToList(files_data, input$header_option_box)
      
    })
    
    filtered_table <- reactiveVal({NULL})
    
    observeEvent(input$get_annotations, {
      #print(input$get_annotations)
      #req(!is.null(input$user_files_upload))
      user_chosen_gene_lists <- table_manage$getGeneListsFromSelect2(user_files_upload_data(), names(user_files_upload_data()))
      user_chosen_table_data <- table_manage$subsetGenesMetaDataDf_rowsOnly(user_chosen_gene_lists)
      filtered_table(user_chosen_table_data)
    })
    
    # output$impc_viability_filter <- renderUI({
    #   df <- filtered_table()
    #   pickerInput(
    #     "impc_viability_filter",
    #     "Select Gene List",
    #     choices = unique(df[["impc_viability"]]),
    #     multiple = TRUE,
    #     selected = unique(df[["impc_viability"]]),
    #     options = pickerOptions(
    #       actionsBox = TRUE,
    #       showTick = TRUE,
    #       width = "100%"
    #     ),
    #     inline = FALSE
    #   )
    #   #print(input$selected_gene_list)
    # })
    
    observe({
      req(!is.null(input$user_files_upload))
      # mice
      updatePickerInput(
        session, "impc_viability_filter", 
        choices = unique(filtered_table()[["impc_viability"]]), 
        selected = unique(filtered_table()[["impc_viability"]])
        )
      updatePickerInput(
        session, "mgi_viability_filter", 
        choices = unique(filtered_table()[["mgi_viability"]]), 
        selected = unique(filtered_table()[["mgi_viability"]])
      )
      
      # omim
      updatePickerInput(
        session, "omim_phenotype_name_filter", 
        choices = unique(filtered_table()[["omim_phenotype_name"]]), 
        selected = unique(filtered_table()[["omim_phenotype_name"]])
      )
      updatePickerInput(
        session, "omim_gene_lethality_filter", 
        choices = unique(filtered_table()[["omim_gene_lethality"]]), 
        selected = unique(filtered_table()[["omim_gene_lethality"]])
      )
      
      # cell lines
      updateSliderInput(session, "depmap_filter",
                        min = min(filtered_table()[['depmap_mean_score_all']], na.rm = TRUE),
                        max = max(filtered_table()[['depmap_mean_score_all']], na.rm = TRUE))
      updateSliderInput(session, "mef_filter",
                        min = min(filtered_table()[['bf_mef']], na.rm = TRUE),
                        max = max(filtered_table()[['bf_mef']], na.rm = TRUE))
      updateSliderInput(session, "lam_filter",
                        min = min(filtered_table()[['bf_lam']], na.rm = TRUE),
                        max = max(filtered_table()[['bf_lam']], na.rm = TRUE))
      
      # sequencing
      updateSliderInput(session, "gnomad_lof_oe_filter",
                        min = min(filtered_table()[['gnomad_lof_oe']], na.rm = TRUE),
                        max = max(filtered_table()[['gnomad_lof_oe']], na.rm = TRUE))
      updateSliderInput(session, "gnomad_mis_oe_filter",
                        min = min(filtered_table()[['gnomad_mis_oe']], na.rm = TRUE),
                        max = max(filtered_table()[['gnomad_mis_oe']], na.rm = TRUE))
      updateSliderInput(session, "shet_rgcme_mean_filter",
                        min = min(filtered_table()[['shet_rgcme_mean']], na.rm = TRUE),
                        max = max(filtered_table()[['shet_rgcme_mean']], na.rm = TRUE))
      updateSliderInput(session, "shet_post_mean_filter",
                        min = min(filtered_table()[['shet_post_mean']], na.rm = TRUE),
                        max = max(filtered_table()[['shet_post_mean']], na.rm = TRUE))
      updateSliderInput(session, "domino_filter",
                        min = min(filtered_table()[['domino']], na.rm = TRUE),
                        max = max(filtered_table()[['domino']], na.rm = TRUE))
      updateSliderInput(session, "scones_filter",
                        min = min(filtered_table()[['scones']], na.rm = TRUE),
                        max = max(filtered_table()[['scones']], na.rm = TRUE))
      updateSliderInput(session, "mean_am_pathogenicity_filter",
                        min = min(filtered_table()[['mean_am_pathogenicity']], na.rm = TRUE),
                        max = max(filtered_table()[['mean_am_pathogenicity']], na.rm = TRUE))
    })
    
    # impc_filter_vals <- reactive({
    #   print(input$impc_viability_filter)
    # })
    # 
    # 
    # 
    observeEvent(input$apply_filters, {
      # print(input$impc_viability_filter)
      current_df <- filtered_table()
      filtered_data <- current_df %>%
        filter((is.na(impc_viability) | impc_viability %in% input$impc_viability_filter)) %>%
        filter(is.na(mgi_viability) | mgi_viability %in% input$mgi_viability_filter) %>%
        filter(depmap_mean_score_all >= input$depmap_filter[1] &
                depmap_mean_score_all <= input$depmap_filter[2]) %>%
        filter(bf_mef >= input$mef_filter[1] &
                 bf_mef <= input$mef_filter[2]) %>%
        filter(bf_lam >= input$lam_filter[1] &
                 bf_lam <= input$lam_filter[2]) %>%
        filter(gnomad_lof_oe >= input$gnomad_lof_oe_filter[1] &
                 gnomad_lof_oe <= input$gnomad_lof_oe_filter[2]) %>%
        filter(gnomad_mis_oe >= input$gnomad_mis_oe_filter[1] &
                 gnomad_mis_oe <= input$gnomad_mis_oe_filter[2]) %>%
        filter(shet_rgcme_mean >= input$shet_rgcme_mean_filter[1] &
                 shet_rgcme_mean <= input$shet_rgcme_mean_filter[2]) %>%
        filter(shet_post_mean >= input$shet_post_mean_filter[1] &
                 shet_post_mean <= input$shet_post_mean_filter[2]) %>%
        filter(domino >= input$domino_filter[1] &
                 domino <= input$domino_filter[2]) %>%
        filter(scones >= input$scones_filter[1] &
                 scones <= input$scones_filter[2]) %>%
        filter(mean_am_pathogenicity >= input$mean_am_pathogenicity_filter[1] &
                 mean_am_pathogenicity <= input$mean_am_pathogenicity_filter[2]) %>%
        filter(is.na(omim_phenotype_name) | omim_phenotype_name %in% input$omim_phenotype_filter) %>%
        filter(is.na(omim_gene_lethality) | omim_gene_lethality %in% input$omim_gene_lethality_filter)

      filtered_table(filtered_data)
    })
    
    # filtered_table_value <- reactive({
    #   x <- filtered_table()
    # })
    
    # filtered_table <- reactive({
    #   
    #   input$get_annotations
    #   print(input$get_annotations)
    #   # all genes
    #   isolate({
    #     req(!is.null(input$user_files_upload))
    #     user_chosen_gene_lists <- table_manage$getGeneListsFromSelect2(user_files_upload_data(), names(user_files_upload_data()))
    #     #req(length(input$selected_gene_list) > 0)
    #     # annotations
    #     user_chosen_table_data <- table_manage$subsetGenesMetaDataDf_rowsOnly(user_chosen_gene_lists)
    #     #print(nrow(user_chosen_table_data))
    #   })
    #   user_chosen_table_data
    #   
    #   # Filters
    #   # input$apply_filters
    #   # isolate({
    #   #   user_chosen_table_data1 <- table_manage$filterGeneIDs(
    #   #     user_chosen_table_data,
    #   #     input$gene_symbol_filter,
    #   #     input$omim_gene_id_filter,
    #   #     input$mgi_id_filter
    #   #   )
    #   #   
    #   #   user_chosen_table_data2 <- table_manage$filterCellLineConstraint(
    #   #     user_chosen_table_data1,
    #   #     input$depmap_filter,
    #   #     input$mef_filter,
    #   #     input$lam_filter
    #   #   )
    #   #   
    #   #   user_chosen_table_data3 <- table_manage$filterSequencingConstraint(
    #   #     user_chosen_table_data2,
    #   #     input$gnomad_lof_oe_filter,
    #   #     input$gnomad_mis_oe_filter,
    #   #     input$shet_rgcme_mean_filter,
    #   #     input$shet_post_mean_filter,
    #   #     input$domino_filter,
    #   #     input$scones_filter,
    #   #     input$mean_am_pathogenicity_filter
    #   #     
    #   #   )
    #   # })
    # })
    
    
   
    
    # UPDATE GENE LIST PICKER
    # observeEvent(input$user_files_upload, {
    #   # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
    #   updatePickerInput(session, "selected_gene_list",
    #                     choices = names(user_files_upload_data()),
    #                     selected = names(user_files_upload_data())
    #                     )
    #   print(input$selected_gene_list)
    # })
    
    # ALTNERATIVE METHOD FOR RENDERING GENE LIST PICKER
    # output$selected_gene_list <- renderUI({
    #   pickerInput(
    #     "selected_gene_list",
    #     "Select Gene List",
    #     choices = names(user_files_upload_data()),
    #     multiple = TRUE,
    #     selected = names(user_files_upload_data()),
    #     options = pickerOptions(
    #       actionsBox = TRUE,
    #       showTick = TRUE,
    #       width = "100%"
    #     ),
    #     inline = FALSE
    #   )
    #   #print(input$selected_gene_list)
    # })
    #
    # observe({
    #   input$selected_gene_list
    #   print(input$selected_gene_list)
    # })
    
    # Initialise values upon file upload
    # observeEvent(input$user_files_upload, {
    #   
    #   user_chosen_table_data <- filtered_table()
    #   
    #   # Update gene id pciker inputs
    #   updatePickerInput(session, "gene_symbol_filter", choices = user_chosen_table_data[['gene_symbol']], selected = user_chosen_table_data[['gene_symbol']])
    #   updatePickerInput(session, "omim_gene_id_filter", choices = user_chosen_table_data[['omim_gene_id']], selected = user_chosen_table_data[['omim_gene_id']])
    #   updatePickerInput(session, "mgi_id_filter", choices = user_chosen_table_data[['mgi_id']], selected = user_chosen_table_data[['mgi_id']])
    #   
    #   # DF is loosing rows where values are NA -> need to keep these rows
    #   updatePickerInput(session, "mgi_viability_filter", choices = unique(user_chosen_table_data[['mgi_viability']]), selected = user_chosen_table_data[['mgi_viability']])
    #   updatePickerInput(session, "impc_viability_filter", choices = unique(user_chosen_table_data[['impc_viability']]), selected = user_chosen_table_data[['impc_viability']])
    #   updatePickerInput(session, "impc_phenotypes_homozygote_filter", choices = unique(user_chosen_table_data[['impc_phenotypes_homozygote']]), selected = user_chosen_table_data[['impc_phenotypes_homozygote']])
    #   updatePickerInput(session, "impc_phenotypes_heterozygote_filter", choices = unique(user_chosen_table_data[['impc_phenotypes_heterozygote']]), selected = user_chosen_table_data[['impc_phenotypes_heterozygote']])
    #   updatePickerInput(session, "impc_phenotypes_hemizygote_filter", choices = unique(user_chosen_table_data[['impc_phenotypes_hemizygote']]), selected = user_chosen_table_data[['impc_phenotypes_hemizygote']])
    #   
    #   # Update cell line constraint metrics sliders
    #   updateSliderInput(session, "depmap_filter",
    #                     min = min(user_chosen_table_data[['depmap_mean_score_all']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['depmap_mean_score_all']], na.rm = TRUE))
    #   updateSliderInput(session, "mef_filter",
    #                     min = min(user_chosen_table_data[['bf_mef']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['bf_mef']], na.rm = TRUE))
    #   updateSliderInput(session, "lam_filter",
    #                     min = min(user_chosen_table_data[['bf_lam']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['bf_lam']], na.rm = TRUE))
    #   
    #   # Update sequencing constraint metrics sliders
    #   updateSliderInput(session, "gnomad_lof_oe_filter",
    #                     min = min(user_chosen_table_data[['gnomad_lof_oe']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['gnomad_lof_oe']], na.rm = TRUE))
    #   updateSliderInput(session, "gnomad_mis_oe_filter",
    #                     min = min(user_chosen_table_data[['gnomad_mis_oe']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['gnomad_mis_oe']], na.rm = TRUE))
    #   updateSliderInput(session, "shet_rgcme_mean_filter",
    #                     min = min(user_chosen_table_data[['shet_rgcme_mean']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['shet_rgcme_mean']], na.rm = TRUE))
    #   updateSliderInput(session, "shet_post_mean_filter",
    #                     min = min(user_chosen_table_data[['shet_post_mean']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['shet_post_mean']], na.rm = TRUE))
    #   updateSliderInput(session, "domino_filter",
    #                     min = min(user_chosen_table_data[['domino']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['domino']], na.rm = TRUE))
    #   updateSliderInput(session, "scones_filter",
    #                     min = min(user_chosen_table_data[['scones']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['scones']], na.rm = TRUE))
    #   updateSliderInput(session, "mean_am_pathogenicity_filter",
    #                     min = min(user_chosen_table_data[['mean_am_pathogenicity']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['mean_am_pathogenicity']], na.rm = TRUE))
    # })
    # 
    # # for updating slider values
    # observeEvent(input$apply_filters, {
    #   
    #   user_chosen_table_data <- filtered_table()
    #   
    #   # Update gene id pciker inputs
    #   updatePickerInput(session, "gene_symbol_filter", choices = user_chosen_table_data[['gene_symbol']], selected = user_chosen_table_data[['gene_symbol']])
    #   updatePickerInput(session, "omim_gene_id_filter", choices = user_chosen_table_data[['omim_gene_id']], selected = user_chosen_table_data[['omim_gene_id']])
    #   updatePickerInput(session, "mgi_id_filter", choices = user_chosen_table_data[['mgi_id']], selected = user_chosen_table_data[['mgi_id']])
    #   
    #   updatePickerInput(session, "mgi_viability_filter", choices = unique(user_chosen_table_data[['mgi_viability']]), selected = user_chosen_table_data[['mgi_viability']])
    #   updatePickerInput(session, "impc_viability_filter", choices = unique(user_chosen_table_data[['impc_viability']]), selected = user_chosen_table_data[['impc_viability']])
    #   updatePickerInput(session, "impc_phenotypes_homozygote_filter", choices = unique(user_chosen_table_data[['impc_phenotypes_homozygote']]), selected = user_chosen_table_data[['impc_phenotypes_homozygote']])
    #   updatePickerInput(session, "impc_phenotypes_heterozygote_filter", choices = unique(user_chosen_table_data[['impc_phenotypes_heterozygote']]), selected = user_chosen_table_data[['impc_phenotypes_heterozygote']])
    #   updatePickerInput(session, "impc_phenotypes_hemizygote_filter", choices = unique(user_chosen_table_data[['impc_phenotypes_hemizygote']]), selected = user_chosen_table_data[['impc_phenotypes_hemizygote']])
    #   
    #   # Update cell line constraint metrics sliders
    #   updateSliderInput(session, "depmap_filter",
    #                     min = min(user_chosen_table_data[['depmap_mean_score_all']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['depmap_mean_score_all']], na.rm = TRUE))
    #   updateSliderInput(session, "mef_filter",
    #                     min = min(user_chosen_table_data[['bf_mef']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['bf_mef']], na.rm = TRUE))
    #   updateSliderInput(session, "lam_filter",
    #                     min = min(user_chosen_table_data[['bf_lam']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['bf_lam']], na.rm = TRUE))
    #   
    #   # Update sequencing constraint metrics sliders
    #   updateSliderInput(session, "gnomad_lof_oe_filter",
    #                     min = min(user_chosen_table_data[['gnomad_lof_oe']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['gnomad_lof_oe']], na.rm = TRUE))
    #   updateSliderInput(session, "gnomad_mis_oe_filter",
    #                     min = min(user_chosen_table_data[['gnomad_mis_oe']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['gnomad_mis_oe']], na.rm = TRUE))
    #   updateSliderInput(session, "shet_rgcme_mean_filter",
    #                     min = min(user_chosen_table_data[['shet_rgcme_mean']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['shet_rgcme_mean']], na.rm = TRUE))
    #   updateSliderInput(session, "shet_post_mean_filter",
    #                     min = min(user_chosen_table_data[['shet_post_mean']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['shet_post_mean']], na.rm = TRUE))
    #   updateSliderInput(session, "domino_filter",
    #                     min = min(user_chosen_table_data[['domino']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['domino']], na.rm = TRUE))
    #   updateSliderInput(session, "scones_filter",
    #                     min = min(user_chosen_table_data[['scones']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['scones']], na.rm = TRUE))
    #   updateSliderInput(session, "mean_am_pathogenicity_filter",
    #                     min = min(user_chosen_table_data[['mean_am_pathogenicity']], na.rm = TRUE),
    #                     max = max(user_chosen_table_data[['mean_am_pathogenicity']], na.rm = TRUE))
    #   
    # })
    # 
    # list(
    #   # File input
    #   user_files_upload = reactive(user_files_upload_data()),
    #   file_input = reactive(input$user_files_upload),
    #   # Selected df columns
    #   selected_columns = reactive(input$show_cols),
    #   # Selected gene list
    #   selected_gene_list = reactive(input$selected_gene_list),
    #   # Filter action button
    #   apply_filters = reactive(input$apply_filters),
    #   # Gene ID filters
    #   filter_gene_symbol = reactive(input$gene_symbol_filter),
    #   filter_omim_gene_id = reactive(input$omim_gene_id_filter),
    #   filter_mgi_id = reactive(input$mgi_id_filter),
    #   # DepMap filters
    #   filter_depmap = reactive(input$depmap_filter),
    #   filter_mef = reactive(input$mef_filter),
    #   filter_lam = reactive(input$lam_filter),
    #   # Filtered df
    #   filtered_meta_data_table = reactive(filtered_table())
    # )
    
    list(
      filtered_meta_data_table = reactive(filtered_table()),
      selected_columns = reactive(input$show_cols),
      file_input = reactive(input$user_files_upload),
      user_files_upload = reactive(user_files_upload_data())
      # ,
      # filter = reactive(impc_filter_vals())
    )
  })
}


