## Imports
# Packages
box::use(
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  glue[glue],
  plotly[...],
  dplyr[...],
  stats[...],
  DT[...]
)

# Modules
box::use(
  app/view[visuals_sidebar, visuals_violinplots_sidebar, gene_ontology_sidebar, reactome_sidebar, table_sidebar],
  app/logic/generate_visuals
)

# Rda data
box::use(
  app/logic/import_rda_data,
)

df <- import_rda_data$meta_data_table_data()
#' @export
ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1/2,
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Mouse models data",
      nav_panel(
        "IMPC",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            #visuals_sidebar$ui(ns("sidebar_data_impc"))
            pickerInput(
              ns("select_list_impc"),
              "Select Gene List",
              NULL,
              multiple = TRUE,
              selected = NULL,
              options = pickerOptions(
                actionsBox = TRUE,
                showTick = TRUE,
                width = "300px"
              ),
              inline = FALSE
            ),
            awesomeCheckbox(
              ns("compare_apcg_impc"),
              "Compare to all protein coding genes"
            )
          ),
          # Main content
          plotlyOutput(ns("impc_chart"))
        )
      ),
      nav_panel(
        "MGI",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            pickerInput(
              ns("select_list_mgi"),
              "Select Gene List",
              NULL,
              multiple = TRUE,
              selected = NULL,
              options = pickerOptions(
                actionsBox = TRUE,
                showTick = TRUE,
                width = "300px"
              ),
              inline = FALSE
            ),
            awesomeCheckbox(
              ns("compare_apcg_mgi"),
              "Compare to all protein coding genes"
            )
          ),
          # Main content
          plotlyOutput(ns("mgi_chart"))
        )
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [IMPC](https://www.mousephenotype.org/)"),
        markdown("Learn more about [MGI](https://www.informatics.jax.org/)")
      )
    ),
    # omim sidebar ----
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Disease Data",
      nav_panel(
        "OMIM",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            pickerInput(
              ns("sidebar_data_omim"),
              "Select Gene List",
              NULL,
              multiple = TRUE,
              selected = NULL,
              options = pickerOptions(
                actionsBox = TRUE,
                showTick = TRUE,
                width = "300px"
              ),
              inline = FALSE
            ),
            awesomeCheckbox(
              ns("compare_apcg_omim"),
              "Compare to all protein coding genes"
            )
          ),
          # Main content
          layout_column_wrap(
            width = 1/2,
            plotlyOutput(ns("omim_chart")),
            plotlyOutput(ns("omim_lethality_chart"))
          )
        )
      ),
      # nav_panel(
      #   "DDG2P",
      #   page_sidebar(
      #     # Sidebar
      #     sidebar = sidebar(
      #       width = "340px",
      #       open = c('open'),
      #       visuals_sidebar$ui(ns("visuals_sidebar4"))
      #     ),
      #     # Main content
      #     h3("DDG2P plots")
      #   )
      # ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [OMIM](https://www.omim.org/help/about)")
      )
    ),
    # sequencing sidebar ----
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Gene constraint metrics",
      nav_panel(
        "Sequencing",
        page_sidebar(
          fillable = FALSE,
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            searchInput(
              ns("search_gene_seq"),
              label = "Search Gene Symbol",
              placeholder = "DLX1",
              btnSearch = icon("search"),
              btnReset = icon("remove"),
              width = "100%",
              value = "",
              resetValue = ""
            ),
            pickerInput(
              ns("selected_list_seq"),
              "Select Data Gene List",
              NULL,
              multiple = TRUE,
              selected = NULL,
              options = pickerOptions(
                actionsBox = TRUE,
                showTick = TRUE,
                width = "100%"
              ),
              inline = FALSE
            ),
            checkboxInput(
              ns("toggle_all_data_points_seq"),
              label = "Show all data points",
              width = "100%"
            ),
            awesomeCheckbox(
              ns("compare_apcg_sequencing"),
              "Compare to all protein coding genes"
            )
          ),
          # Main content
          layout_column_wrap(
            width = 1/2,
            plotlyOutput(ns("gnomad_lof"), height = "400px"),
            plotlyOutput(ns("gnomad_mis"), height = "400px"),
            plotlyOutput(ns("shet_rgcme"), height = "400px"),
            plotlyOutput(ns("shet_posterior"), height = "400px"),
            plotlyOutput(ns("domino"), height = "400px"),
            plotlyOutput(ns("scones"), height = "400px"),
            plotlyOutput(ns("alpha_missense"), height = "400px")
          )
        ),
      ),
      nav_panel(
        "Cell Lines",
        page_sidebar(
          fillable = FALSE,
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            searchInput(
              ns("search_gene_cell_lines"),
              label = "Search Gene Symbol",
              placeholder = "DLX1",
              btnSearch = icon("search"),
              btnReset = icon("remove"),
              width = "100%",
              value = "",
              resetValue = ""
            ),
            pickerInput(
              ns("selected_list_cell_lines"),
              "Select Data Gene List",
              NULL,
              multiple = TRUE,
              selected = NULL,
              options = pickerOptions(
                actionsBox = TRUE,
                showTick = TRUE,
                width = "100%"
              ),
              inline = FALSE
            ),
            checkboxInput(
              ns("toggle_all_data_points_cell_lines"),
              label = "Show all data points",
              width = "100%"
            ),
            awesomeCheckbox(
              ns("compare_apcg_cell_lines"),
              "Compare to all protein coding genes"
            )
          ),
          # Main content
          layout_column_wrap(
            width = 1/2,
            plotlyOutput(ns("depmap"), height = "400px"),
            plotlyOutput(ns("bf_mef"), height = "400px"),
            plotlyOutput(ns("bf_lam"), height = "400px")
          )
        ),
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [gnomad_lof & gnomad_mis](https://gnomad.broadinstitute.org/downloads#v4-constraint)")
      )
    ),
    # panther sidebar ----
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Proteins",
      nav_panel(
        "Panther Protein Classes",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            pickerInput(
              ns("sidebar_data_panther_classes"),
              "Select Gene List",
              NULL,
              multiple = TRUE,
              selected = NULL,
              options = pickerOptions(
                actionsBox = TRUE,
                showTick = TRUE,
                width = "300px"
              ),
              inline = FALSE
            ),
            awesomeCheckbox(
              ns("compare_apcg_panther"),
              "Compare to all protein coding genes"
            )
          ),
          # Main content
          plotlyOutput(ns("panther_classes"))
        ),
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [PANTHERTdb](https://www.pantherdb.org/about.jsp)")
      )
    ),
    # upset sidebar ----
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Upset Plot",
      nav_panel(
        "Upset",
        page_sidebar(
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            pickerInput(
              ns("sidebar_data_upset"),
              "Select Gene List",
              NULL,
              multiple = TRUE,
              selected = NULL,
              options = pickerOptions(
                actionsBox = TRUE,
                showTick = TRUE,
                width = "300px"
              ),
              inline = FALSE
            )
          ),
          # Main content
          htmlOutput(ns("upsetPlot"))
        ),
      )
    ),
    # GO semantic similarity sidebar ----
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = "Enrichment and Semantic Similarity Analysis",
      nav_panel(
        "Gene Ontology",
        page_sidebar(
          fillable = FALSE,
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            # NEW SIDEBAR WITHOUT ALL PROTEIN CODING GENES?
            # gene_ontology_sidebar$ui(ns("sidebar_data_go"))
            pickerInput(
              ns("select_gene_list_go"),
              "Select Gene List",
              NULL,
              multiple = FALSE,
              selected = NULL,
              options = pickerOptions(
                actionsBox = TRUE,
                showTick = TRUE,
                width = "300px"
              ),
              inline = FALSE
            ),
            pickerInput(
              ns("select_ontology"),
              "Select GO Ontology",
              c("BP", "MF", "CC"),
              multiple = FALSE,
              selected = "BP",
              options = pickerOptions(
                actionsBox = TRUE,
                showTick = TRUE,
                width = "300px"
              ),
              inline = FALSE
            ),
            actionButton(
              ns("get_enriched_terms_go"),
              "Get enriched terms"
            ),
            awesomeCheckbox(
              ns("show_legend"),
              "Show plot legend",
              value = TRUE
            )
          ),
          # Main content
          #h5("Semantic Similarity Analysis of Enriched Terms"),
          plotlyOutput(ns("go_semantic_similarity_plot"), height = "500px"),
          #h5("Top 10 Enriched Terms"),
          DTOutput(ns("enriched_go_terms"))

        ),
      ),
      nav_panel(
        "Reactome",
        page_sidebar(
          fillable = FALSE,
          # Sidebar
          sidebar = sidebar(
            width = "340px",
            open = c('open'),
            reactome_sidebar$ui(ns("sidebar_data_reactome"))
          ),
          # Main content
          layout_column_wrap(
            width = 1,
            plotOutput(ns("reactome_enrichment_plot")),
            DTOutput(ns("top_enriched_reactome_terms"))
          )
        ),
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown("Learn more about [gnomad_lof & gnomad_mis](https://gnomad.broadinstitute.org/downloads#v4-constraint)")
      )
    )
  )
}

#' @export
server <- function(id, sidebar_data) {
  moduleServer(id, function(input, output, session) {

    # Get sidebar inputs
    # sidebar_input_impc_barchart <- visuals_sidebar$server("sidebar_data_impc")
    # sidebar_input_mgi_barchart <- visuals_sidebar$server("sidebar_data_mgi")
    # sidebar_input_omim_barchart <- visuals_sidebar$server("sidebar_data_omim")
    # sidebar_input_panther_classes_barchart <- visuals_sidebar$server("sidebar_data_panther_classes")
    # sidebar_input_upset <- visuals_sidebar$server("sidebar_data_upset")
    sidebar_input_go <- gene_ontology_sidebar$server("sidebar_data_go")
    sidebar_input_reactome <- reactome_sidebar$server("sidebar_data_reactome")
    sidebar_input_sequencing_violinplots <- visuals_violinplots_sidebar$server("sidebar_data_sequencing")
    sidebar_input_cell_lines_violinplots <- visuals_violinplots_sidebar$server("sidebar_data_cell_lines")

    # Get gene list data and meta data
    #gene_list_data <- import_rda_data$morphic_gene_list_data()
    meta_data <- import_rda_data$visuals_data()
    constraint_metrics_plot_data <- import_rda_data$constraint_metrics()
    go_scatter_plots <- import_rda_data$go_scatter_plots()
    go_top_enriched_terms_tables <- import_rda_data$go_top_enriched_terms_tables()
    reactome_enrichment_plots <- import_rda_data$reactome_enrichment_plots()
    reactome_enrichment_tables <- import_rda_data$reactome_enrichment_tables()

    #input_compare_apcg_impc <- reactive({input$compare_apcg_impc})
    # update sidebar choices
    # mouse plots ----
    observe({
      # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
      req(!is.null(sidebar_data$file_input()))
      updatePickerInput(session, "select_list_impc", choices = names(sidebar_data$user_files_upload()), selected = names(sidebar_data$user_files_upload()))
      #input_compare_apcg_impc_val(input_compare_apcg_impc())
      # print(input_compare_apcg_impc_val())
      # updateAwesomeCheckbox(session, "compare_apcg_impc", value = input_compare_apcg_impc_val())

    })

    impc_data <- reactiveVal({NULL})

    observe({
      gene_list_data <- sidebar_data$user_files_upload()
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect2(input$select_list_impc, gene_list_data)
      df <- import_rda_data$meta_data_table_data()
      if (input$compare_apcg_impc == TRUE) {
        gene_lists_for_plots <- append(gene_lists_for_plots, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      impc_data(gene_lists_for_plots)
      #print(impc_data())
    })

    output$impc_chart <- renderPlotly({
      req(!is.null(input$select_list_impc))
      # gene_list_data <- sidebar_data$user_files_upload()
      # gene_lists_for_plots <- generate_visuals$getDataFromUserSelect2(input$select_list_impc, gene_list_data)
      # gene_lists_for_plots_impc(gene_lists_for_plots)
      #generate_visuals$generateImpcBarchart(gene_lists_for_plots, meta_data)
      generate_visuals$generateImpcBarchart(impc_data(), meta_data, input$compare_apcg_impc)
    }) %>%
      bindCache(input$select_list_impc,
                input$compare_apcg_impc)

    # update sidebar choices
    observe({
      # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
      req(!is.null(sidebar_data$file_input()))
      updatePickerInput(session, "select_list_mgi", choices = names(sidebar_data$user_files_upload()), selected = names(sidebar_data$user_files_upload()))
    })

    output$mgi_chart <- renderPlotly({
      req(!is.null(input$select_list_impc))
      gene_list_data <- sidebar_data$user_files_upload()
      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect2(input$select_list_impc, gene_list_data)
      if (input$compare_apcg_mgi == TRUE) {
        gene_lists_for_plots <- append(gene_lists_for_plots, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      generate_visuals$generateMgiBarchart(gene_lists_for_plots, meta_data)

    }) %>%
      bindCache(input$select_list_impc,
                input$compare_apcg_mgi)

    # omim plots ----
    # update sidebar choices
    observe({
      # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
      req(!is.null(sidebar_data$file_input()))
      updatePickerInput(session, "sidebar_data_omim", choices = names(sidebar_data$user_files_upload()), selected = names(sidebar_data$user_files_upload()))
    })

    output$omim_chart <- renderPlotly({
      req(!is.null(input$sidebar_data_omim))
      gene_list_data <- sidebar_data$user_files_upload()
      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect2(input$sidebar_data_omim, gene_list_data)
      if (input$compare_apcg_omim == TRUE) {
        gene_lists_for_plots <- append(gene_lists_for_plots, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      generate_visuals$generateHasOmimPlot(gene_lists_for_plots, meta_data)

    }) %>%
      bindCache(input$sidebar_data_omim,
                input$compare_apcg_omim)

    output$omim_lethality_chart <- renderPlotly({
      req(!is.null(input$sidebar_data_omim))
      gene_list_data <- sidebar_data$user_files_upload()
      # Generate plot data frames
      gene_lists_for_plots <- generate_visuals$getDataFromUserSelect2(input$sidebar_data_omim, gene_list_data)
      if (input$compare_apcg_omim == TRUE) {
        gene_lists_for_plots <- append(gene_lists_for_plots, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      generate_visuals$generateOmimLethalityPlot(gene_lists_for_plots, meta_data)

    }) %>%
      bindCache(input$sidebar_data_omim,
                input$compare_apcg_omim)
    #
    #  Reactive variables for Violin plot options
    # Sequencing plots ---
    toggle_data_points_sequencing <- reactive({
      input$search_gene_seq
    })

    gene_list_selection_sequencing <- reactive({
      input$selected_list_seq
    })

    highlighted_genes_sequencing <- reactive({
      input$toggle_all_data_points_seq
    })
    #
    # # Cell lines ---
    toggle_data_points_cell_lines <- reactive({
      input$search_gene_cell_lines
    })

    gene_list_selection_cell_lines <- reactive({
      input$selected_list_cell_lines
    })

    highlighted_genes_cell_lines <- reactive({
      input$toggle_all_data_points_cell_lines
    })

    observe({
      # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
      req(!is.null(sidebar_data$file_input()))
      updatePickerInput(session, "selected_list_seq", choices = names(sidebar_data$user_files_upload()), selected = names(sidebar_data$user_files_upload()))
    })

    # Sequencing plots ----
    output$gnomad_lof <- renderPlotly({
      req(!is.null(input$selected_list_seq))
      column <- 'lof_oe'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_seq, gene_list_data)
      if (input$compare_apcg_sequencing == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_seq, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, 0.35, input$toggle_all_data_points_seq)

    }) %>%
      bindCache(
        c(
          input$selected_list_seq,
          input$search_gene_seq,
          input$toggle_all_data_points_seq,
          input$compare_apcg_sequencing
          )
        )

    output$gnomad_mis <- renderPlotly({
      req(!is.null(input$selected_list_seq))
      column <- 'mis_oe'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_seq, gene_list_data)
      if (input$compare_apcg_sequencing == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_seq, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, input$toggle_all_data_points_seq)

    }) %>%
      bindCache(
        c(
          input$selected_list_seq,
          input$search_gene_seq,
          input$toggle_all_data_points_seq,
          input$compare_apcg_sequencing
        )
      )
    #
    # output$gnomad_mis <- renderPlotly({
    #   req(!is.null(gene_list_selection_sequencing()))
    #   data <- constraint_metrics_plot_data
    #   column <- 'mis_oe'
    #   gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
    #   plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
    #   genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))
    #
    #   generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_sequencing())
    #
    # }) %>%
    #   bindCache(
    #     c(
    #       gene_list_selection_sequencing(),
    #       highlighted_genes_sequencing(),
    #       toggle_data_points_sequencing()
    #     )
    #   )
    #
    output$shet_rgcme <- renderPlotly({
      req(!is.null(input$selected_list_seq))
      column <- 'mean'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_seq, gene_list_data)
      if (input$compare_apcg_sequencing == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_seq, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, 0.075, input$toggle_all_data_points_seq)

    }) %>%
      bindCache(
        c(
          input$selected_list_seq,
          input$search_gene_seq,
          input$toggle_all_data_points_seq,
          input$compare_apcg_sequencing
        )
      )
    # output$shet_rgcme <- renderPlotly({
    #   req(!is.null(gene_list_selection_sequencing()))
    #   data <- constraint_metrics_plot_data
    #   column <- 'mean'
    #   gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
    #   plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
    #   genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))
    #
    #   generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, 0.075, toggle_data_points_sequencing())
    #
    # }) %>%
    #   bindCache(
    #     c(
    #       gene_list_selection_sequencing(),
    #       highlighted_genes_sequencing(),
    #       toggle_data_points_sequencing()
    #     )
    #   )
    #
    output$shet_posterior <- renderPlotly({
      req(!is.null(input$selected_list_seq))
      column <- 'post_mean'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_seq, gene_list_data)
      if (input$compare_apcg_sequencing == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_seq, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, 0.1, input$toggle_all_data_points_seq)

    }) %>%
      bindCache(
        c(
          input$selected_list_seq,
          input$search_gene_seq,
          input$toggle_all_data_points_seq,
          input$compare_apcg_sequencing
        )
      )
    # output$shet_posterior <- renderPlotly({
    #   req(!is.null(gene_list_selection_sequencing()))
    #   data <- constraint_metrics_plot_data
    #   column <- 'post_mean'
    #   gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
    #   plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
    #   genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))
    #
    #   generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, 0.1, toggle_data_points_sequencing())
    #
    # }) %>%
    #   bindCache(
    #     c(
    #       gene_list_selection_sequencing(),
    #       highlighted_genes_sequencing(),
    #       toggle_data_points_sequencing()
    #     )
    #   )
    #
    output$domino <- renderPlotly({
      req(!is.null(input$selected_list_seq))
      column <- 'DOMINO'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_seq, gene_list_data)
      if (input$compare_apcg_sequencing == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_seq, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, input$toggle_all_data_points_seq)

    }) %>%
      bindCache(
        c(
          input$selected_list_seq,
          input$search_gene_seq,
          input$toggle_all_data_points_seq,
          input$compare_apcg_sequencing
        )
      )
    # output$domino <- renderPlotly({
    #   req(!is.null(gene_list_selection_sequencing()))
    #   data <- constraint_metrics_plot_data
    #   column <- 'DOMINO'
    #   gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
    #   plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
    #   genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))
    #
    #   generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_sequencing())
    #
    # }) %>%
    #   bindCache(
    #     c(
    #       gene_list_selection_sequencing(),
    #       highlighted_genes_sequencing(),
    #       toggle_data_points_sequencing()
    #     )
    #   )
    #
    output$scones <- renderPlotly({
      req(!is.null(input$selected_list_seq))
      column <- 'SCoNeS'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_seq, gene_list_data)
      if (input$compare_apcg_sequencing == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_seq, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, input$toggle_all_data_points_seq)

    }) %>%
      bindCache(
        c(
          input$selected_list_seq,
          input$search_gene_seq,
          input$toggle_all_data_points_seq,
          input$compare_apcg_sequencing
        )
      )
    # output$scones <- renderPlotly({
    #   req(!is.null(gene_list_selection_sequencing()))
    #   data <- constraint_metrics_plot_data
    #   column <- 'SCoNeS'
    #   gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
    #   plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
    #   genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))
    #
    #   generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_sequencing())
    #
    # }) %>%
    #   bindCache(
    #     c(
    #       gene_list_selection_sequencing(),
    #       highlighted_genes_sequencing(),
    #       toggle_data_points_sequencing()
    #     )
    #   )
    #
    output$alpha_missense <- renderPlotly({
      req(!is.null(input$selected_list_seq))
      column <- 'mean_am_pathogenicity'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_seq, gene_list_data)
      if (input$compare_apcg_sequencing == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_seq, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, input$toggle_all_data_points_seq)

    }) %>%
      bindCache(
        c(
          input$selected_list_seq,
          input$search_gene_seq,
          input$toggle_all_data_points_seq,
          input$compare_apcg_sequencing
        )
      )
    # output$alpha_missense <- renderPlotly({
    #   req(!is.null(gene_list_selection_sequencing()))
    #   data <- constraint_metrics_plot_data
    #   column <- 'mean_am_pathogenicity'
    #   gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_sequencing(), gene_list_data)
    #   plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
    #   genes_to_highlight <- unlist(strsplit(highlighted_genes_sequencing(), ","))
    #
    #   generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_sequencing())
    #
    # }) %>%
    #   bindCache(
    #     c(
    #       gene_list_selection_sequencing(),
    #       highlighted_genes_sequencing(),
    #       toggle_data_points_sequencing()
    #     )
    #   )
    #
    # # Cell line metrics ----
    observe({
      # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
      req(!is.null(sidebar_data$file_input()))
      updatePickerInput(session, "selected_list_cell_lines", choices = names(sidebar_data$user_files_upload()), selected = names(sidebar_data$user_files_upload()))
    })

    # Sequencing plots ----
    output$depmap <- renderPlotly({
      req(!is.null(input$selected_list_cell_lines))
      column <- 'mean_score_all'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_cell_lines, gene_list_data)
      if (input$compare_apcg_cell_lines == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_cell_lines, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, input$toggle_all_data_points_cell_lines)

    }) %>%
      bindCache(
        c(
          input$selected_list_cell_lines,
          input$search_gene_cell_lines,
          input$toggle_all_data_points_cell_lines,
          input$compare_apcg_cell_lines
        )
      )
    # output$depmap <- renderPlotly({
    #   req(!is.null(gene_list_selection_cell_lines()))
    #   data <- constraint_metrics_plot_data
    #   column <- 'mean_score_all'
    #   gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_cell_lines(), gene_list_data)
    #   plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
    #   genes_to_highlight <- unlist(strsplit(highlighted_genes_cell_lines(), ","))
    #
    #   generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_cell_lines())
    #
    # }) %>%
    #   bindCache(
    #     c(
    #       gene_list_selection_cell_lines(),
    #       highlighted_genes_cell_lines(),
    #       toggle_data_points_cell_lines()
    #     )
    #   )
    #
    output$bf_mef <- renderPlotly({
      req(!is.null(input$selected_list_cell_lines))
      column <- 'bf_mef'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_cell_lines, gene_list_data)
      if (input$compare_apcg_cell_lines == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_cell_lines, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, input$toggle_all_data_points_cell_lines)

    }) %>%
      bindCache(
        c(
          input$selected_list_cell_lines,
          input$search_gene_cell_lines,
          input$toggle_all_data_points_cell_lines,
          input$compare_apcg_cell_lines
        )
      )
    # output$bf_mef <- renderPlotly({
    #   req(!is.null(gene_list_selection_cell_lines()))
    #   data <- constraint_metrics_plot_data
    #   column <- 'bf_mef'
    #   gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_cell_lines(), gene_list_data)
    #   plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
    #   genes_to_highlight <- unlist(strsplit(highlighted_genes_cell_lines(), ","))
    #
    #   generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_cell_lines())
    #
    # }) %>%
    #   bindCache(
    #     c(
    #       gene_list_selection_cell_lines(),
    #       highlighted_genes_cell_lines(),
    #       toggle_data_points_cell_lines()
    #     )
    #   )
    #
    output$bf_lam <- renderPlotly({
      req(!is.null(input$selected_list_cell_lines))
      column <- 'bf_lam'
      gene_list_data <- sidebar_data$user_files_upload()
      data <- constraint_metrics_plot_data
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$selected_list_cell_lines, gene_list_data)
      if (input$compare_apcg_cell_lines == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      #print(gene_lists)
      plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
      genes_to_highlight <- unlist(strsplit(input$search_gene_cell_lines, ","))

      generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, input$toggle_all_data_points_cell_lines)

    }) %>%
      bindCache(
        c(
          input$selected_list_cell_lines,
          input$search_gene_cell_lines,
          input$toggle_all_data_points_cell_lines,
          input$compare_apcg_cell_lines
        )
      )
    # output$bf_lam <- renderPlotly({
    #   req(!is.null(gene_list_selection_cell_lines()))
    #   data <- constraint_metrics_plot_data
    #   column <- 'bf_lam'
    #   gene_lists <- generate_visuals$getDataFromUserSelect(gene_list_selection_cell_lines(), gene_list_data)
    #   plot_data <- generate_visuals$getViolinPlotData(data, column, gene_lists)
    #   genes_to_highlight <- unlist(strsplit(highlighted_genes_cell_lines(), ","))
    #
    #   generate_visuals$renderViolinPlot(plot_data, column, genes_to_highlight, NULL, toggle_data_points_cell_lines())
    #
    # }) %>%
    #   bindCache(
    #     c(
    #       gene_list_selection_cell_lines(),
    #       highlighted_genes_cell_lines(),
    #       toggle_data_points_cell_lines()
    #     )
    #   )
    #

    # panther plots ----
    observe({
      # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
      req(!is.null(sidebar_data$file_input()))
      updatePickerInput(session, "sidebar_data_panther_classes", choices = names(sidebar_data$user_files_upload()), selected = names(sidebar_data$user_files_upload()))
    })

    output$panther_classes <- renderPlotly({
      req(!is.null(input$sidebar_data_panther_classes))
      gene_list_data <- sidebar_data$user_files_upload()

      gene_lists <- generate_visuals$getDataFromUserSelect2(input$sidebar_data_panther_classes, gene_list_data)
      if (input$compare_apcg_cell_lines == TRUE) {
        gene_lists <- append(gene_lists, list(all_protein_coding_genes = df[["gene_symbol"]]))
      }
      generate_visuals$getPantherPlots(meta_data, gene_lists)
    }) %>%
      bindCache(
        input$sidebar_data_panther_classes,
        input$compare_apcg_panther
      )



    # upset plot ----
    observe({
      # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
      req(!is.null(sidebar_data$file_input()))
      updatePickerInput(session, "sidebar_data_upset", choices = names(sidebar_data$user_files_upload()), selected = names(sidebar_data$user_files_upload()))
    })

    output$upsetPlot <- renderUI({
      req(!is.null(input$sidebar_data_upset))
      gene_list_data <- sidebar_data$user_files_upload()
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$sidebar_data_upset, gene_list_data)
      htmltools::browsable(generate_visuals$generateUpsetR(gene_lists))
    }) %>%
      bindCache(
        input$sidebar_data_upset
      )
    #
    # output$go_semantic_similarity_plot <- renderPlotly({
    #   plot <- generate_visuals$renderGoScatterPlot(go_scatter_plots, sidebar_input_go$ontology(), sidebar_input_go$dpc_selected(), sidebar_input_go$show_legend())
    #   ggplotly(plot)
    # }) %>%
    #   bindCache(
    #     c(
    #       sidebar_input_go$ontology(),
    #       sidebar_input_go$dpc_selected(),
    #       sidebar_input_go$show_legend()
    #     )
    #   )
    #
    # # How to go about this - only single choice? instead of multiple options
    # output$top_enriched_go_terms <- renderDT({
    #   table <- generate_visuals$renderGoTable(go_top_enriched_terms_tables, sidebar_input_go$ontology(), sidebar_input_go$dpc_selected())
    #
    #   datatable(
    #     table,
    #     options = list(
    #       searching = FALSE,  # Remove search bar
    #       lengthChange = FALSE,  # Remove number of results per page
    #       paging = FALSE  # Remove pagination controls
    #     )
    #   )
    #
    # })

    # GO semantic analysis ----
    observe({
      # Update choices for selected_gene_list based on the names of lists in user_files_upload_data
      req(!is.null(sidebar_data$file_input()))
      updatePickerInput(session, "select_gene_list_go", choices = names(sidebar_data$user_files_upload()), selected = names(sidebar_data$user_files_upload())[1])
    })

    go_enriched_terms <- reactiveVal({NULL})

    observeEvent(input$get_enriched_terms_go, {
      gene_list_data <- sidebar_data$user_files_upload()
      gene_lists <- generate_visuals$getDataFromUserSelect2(input$sidebar_data_upset, gene_list_data)
      gene_list <- gene_lists[[input$select_gene_list_go]]
      background <- df
      ontology <- input$select_ontology
      enriched_terms <- generate_visuals$getEnrichedGoTerms(
        gene_list, background, ontology
      )
      go_enriched_terms(enriched_terms)
    })

    # to do
    # ADD BINDCACHE
    output$enriched_go_terms <- renderDT({
      # datatable(
      #   data.frame(go_enriched_terms(), row.names = NULL) %>%
      #     select(ID, Description) %>%
      #     rename(GO_ID = ID)
      # )
      x <-  data.frame(go_enriched_terms(), row.names = NULL) %>%
        select(ID, Description, qvalue) %>%
        rename(GO_ID = ID)
      datatable(x)
    })

    # to do
    # ADD BINDCACHE + GENE LIST & ONTOLOGY OPTIONS SELECTION
    output$go_semantic_similarity_plot <- renderPlotly({
      generate_visuals$generateGoSemanticSimilarityPlot(go_enriched_terms())
    })


    #
    # output$reactome_enrichment_plot <- renderPlot({
    #   generate_visuals$renderReactomeEnrichmentPlot(reactome_enrichment_plots, sidebar_input_reactome$dpc_selected_reactome())
    # }) %>%
    #   bindCache(
    #     sidebar_input_reactome$dpc_selected_reactome()
    #   )
    #
    #
    # output$top_enriched_reactome_terms <- renderDT({
    #   table <- generate_visuals$renderReactomeTable(reactome_enrichment_tables, sidebar_input_reactome$dpc_selected_reactome())
    #
    #   datatable(
    #     table,
    #     options = list(
    #       searching = FALSE,  # Remove search bar
    #       lengthChange = FALSE,  # Remove number of results per page
    #       paging = FALSE  # Remove pagination controls
    #     )
    #   )
    #   })


  })
}
