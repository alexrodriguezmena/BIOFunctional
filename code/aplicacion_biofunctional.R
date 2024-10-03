# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = div(
    style = "font-size: 14px; font-weight: bold; font-family: Georgia;",
    "BIOFunctional"
  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HOME", tabName = "inicio", icon = icon("home")),
      menuItem("KEGG", tabName = "kegg", icon = icon("chart-bar"),
               menuSubItem("Filter", tabName = "opcion3_kegg"),
               menuSubItem("Functional Analysis", tabName = "opcion1_kegg"),
               menuSubItem("Network Analysis", tabName = "opcion2_kegg"),
               menuSubItem("HeatMap",tabName = "Heatmap"),
               menuSubItem("Volcano",tabName = "Volcano")
      ),
      menuItem("Gene Ontologies", tabName = "gene_ontologies", icon = icon("dna"),
               menuSubItem("Filter", tabName = "opcion3_gene_ontologies"),
               menuSubItem("Functional Analysis", tabName = "opcion1_gene_ontologies"),
               menuSubItem("Network Analysis", tabName = "opcion2_gene_ontologies"),
               menuSubItem("HeatMap",tabName = "Heatmap_go"),
               menuSubItem("Volcano",tabName = "Volcano_go")
      ),
      menuItem("HELP", tabName = "contact", icon = icon("clipboard-user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "inicio",
              fluidRow(
                box(
                  title = "A comprehensive software for interpretation and visualization of functional analysis of Gene Ontologies and KEGG Pathways",
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  style = "font-size: 14px; font-family: Times New Roman;",
                  p("A comprehensive app designed for the interpretation and visualization of the functional analysis related to KEGG pathways and gene ontologies gives researchers and specialists a tool to get detailed functional information about their data, specifically going deep into biological pathways and gene functions information. By using a variety of techniques and libraries, such as Shiny, htrr, dplyr, tibble, and rvest, we have developed an application that provides a well-designed user-oriented interface with all the facilities to assess their data and start analyzing it directly from scratch through a few steps."),
                  br(),
                  p("The software allows an exhaustive exploration of KEGG pathways and Gene Ontologies, facilitating the analysis of complex biological processes.To achieve this, functions described in the scripts integrate data manipulation methods and web scraping techniques to extract the necessary information from online official databases, Kyoto Encyclopedia of Genes and Genomes (KEGG) and QuickGo. Furthermore, those functions are computed by parallel processing, resulting in efficient petitions to the database servers and allowing the user to get quick results from a large dataset."),
                  br(),
                  p("A fundamental feature in the app is the capability, by the techniques explained above, to obtain ancestral information for KEGG pathways and gene ontologies, making it easier to understand their hierarchy and how each of the samples in a dataset is classified through it.This offers users a way to study the dataset at different levels of taxonomy directly from the raw data.Additionally, the ability to create interactive networks is implemented, aiming to represent all the experimental data to see the relationships between the groups and the ontologies, without disregarding the classification created. This is the main tool to understand the meaning of the relations that will be seen around the system displayed."),
                  br(),
                  p("As a result of all these attributes, the software represents a key tool for the analyst involved in the study of biological pathways, providing an intuitive interface with advanced data processing techniques, allowing researchers to puzzle out the intricacy of the biological functions and obtain insights into the relationships between genes or molecular components.")
                  ),
                div(
                  img(src = "https://biysc.org/sites/default/files/ub_facultat_biologia.png", height = 300, width = 600)
                )
              )
      ),
      tabItem(tabName = "kegg",
              fluidRow(
                box(title = "KEGG",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary"
                )
              )
      ),
      tabItem(tabName = "gene_ontologies",
              fluidRow(
                box(title = "Gene Ontologies",
                    width = 12,
                    solidHeader = TRUE,
                    status = "warning"
                )
              )
      ),
      tabItem(tabName = "contact",
              fluidRow(
                box(title = "Contact Information", width = 12, status = "info",
                    "Developed by Alejandro Rodríguez & Antonio Monleón. Section of Statistics. Department of Genetics, Microbiology and Statistics. UB. For any inquiries or support, please contact us at:",
                    br(),
                    br(),
                    "Alex: alejandro.rodriguez@alum.esci.upf.edu",
                    br(),
                    "Toni: amonleong@ub.edu"
                )
              )
      ),
      tabItem(tabName = "opcion3_kegg",
              fluidRow(
                box(title = "Filter",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_filter_kegg", "Upload File"),
                    uiOutput("column_selectors"),
                    textInput("diseases", "Enter Diseases (comma-separated):", value = ""),
                    downloadButton("downloadFile_filter_kegg", "Download Processed File")
                ),
                box(
                  withSpinner(
                    dataTableOutput("table_filter_kegg")
                  ),
                  br(),
                  actionButton("clear_filter_kegg", "Clear Data", icon = icon("trash"))
                )
              )
      ),
      tabItem(tabName = "opcion1_kegg",
              fluidRow(
                box(title = "Functional Analysis",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_kegg", "Upload File"),
                    downloadButton("downloadFile_kegg", "Download Processed File"),
                    br(),
                    br(),
                    br(),
                    withSpinner(
                      dataTableOutput("table_kegg")
                    ),
                    br(),
                    actionButton("clear_kegg", "Clear Data", icon = icon("trash"))
                )
              )
      ),
      tabItem(tabName = "opcion2_kegg",
              fluidRow(
                box(title = "Network Analysis Filters", width = 3, status = "danger",
                    fileInput("file_kegg_network", "Upload File"),
                    selectInput("sample_kegg", "Sample", choices = NULL, multiple = FALSE),
                    selectInput("disease_kegg", "Disease", choices = NULL, multiple = FALSE),                    
                    selectInput("group_kegg", "Group:", choices = NULL, multiple = FALSE),
                    selectInput("group_by_kegg", "Group by:", choices = NULL, multiple = FALSE),
                    downloadButton("download_network_kegg", "Download Network"),
                    br(),
                    actionButton("clear_kegg_network", "Clear Data", icon = icon("trash")),
                    uiOutput("legend")
                ),
                box(title = "Network", width = 9, status = "danger",
                    withSpinner(visNetworkOutput("network_kegg", width = "100%", height = "800px")),
                    box(title = tagList("AI conclusions prompt", icon("question-circle", id = "helpIcon")), width = 12, status = "info",
                        textOutput("text_kegg"),
                        br(),
                        downloadButton("download_prompt_kegg", "Download Prompt")
                    )
                )
              )
      ),
      tabItem(tabName = "opcion3_gene_ontologies",
              fluidRow(
                box(title = "Filter",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_filter_gene_ontologies", "Upload File"),
                    uiOutput("column_selectors_go"),
                    textInput("diseases_go", "Enter Diseases (comma-separated):", value = ""),
                    downloadButton("downloadFile_filter_go", "Download Processed File")
                ),
                box(
                  withSpinner(
                    dataTableOutput("table_filter_go")
                  ),
                  br(),
                  actionButton("clear_filter_go", "Clear Data", icon = icon("trash"))
                )
              )
      ),
      tabItem(tabName = "opcion1_gene_ontologies",
              fluidRow(
                box(title = "Functional Analysis",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_gene_ontologies", "Upload File"),
                    downloadButton("downloadFile_gene_ontologies", "Download Processed File"),
                    br(),
                    br(),
                    br(),
                    withSpinner(
                      dataTableOutput("table_gene_ontologies")
                    ),
                    br(),
                    actionButton("clear_go", "Clear Data", icon = icon("trash"))
                )
              )
      ),
      tabItem(tabName = "Heatmap",
              fluidRow(
                box(title = "Heatmap Analysis",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_heatmap", "Upload KEGG Data"),
                    selectInput("sample_heatmap", "Sample", choices = NULL),
                    selectInput("disease_heatmap", "Disease", choices = NULL),
                    selectInput("group_by_heatmap", "Group By:", choices = c("Domain", "Subdomain", "Relation")),
                    selectInput("ontology_heatmap", "Ontology", choices = NULL),
                    downloadButton("download_heatmap", "Download Heatmap")
                ),
                box(title = "Heatmap Visualization",
                    width = 12,
                    solidHeader = TRUE,
                    status = "info",
                    withSpinner(plotlyOutput("heatmap_plot", height = "600px"))
                )
              )
      ),
      tabItem(tabName = "Heatmap_go",
              fluidRow(
                box(title = "Heatmap Analysis",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_heatmap_go", "Upload KEGG Data"),
                    selectInput("sample_heatmap_go", "Sample", choices = NULL),
                    selectInput("disease_heatmap_go", "Disease", choices = NULL),
                    selectInput("ont_description_heatmap_go", "Ontology Description", choices = NULL),  # Nuevo filtro
                    downloadButton("download_heatmap_go", "Download Heatmap")
                ),
                box(title = "Heatmap Visualization",
                    width = 12,
                    solidHeader = TRUE,
                    status = "info",
                    withSpinner(plotlyOutput("heatmap_plot_go", height = "600px"))
                )
              )
      ),
      tabItem(tabName = "Volcano",
              fluidRow(
                box(title = "Volcano Analysis",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_volcano", "Upload KEGG Data"),
                    selectInput("sample_volcano", "Sample", choices = NULL),
                    selectInput("disease_volcano", "Disease", choices = NULL),
                    selectInput("group_by_volcano", "Group By:", choices = c("Domain", "Subdomain", "Relation")),
                    selectInput("ontology_volcano", "Ontology", choices = NULL),
                    downloadButton("download_volcano", "Download Volcano")
                ),
                box(title = "Volcano Visualization",
                    width = 12,
                    solidHeader = TRUE,
                    status = "info",
                    withSpinner(plotlyOutput("volcano_plot", height = "600px"))
                )
              )
      ),
      tabItem(tabName = "Volcano_go",
              fluidRow(
                box(title = "Volcano Analysis",
                    width = 12,
                    solidHeader = TRUE,
                    status = "success",
                    fileInput("file_volcano_go", "Upload KEGG Data"),
                    selectInput("sample_volcano_go", "Sample", choices = NULL),
                    selectInput("disease_volcano_go", "Disease", choices = NULL),
                    selectInput("ont_description_volcano_go", "Ontology Description", choices = NULL),  # Nuevo filtro
                    downloadButton("download_volcano_go", "Download Volcano")
                ),
                box(title = "Volcano Visualization",
                    width = 12,
                    solidHeader = TRUE,
                    status = "info",
                    withSpinner(plotlyOutput("volcano_plot_go", height = "600px"))
                )
              )
      ),
      tabItem(tabName = "opcion2_gene_ontologies",
              fluidRow(
                box(title = "Network Analysis Filters", width = 3, status = "danger",
                    fileInput("file_go_network", "Upload File"),
                    selectInput("sample_go", "Sample", choices = NULL, multiple = FALSE),
                    selectInput("ont_description", "Ontology Description", choices = NULL, multiple = FALSE),
                    selectInput("disease_go", "Disease", choices = NULL, multiple = FALSE),
                    selectInput("group_go", "Group", choices = NULL, multiple = FALSE),
                    downloadButton("download_network_go", "Download Network"),
                    br(),
                    actionButton("clear_go_network", "Clear Data", icon = icon("trash")),
                    uiOutput("legend_go")
                    ),
                box(title = "Network", width = 9, status = "danger",
                    withSpinner(visNetworkOutput("network_go", width = "100%", height = "800px")),
                    box(title = tagList("AI conclusions prompt", icon("question-circle", id = "helpIcon2")), width = 12, status = "info",
                        textOutput("text_go"),
                        br(),
                        downloadButton("download_prompt_go", "Download Prompt")
                    )
                )
              )
      )
    )
  )
)




#Define server logic
server <- function(input, output, session) {
  # Render popovers
  output$helpIcon <- renderUI({
    bsPopover(id = "popover_helpIcon", title = "AI Conclusions Prompt",
              content = "This section provides insights and conclusions drawn by the AI based on the analysis performed. It helps in understanding the significant findings and their implications.",
              placement = "right", trigger = "click")
  })
  
  output$helpIcon2 <- renderUI({
    bsPopover(id = "popover_helpIcon2", title = "AI Conclusions Prompt",
              content = "This section provides insights and conclusions drawn by the AI based on the analysis performed. It helps in understanding the significant findings and their implications.",
              placement = "right", trigger = "click")
  })
  
  # Activate popovers
  observe({
    shinyjs::runjs("
      $('#helpIcon').click(function() {
        $('#popover_helpIcon').popover('toggle');
      });
      
      $('#helpIcon2').click(function() {
        $('#popover_helpIcon2').popover('toggle');
      });
    ")
  })
  
  observeEvent(input$file_filter_kegg, {
    loadedData <- reactive({
      req(input$file_filter_kegg)
      df <- read.csv(input$file_filter_kegg$datapath)
      df$Disease <- NA
      
      if (nchar(input$diseases) > 0) {
        for (disease in unlist(strsplit(input$diseases, ","))) {
          df$Disease <- ifelse(grepl(disease, df$GROUP), disease, df$Disease)
        }
      }
      
      # Asegúrate de incluir la columna ONTOLOGY en el conjunto de datos
      df <- df[, c("ONTOLOGY", colnames(df))]
      df
    })
    
    
    
    # Render column selectors based on loaded data
    output$column_selectors <- renderUI({
      req(loadedData())
      colnames <- colnames(loadedData())
      tagList(
        selectInput("sample", "Sample Column:", choices = colnames),
        selectInput("up_down", "Up/Down Column:", choices = colnames),
        selectInput("ont_description", "Ontology Description Column:", choices = colnames),
        selectInput("ontology_kegg_2", "Ontology Column:", choices = colnames),
        selectInput("experimental_group", "Experimental Group Comparison Column:", choices = colnames),
        selectInput("experimental_group_1", "Experimental Group 1 Column:", choices = colnames),
        selectInput("experimental_group_2", "Experimental Group 2 Column:", choices = colnames),
        selectInput("ea_value", "EA Value Column:", choices = colnames)
      )
    })
    
    output$table_filter_kegg <- renderTable({
      req(loadedData())
      df <- loadedData()
      if (!is.null(df) && is.data.frame(df)) {  # Add this check
        df <- df[, c(input$sample, input$up_down, input$ont_description, input$ontology_kegg_2, input$experimental_group, input$experimental_group_1, input$experimental_group_2, input$ea_value, "Disease")]
      }
      df
    })
    
    
    # Download filtered data
    output$downloadFile_filter_kegg <- downloadHandler(
      filename = function() {
        paste("filtered_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df_filtered <- loadedData()
        df_filtered <- df_filtered[, c(input$ontology_kegg_2, input$ont_description,input$sample, input$up_down, input$experimental_group, input$experimental_group_1, input$experimental_group_2, input$ea_value, "Disease")]
        # Cambiar los nombres de las columnas
        colnames(df_filtered) <- c("ONTOLOGY", "ONT_DESCRIPTION","sample", "UP_DOWN","GROUP","GROUP_1", "GROUP_2","EA_VALUE","Disease")
        write.csv(df_filtered, file, row.names = FALSE)
      }
    )
  })
  # Lógica para procesar el archivo de KEGG
  observeEvent(input$file_kegg, {
    req(input$file_kegg)
    
    filename <- input$file_kegg$name
    data_kegg <- read.csv(input$file_kegg$datapath)
    
    # Llama a la función para procesar los datos de KEGG
    processed_kegg_data <- ancestors_kegg(data_kegg)
  
    # Generar la respuesta para descargar el archivo procesado
    output$downloadFile_kegg <- downloadHandler(
      filename = function() {
        "processed_kegg_data.csv"  # Especifica el nombre del archivo correctamente
      },
      content = function(file) {
        write.csv(processed_kegg_data, file, row.names = FALSE)
      }
    )
    
    # Mostrar la tabla procesada de KEGG
    output$table_kegg <- renderDataTable({  
      processed_kegg_data[, c("ONTOLOGY","ONT_DESCRIPTION", "sample", "GROUP_1","GROUP_2","UP_DOWN", "Disease","metabolic_domain","metabolic_subdomain")]
    })
  })
  # Lógica para limpiar los datos de análisis funcional de KEGG
  observeEvent(input$clear_kegg, {
    # Eliminar la tabla y el archivo cargado
    output$table_kegg <- renderDataTable({ NULL })
    unlink(input$file_kegg$datapath)  # Eliminar el archivo cargado
  })
  
  # Lógica para procesar el archivo de Gene Ontologies
  observeEvent(input$file_gene_ontologies, {
    req(input$file_gene_ontologies)  # Asegurar que se haya cargado un archivo
    
    original_data <- read.csv(input$file_gene_ontologies$datapath)
    # Llamar a la función ancestors para Gene Ontologies
    
    processed_data <- ancestors_gene_ontologies(original_data$ONTOLOGY, original_data$GROUP)

    colnames(processed_data) <- c("GROUP", "ONTOLOGY", "ANCESTORS")
    # Modificar FILE.GOEA.ANCESTORS
    processed_data$ANCESTORS <- lapply(processed_data$ANCESTORS, function(x) {
      x <- unlist(strsplit(x, ", "))
      x <- x[!x %in% c("GO:0003674", "GO:0005575", "GO:0008150")]
      x <- trimws(x)  
      paste(x, collapse = ", ")
    })
    
    # Realizar el merge por la columna "ONTOLOGY"
    original_data <- merge(original_data, processed_data[, c("ONTOLOGY", "ANCESTORS")], by = "ONTOLOGY", all.x = TRUE)
    
    # Summarise
    original_data <- original_data %>%
      group_by(GROUP, ONTOLOGY, UP_DOWN) %>%
      summarise(
        ONT_DESCRIPTION = unique(ONT_DESCRIPTION),
        Disease = unique(Disease),
        EA_VALUE = mean(EA_VALUE),
        GROUP_1 = toString(unique(GROUP_1)),
        GROUP_2 = toString(unique(GROUP_2)),
        sample = toString(unique(sample)),
        UP_DOWN = unique(UP_DOWN),
        ANCESTORS = toString(unique(ANCESTORS)),
        .groups = 'drop'
      ) %>%
      filter(ANCESTORS != "NA" & ANCESTORS != "")
    
  
    
    # Apply function to find the first matching ancestor to original_data
    original_data <- original_data %>%
      mutate(first_ancestor = sapply(strsplit(ANCESTORS, ", "), find_first_matching_ancestor))
    
    # Apply function to get the name of the first ancestor to original_data
    original_data <- original_data %>%
      mutate(first_ancestor_name = sapply(first_ancestor, get_first_ancestor_name))
    
    # Eliminar la columna ANCESTORS
    data_without_ancestors <- original_data[, !names(original_data) %in% "ANCESTORS"]
    data_without_GROUPS <- data_without_ancestors[, !names(data_without_ancestors) %in% "GROUP"]
    
    # Definir la función downloadHandler para descargar el archivo original_data
  output$downloadFile_gene_ontologies <- downloadHandler(
    filename = function() {
      "go.csv"
    },
    content = function(file) {
      write.csv(data_without_ancestors, file, row.names = FALSE)
    }
  )
  

    
    # Mostrar la tabla procesada en la interfaz de usuario
    output$table_gene_ontologies <- renderDataTable({  # Usar renderDataTable en lugar de renderTable
      data_without_GROUPS[,1:9]
    })
  })
  
  # Lógica para limpiar los datos de análisis funcional de KEGG
  observeEvent(input$clear_go, {
    # Eliminar la tabla y el archivo cargado
    output$table_gene_ontologies <- renderDataTable({ NULL })
    unlink(input$file_gene_ontologies$datapath)  # Eliminar el archivo cargado
  })
  
 
  # Lógica para procesar el archivo de KEGG
  observeEvent(input$file_kegg_network, {
    
    req(input$file_kegg_network)
    dataset <- read.csv(input$file_kegg_network$datapath, stringsAsFactors = FALSE)
    dataset <- analyze_regulation(dataset) 
    
    # Actualización de selectInput
    observe({
      # Actualizar selectInput con los datos cargados
      updateSelectInput(session, "sample_kegg", choices = unique(dataset$sample))
      updateSelectInput(session, "disease_kegg", choices = unique(dataset$Disease))
      updateSelectInput(session, "group_by_kegg", choices = c("Domain", "Subdomain", "Relation"))
    })
    
    # Actualización dinámica del selector de "Group"
    observeEvent(list(input$sample_kegg, input$disease_kegg), {
      req(input$sample_kegg, input$disease_kegg)
      
      # Filtrar el dataset basado en sample y disease seleccionados
      filtered_data_group <- dataset %>%
        filter(sample == input$sample_kegg,
               Disease %in% input$disease_kegg)
      
      # Obtener los grupos disponibles basados en los filtros
      available_groups <- unique(filtered_data_group$GROUP)
      
      # Actualizar el selectInput de group_kegg con los grupos disponibles y la opción "All groups"
      updateSelectInput(session, "group_kegg", choices = c("All groups", available_groups))
    })
    
    observeEvent(list(input$sample_kegg, input$disease_kegg, input$group_kegg, input$group_by_kegg), {
      req(input$sample_kegg, input$disease_kegg, input$group_kegg, input$group_by_kegg)
      
      # Filtrar los datos basado en sample y disease seleccionados
      filtered_data <- dataset %>%
        filter(sample == input$sample_kegg,
               Disease %in% input$disease_kegg)
      
      # Si se selecciona "All groups", no filtramos por GROUP
      if (input$group_kegg != "All groups") {
        filtered_data <- filtered_data %>% filter(GROUP == input$group_kegg)
      }
      
      filtered_data$UP_DOWN <- toupper(filtered_data$UP_DOWN)
      
      # Extraer ontologías únicas de los datos filtrados
      unique_ontologies <- unique(filtered_data$ONTOLOGY)
      
      # Definir la columna de agrupamiento en función de la selección del usuario
      group_by_col <- switch(input$group_by_kegg,
                             "Domain" = "metabolic_domain",
                             "Subdomain" = "metabolic_subdomain",
                             "Relation" = "ONT_DESCRIPTION")
      
      # Inicializar listas vacías para nodos y edges
      all_nodes <- list()
      all_edges <- list()
      
      # Definir un mapa de colores para los grupos
      group_colors <- rainbow(length(unique(filtered_data[[group_by_col]])))
      names(group_colors) <- unique(filtered_data[[group_by_col]])
      
      # Iterar sobre cada ontología
      for (ontology in unique_ontologies) {
        # Filtrar los datos para la ontología actual
        ontology_data <- filtered_data %>% filter(ONTOLOGY == ontology)
        
        # Obtener todos los grupos únicos entre GROUP_1 y GROUP_2 para la ontología actual
        all_groups <- unique(c(ontology_data$GROUP_1, ontology_data$GROUP_2))
        
        # Crear nodos para todos los grupos únicos
        nodes <- data.frame(id = paste(ontology, all_groups, sep = "_"), label = ontology, group = all_groups,
                            Domain = unique(ontology_data$metabolic_domain),
                            Subdomain = unique(ontology_data$metabolic_subdomain),
                            Relation = unique(ontology_data$ONT_DESCRIPTION),
                            color.background = group_colors[all_groups], 
                            title = unique(ontology_data$ONT_DESCRIPTION)) # Asignar color de grupo
        
        # Definir tipos de flechas basados en UP_DOWN
        arrow_types <- ifelse(ontology_data$UP_DOWN == "UP", "to",
                              ifelse(ontology_data$UP_DOWN == "DOWN", "to", "none"))
        
        # Crear edges basados en los datos filtrados para la ontología actual
        edges <- data.frame(from = paste(ontology, ontology_data$GROUP_1, sep = "_"),
                            to = paste(ontology, ontology_data$GROUP_2, sep = "_"),
                            arrows = arrow_types,
                            color = ifelse(ontology_data$UP_DOWN == "UP", "blue",
                                           ifelse(ontology_data$UP_DOWN == "DOWN", "red", "black")),
                            title = paste("EA_value:", ontology_data$EA_VALUE))  # Añadir EA_VALUE en el tooltip
        
        # Añadir nodos y edges a las listas
        all_nodes[[ontology]] <- nodes
        all_edges[[ontology]] <- edges
      }
      
      # Combinar todos los nodos y edges
      all_nodes_combined <- do.call(rbind, all_nodes)
      all_edges_combined <- do.call(rbind, all_edges)
      
      # Renderizar la visualización de la red
      output$network_kegg <- renderVisNetwork({
        visNetwork(all_nodes_combined, edges = all_edges_combined, main = "Sample", width = "100%", height = "100%") %>%
          visNodes(color = list(border = "black"), shadow = TRUE) %>%  # Establecer color de borde y sombra en nodos
          visEdges(arrows = "to") %>%
          visOptions(highlightNearest = TRUE, selectedBy = input$group_by_kegg) %>%
          visLegend(main = "Groups", useGroups = TRUE, zoom = FALSE)
      })
      
      # Descargar la red visualizada
      output$download_network_kegg <- downloadHandler(
        filename = function() {
          paste('network-', Sys.Date(), '.html', sep = '')
        },
        content = function(con) {
          visNetwork(all_nodes_combined, edges = all_edges_combined, main = "Sample", width = "100%", height = "100%") %>%
            visNodes(color = list(border = "black"), shadow = TRUE) %>%  # Establecer color de borde y sombra en nodos
            visEdges(arrows = "to") %>%
            visOptions(highlightNearest = TRUE, selectedBy = input$group_by_kegg) %>%
            visLegend(main = "Groups", useGroups = TRUE, zoom = FALSE) %>% 
            visSave(con)
        }
      )
    })
    
    # Definir los colores de las flechas para la leyenda
    arrow_colors <- c("Up-regulated" = "blue", "Down-regulated" = "red", "Neutral" = "black")
    
    # Renderizado dinámico de la leyenda
    output$legend <- renderUI({
      tagList(
        tags$h3("Legend"),
        tags$h4("Arrows"),
        lapply(names(arrow_colors), function(arrow) {
          tags$div(style = "margin-bottom: 10px;",
                   tags$div(style = paste("display: inline-block; width: 20px; height: 2px; background-color:", arrow_colors[arrow], ";")), 
                   arrow)
        })
      )
    })
    
    # Texto adicional en la interfaz
    output$text_kegg <-renderText({
      "Download a comprehensive prompt designed to assist in the analysis of Gene Ontology (GO) enrichment across your study. 
      This prompt includes detailed information on the most enriched ontologies, samples analyzed, descriptions of GO terms, and more. It is a valuable 
      tool for scientists and researchers conducting in-depth biological data analysis."
    })
    
    # Descargar el archivo prompt
    output$download_prompt_kegg <- downloadHandler(
      filename = function() {
        paste("prompt-", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        top_ontologies <- filtered_data %>%
          arrange(desc(EA_VALUE)) %>%
          head(20)
        
        intro_text <- "Write an exhaustive analysis focusing on biological experimental conclusions to learn how the diseases are doing on a Gene Ontology (GO) enrichment dataset for CU or EC, taking the main ideas for all the dataset without specifying in each of them. The dataset includes enrichment information for the 15 most enriched ontologies in the dataset. Each entry in the dataset has the following attributes:

Ontology: The KEGG identifier.
Sample: Sample used for enrichment analysis.
Description: Description of the KEGG term.
Disease: Disease studied.
Group_1: First group for comparison.
Group_2: Second group for comparison.
ea_value: Enrichment value.
metabolic_domain: The first level hierarchy pathway in the KEGG hierarchy.
metabolic_subdomain: Second level of hierarchy pathway in the KEGG hierarchy.

So here are the gene ontologies to analyze:\\n\\n"
        
        ontology_list <- character(nrow(top_ontologies))
        
        for (i in seq_len(nrow(top_ontologies))) {
          ontology_info <- paste(
            "- Ontology:", top_ontologies$ONTOLOGY[i],
            "| Sample:", top_ontologies$sample[i],
            "| Description:", top_ontologies$ONT_DESCRIPTION[i],
            "| Disease:", top_ontologies$Disease[i],
            "| Group_1:", top_ontologies$GROUP_1[i],
            "| Group_2:", top_ontologies$GROUP_2[i],
            "| ea_value:", top_ontologies$EA_VALUE[i],
            "| metabolic_domain:", top_ontologies$metabolic_domain[i],
            "| metabolic_subdomain:", top_ontologies$metabolic_subdomain[i],
            sep = " "
          )
          
          # Append each ontology_info to ontology_list
          ontology_list[i] <- ontology_info
        }
        
        # Combine all ontology_list elements into a single string separated by newline characters
        ontology_list_text <- paste(ontology_list, collapse = "\\n")
        
        # Combine the introductory text and the ontology list text
        final_text <- paste(intro_text, ontology_list_text, sep = "")
        
        # Write the final text to the file
        writeLines(final_text, file)
      }
    )
  })
  
  # Lógica para limpiar los datos de análisis funcional de KEGG
  observeEvent(input$clear_kegg_network, {
    # Eliminar la tabla y el archivo cargado
    output$network_kegg <- renderVisNetwork({ NULL })
    unlink(input$file_kegg_network$datapath)  # Eliminar el archivo cargado
  })
  

  
  observeEvent(input$file_heatmap, {
    req(input$file_heatmap)
    dataset <- read.csv(input$file_heatmap$datapath, stringsAsFactors = FALSE)
    dataset <- analyze_regulation(dataset)
    
    # Actualización de selectInput
    updateSelectInput(session, "sample_heatmap", choices = unique(dataset$sample))
    updateSelectInput(session, "disease_heatmap", choices = unique(dataset$Disease))
    updateSelectInput(session, "group_by_heatmap", choices = c("Domain", "Subdomain", "Relation"))
    
    # Observador para actualizar las ontologías en base al nivel de agrupamiento
    observeEvent(input$group_by_heatmap, {
      filtered_data <- dataset %>%
        filter(sample == input$sample_heatmap, Disease == input$disease_heatmap)
      
      if (input$group_by_heatmap == "Domain") {
        unique_ontologies <- unique(filtered_data$metabolic_domain)
        updateSelectInput(session, "ontology_heatmap", choices = unique_ontologies)
      } else if (input$group_by_heatmap == "Subdomain") {
        unique_ontologies <- unique(filtered_data$metabolic_subdomain)
        updateSelectInput(session, "ontology_heatmap", choices = unique_ontologies)
      } else {
        unique_ontologies <- unique(filtered_data$ONT_DESCRIPTION)
        updateSelectInput(session, "ontology_heatmap", choices = unique_ontologies)
      }
    })
    
    observeEvent(input$ontology_heatmap, {
      req(input$ontology_heatmap)
      
      # Filtrar datos basados en las selecciones del usuario
      filtered_data <- dataset %>%
        filter(sample == input$sample_heatmap, Disease == input$disease_heatmap)
      
      group_by_col <- switch(input$group_by_heatmap,
                             "Domain" = "metabolic_domain",
                             "Subdomain" = "metabolic_subdomain",
                             "Relation" = "ONT_DESCRIPTION")
      
      # Filtrar para obtener las ontologías bajo el grupo seleccionado
      filtered_data <- filtered_data %>%
        filter(!!sym(group_by_col) == input$ontology_heatmap) %>%
        group_by(GROUP, ONT_DESCRIPTION) %>%
        summarize(EA_VALUE = mean(EA_VALUE, na.rm = TRUE), .groups = 'drop')
      
      # Crear el heatmap
      output$heatmap_plot <- renderPlotly({
        p <- ggplot(filtered_data, aes(x = ONT_DESCRIPTION, y = GROUP, fill = EA_VALUE)) +
          geom_tile(color = "white") +
          scale_fill_gradient(low = "blue", high = "red") +
          labs(
            title = paste("Heatmap de la Muestra:", input$sample_heatmap, 
                          "y Enfermedad:", input$disease_heatmap),
            x = "Ontologías",
            y = "Grupo Experimental",
            fill = "Valor de EA (EA_VALUE)"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
          )
        
        ggplotly(p)  # Convierte el ggplot a plotly para interacción
      })
    })
    
    output$download_heatmap <- downloadHandler(
      filename = function() {
        paste('heatmap-', Sys.Date(), '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6)
      }
    )
  })
  observeEvent(input$file_volcano, {
    req(input$file_volcano)
    dataset <- read.csv(input$file_volcano$datapath, stringsAsFactors = FALSE)
    dataset <- analyze_regulation(dataset)
    
    # Actualización de selectInput para los valores únicos
    updateSelectInput(session, "sample_volcano", choices = unique(dataset$sample))
    updateSelectInput(session, "disease_volcano", choices = unique(dataset$Disease))
    updateSelectInput(session, "group_by_volcano", choices = c("Domain", "Subdomain", "Relation"))
    
    # Observador para actualizar las ontologías en base al nivel de agrupamiento
    observeEvent(input$group_by_volcano, {
      filtered_data <- dataset %>%
        filter(sample == input$sample_volcano, Disease == input$disease_volcano)
      
      if (input$group_by_volcano == "Domain") {
        unique_ontologies <- unique(filtered_data$metabolic_domain)
        updateSelectInput(session, "ontology_volcano", choices = unique_ontologies)
      } else if (input$group_by_volcano == "Subdomain") {
        unique_ontologies <- unique(filtered_data$metabolic_subdomain)
        updateSelectInput(session, "ontology_volcano", choices = unique_ontologies)
      } else {
        unique_ontologies <- unique(filtered_data$ONT_DESCRIPTION)
        updateSelectInput(session, "ontology_volcano", choices = unique_ontologies)
      }
    })
    
    observeEvent(input$ontology_volcano, {
      req(input$ontology_volcano)
      
      # Filtrar datos basados en las selecciones del usuario
      filtered_data <- dataset %>%
        filter(sample == input$sample_volcano, Disease == input$disease_volcano)
      
      group_by_col <- switch(input$group_by_volcano,
                             "Domain" = "metabolic_domain",
                             "Subdomain" = "metabolic_subdomain",
                             "Relation" = "ONT_DESCRIPTION")
      
      # Filtrar para obtener las ontologías bajo el grupo seleccionado
      volcano_data <- filtered_data %>%
        filter(!!sym(group_by_col) == input$ontology_volcano) %>%
        select(log2FoldChange, pvalue, gene)
      
      # Transformar el p-value a -log10(p-value) para el volcano plot
      volcano_data$negLogPval <- -log10(volcano_data$pvalue)
      
      # Crear el volcano plot
      output$volcano_plot <- renderPlotly({
        # Crear el ggplot para el volcano plot
        p <- ggplot(volcano_data, aes(x = log2FoldChange, y = negLogPval, text = gene)) +
          geom_point(aes(color = negLogPval > -log10(0.05) & abs(log2FoldChange) > 1), 
                     size = 2, alpha = 0.6) +
          scale_color_manual(values = c("black", "red")) +
          geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue") +
          geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "blue") +
          labs(x = "log2(Fold Change)", y = "-log10(p-value)", 
               title = paste("Volcano Plot: Sample:", input$sample_volcano, 
                             "Disease:", input$disease_volcano)) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
        
        # Hacer el plot interactivo con plotly
        ggplotly(p, tooltip = "text")
      })
    })
    
    # Opción para descargar el volcano plot
    output$download_volcano <- downloadHandler(
      filename = function() {
        paste('volcano-', Sys.Date(), '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6)
      }
    )
  })
  
  
  observeEvent(input$file_go_network, {
    req(input$file_go_network)
    df_4 <- read.csv(input$file_go_network$datapath, stringsAsFactors = FALSE)
    df_4 <- analyze_regulation(df_4)
    
    observe({
      updateSelectInput(session, "sample_go", choices = unique(df_4$sample))
      updateSelectInput(session, "ont_description", choices = unique(df_4$ONT_DESCRIPTION))
      updateSelectInput(session, "disease_go", choices = unique(df_4$Disease))
    })
    
    observeEvent(list(input$sample_go, input$ont_description, input$disease_go), {
      req(input$sample_go, input$ont_description, input$disease_go)
      
      # Filtrar el dataframe según los valores seleccionados en los otros inputs
      filtered_data_group <- df_4 %>%
        filter(sample == input$sample_go,
               ONT_DESCRIPTION == input$ont_description,
               Disease %in% input$disease_go)
      
      # Extraer solo los grupos correspondientes a la selección actual
      available_groups <- unique(filtered_data_group$GROUP)
      
      # Añadir la opción "All groups" al selector de grupo
      updateSelectInput(session, "group_go", choices = c("All groups", available_groups))
    })
    
    observeEvent(input$group_go, {
      req(input$sample_go, input$ont_description, input$disease_go)
      
      # Lógica para filtrar el dataframe
      if (input$group_go == "All groups") {
        # Si se selecciona "All groups", no filtrar por grupo
        filtered_data <- df_4 %>%
          filter(sample == input$sample_go,
                 ONT_DESCRIPTION == input$ont_description,
                 Disease %in% input$disease_go)
      } else {
        # Filtrar el dataframe con el grupo seleccionado
        filtered_data <- df_4 %>%
          filter(sample == input$sample_go,
                 ONT_DESCRIPTION == input$ont_description,
                 Disease %in% input$disease_go,
                 GROUP == input$group_go)
      }
      
      unique_ontologies <- unique(filtered_data$ONTOLOGY)
      
      
      # Initialize empty lists to store nodes and edges
      all_nodes <- list()
      all_edges <- list()
      # Define a color map for groups
      group_colors <- rainbow(length(unique(filtered_data$group_by_col)))
      names(group_colors) <- unique(filtered_data$group_by_col)
      
      # Iterate over each ontology
      for (ontology in unique_ontologies) {
        # Filter data for the current ontology
        ontology_data <- filtered_data %>% filter(ONTOLOGY == ontology)
        
        # Get all unique groups between GROUP_1 and GROUP_2 for the current ontology
        all_groups <- unique(c(ontology_data$GROUP_1, ontology_data$GROUP_2))
        
        # Create nodes for all unique groups
        nodes <- data.frame(id = paste(ontology, all_groups, sep = "_"), label = ontology, group = all_groups,
                            first_anc = unique(ontology_data$first_ancestor_name), title = ontology)
        
        # Create a vector of arrow types based on UP_DOWN values
        arrow_types_goea <- ifelse(ontology_data$UP_DOWN == "UP", "to",
                                   ifelse(ontology_data$UP_DOWN == "DOWN", "to", "none"))
        
        # Create edges based on filtered data for the current ontology
        edges <- data.frame(from = paste(ontology, ontology_data$GROUP_1, sep = "_"),
                            to = paste(ontology, ontology_data$GROUP_2, sep = "_"),
                            arrows = arrow_types_goea,
                            color = ifelse(ontology_data$UP_DOWN == "UP", "blue",
                                           ifelse(ontology_data$UP_DOWN == "DOWN", "red", "black")),
                            title = paste("EA_value:", ontology_data$EA_VALUE))  # Tooltip to show EA_value
        
        
        # Append nodes and edges to the lists
        all_nodes[[ontology]] <- nodes
        all_edges[[ontology]] <- edges
      }
      
      # Combine all nodes and edges
      all_nodes_combined <- do.call(rbind, all_nodes)
      all_edges_combined <- do.call(rbind, all_edges)
      
      # Render the network visualization
      output$network_go <- renderVisNetwork({
        visNetwork(all_nodes_combined, edges = all_edges_combined, main = "Sample", width = "100%", height = "100%") %>%
          visNodes(color = list(border = "black"), shadow = TRUE) %>%  # Setting node border color and adding shadow
          visEdges(arrows = "to") %>%
          visGroups(groupname = "group", color = list(background = rainbow(length(unique(all_nodes_combined$group))))) %>%
          visLegend(main = "Groups", useGroups = TRUE, position = "left", zoom = FALSE) %>%
          visOptions(highlightNearest = TRUE, selectedBy = "first_anc")  
      })
      
      
      arrow_colors <- c("Up-regulated" = "blue", "Down-regulated" = "red", "Neutral" = "black")
      
      
      # Renderizado dinámico de la leyenda
      output$legend_go <- renderUI({
        tagList(
          tags$h3("Legend"),
          tags$h4("Arrows"),
          lapply(names(arrow_colors), function(arrow) {
            tags$div(style = "margin-bottom: 10px;",
                     tags$div(style = paste("display: inline-block; width: 20px; height: 2px; background-color:", arrow_colors[arrow], ";")), 
                     arrow)
          })
        )
      })
      
      output$text_go <-renderText({
        "Download a comprehensive prompt designed to assist in the analysis of Gene Ontology (GO) enrichment across your study. 
        This prompt includes detailed information on the most enriched ontologies, samples analyzed, descriptions of GO terms, and more. It is a valuable 
        tool for scientists and researchers conducting in-depth biological data analysis."
      })
      
      
      
      output$download_prompt_go <- downloadHandler(
        filename = function() {
          paste("prompt-", Sys.Date(), ".txt", sep = "")
        },
        content = function(file) {
          top_ontologies <- filtered_data %>%
            arrange(desc(EA_VALUE)) %>%
            head(20)
          
          intro_text <- "Write an exhaustive analysis focusing on biological experimental conclusions to learn how the diseases are doing on a Gene Ontology (GO) enrichment dataset for CU or EC, taking the main ideas for all the dataset without specifying in each of them. The dataset includes enrichment information for the 15 most enriched ontologies in the dataset. Each entry in the dataset has the following attributes:

Ontology: The GO identifier.
Sample: Sample used for enrichment analysis.
Description: Description of the GO term.
Disease: Disease studied.
Group_1: First group for comparison.
Group_2: Second group for comparison.
ea_value: Enrichment value.
first_ancestor: The most general ancestor node in the GO hierarchy.

So here are the gene ontologies to analyze:\\n\\n"
          
          ontology_list <- character(nrow(top_ontologies))
          
          for (i in seq_len(nrow(top_ontologies))) {
            ontology_info <- paste(
              "- Ontology:", top_ontologies$ONTOLOGY[i],
              "| Sample:", top_ontologies$sample[i],
              "| Description:", top_ontologies$ONT_DESCRIPTION[i],
              "| Disease:", top_ontologies$Disease[i],
              "| Group_1:", top_ontologies$GROUP_1[i],
              "| Group_2:", top_ontologies$GROUP_2[i],
              "| ea_value:", top_ontologies$EA_VALUE[i],
              "| first_ancestor:", top_ontologies$first_ancestor_name[i],
              sep = " "
            )
            
            # Append each ontology_info to ontology_list
            ontology_list[i] <- ontology_info
          }
          
          # Combine all ontology_list elements into a single string separated by newline characters
          ontology_list_text <- paste(ontology_list, collapse = "\\n")
          
          # Combine the introductory text and the ontology list text
          final_text <- paste(intro_text, ontology_list_text, sep = "")
          
          # Write the final text to the file
          writeLines(final_text, file)
        })
    })
  })
    output$download_network_go <- downloadHandler(
      filename = function() {
        paste('network-', Sys.Date(), '.html', sep='')
      },
      content = function(con) 
        
        visNetwork(nodes, edges = filtered_edges, main = "Sample", width = "100%") %>%
        visNodes(color = list(background = "white", border = "black"), size = "value") %>%
        visGroups(groupname = "group", color = list(border = "black", background = rainbow(length(group_names), start = 0, end = 1)), legend = TRUE) %>%
        visPhysics(
          enabled = TRUE,
          repulsion = list(nodeDistance = 1)
        ) %>%
        visOptions(highlightNearest = TRUE, selectedBy = "first_anc") %>%
        visLegend(main = "Group", useGroups = TRUE, zoom = FALSE) %>% 
        visSave(con)
    )
    
  
  # Lógica para limpiar los datos de análisis funcional de KEGG
  observeEvent(input$clear_go_network, {
    # Eliminar la tabla y el archivo cargado
    output$network_go <- renderDataTable({ NULL })
    unlink(input$file_go_network$datapath)  # Eliminar el archivo cargado
    
    
  })
  
  # Lógica para procesar el archivo y generar el heatmap
  observeEvent(input$file_heatmap_go, {
    req(input$file_heatmap_go)
    dataset <- read.csv(input$file_heatmap_go$datapath, stringsAsFactors = FALSE)
    dataset <- analyze_regulation(dataset)
    
    # Actualización de selectInput
    updateSelectInput(session, "sample_heatmap_go", choices = unique(dataset$sample))
    updateSelectInput(session, "disease_heatmap_go", choices = unique(dataset$Disease))
    
    # Observador para actualizar las ontologías en base a las selecciones de sample y disease
    observeEvent(input$sample_heatmap_go, {
      req(input$sample_heatmap_go, input$disease_heatmap_go)
      
      # Filtrar datos
      filtered_data <- dataset %>%
        filter(sample == input$sample_heatmap_go, Disease == input$disease_heatmap_go)
      
      # Actualizar el selectInput de ONT_DESCRIPTION
      updateSelectInput(session, "ont_description_heatmap_go", choices = unique(filtered_data$ONT_DESCRIPTION))
    })
    
    # Observador para crear el heatmap basado en las selecciones del usuario
    observeEvent(input$ont_description_heatmap_go, {
      req(input$ont_description_heatmap_go)
      
      # Filtrar datos basados en las selecciones del usuario
      filtered_data <- dataset %>%
        filter(sample == input$sample_heatmap_go, 
               Disease == input$disease_heatmap_go,
               ONT_DESCRIPTION == input$ont_description_heatmap_go)  # Añadir el filtro de ONT_DESCRIPTION
      
      # Agrupar y resumir los datos
      heatmap_data <- filtered_data %>%
        group_by(GROUP, ONTOLOGY) %>%
        summarize(EA_VALUE = mean(EA_VALUE, na.rm = TRUE), .groups = 'drop')
      
      # Crear el heatmap
      output$heatmap_plot_go <- renderPlotly({
        p <- ggplot(heatmap_data, aes(x = ONTOLOGY, y = GROUP, fill = EA_VALUE)) +
          geom_tile(color = "white") +
          scale_fill_gradient(low = "blue", high = "red") +
          labs(
            title = paste("Heatmap de la Muestra:", input$sample_heatmap_go, 
                          "y Enfermedad:", input$disease_heatmap_go),
            x = "Ontologías",
            y = "Grupo Experimental",
            fill = "Valor de EA (EA_VALUE)"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
          )
        
        ggplotly(p, tooltip = c("x", "y", "fill"))  # Agrega tooltips para mostrar valores
      })
    })
    
    # Descargar el heatmap
    output$download_heatmap_go <- downloadHandler(
      filename = function() {
        paste('heatmap_go-', Sys.Date(), '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6)
      }
    )
  })
}
