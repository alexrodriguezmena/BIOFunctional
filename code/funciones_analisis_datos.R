# Especificar un mirror de CRAN
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Lista de paquetes requeridos
required_packages <- c(
  "shiny",              # For creating interactive applications
  "shinydashboard",     # For creating dashboards within the Shiny application
  "shinycssloaders",    # For adding loading indicators
  "httr",               # For making HTTP requests
  "readr",              # For reading rectangular data (CSV, TSV, etc.)
  "dplyr",              # For data manipulation using a consistent set of verbs
  "tibble",             # For working with data frames in a modern and consistent way
  "future.apply",       # For running functions in parallel to speed up processing
  "DT",                 # For displaying interactive tables in Shiny apps
  "rvest",              # For web scraping and parsing HTML pages
  "visNetwork",         # For creating interactive network visualizations
  "bslib",              # For customizing and theming Shiny applications using Bootstrap
  "fastmap",            # For creating efficient key-value stores, useful for caching
  "shinyBS",            # For adding additional Bootstrap components like modals and tooltips
  "shinyjs",            # For using JavaScript in Shiny apps to enhance interactivity
  "plotly"
)

# Instalar y cargar los paquetes
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}



# Function to find the kegg structure
ancestors_kegg <- function(data) {
  relations <- unique(data$ONTOLOGY)
  processed_data <- data.frame()  
  
  for (i in seq_along(relations)) {
    relation <- relations[i] 
    
    # Obtain the reactions and interactions
    metabolic_domain <- ""   
    metabolic_subdomain <- ""    
    
    # read the website and extract the table
    url <- paste0("https://www.genome.jp/dbget-bin/www_bget?pathway:", relation)
    webpage <- read_html(url)
    
    # web scraping ont he table
    tables <- html_nodes(webpage, "table")
    table <- html_table(tables[[1]], fill = TRUE)
    
    # Find the line with the relevant info
    class_row_index <- which(table[, 1] == "Class")
    if (length(class_row_index) > 0) {
      class_row <- table[class_row_index, ]
      
      class_values <- unlist(strsplit(as.character(class_row), ";"))
      metabolic_domain <- class_values[2]
      metabolic_subdomain <- gsub("BRITE hierarchy", "", class_values[3])
    }
    
    # Create a dataframe with the processsed data
    subset_data <- subset(data, ONTOLOGY == relation)
    subset_data$metabolic_domain <- metabolic_domain
    subset_data$metabolic_subdomain <- metabolic_subdomain
    processed_data <- rbind(processed_data, subset_data)
  }
  processed_data$metabolic_domain <- ifelse(processed_data$metabolic_domain == "BRITE hierarchy", NA, processed_data$metabolic_domain)
  return(processed_data)
}

ancestors_gene_ontologies <- function(ontologies, groups) {
  # Function ancestors retrieval
  get_ancestors <- function(ontology) {
    url <- sprintf("https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/%s/ancestors?relations=is_a%%2Cpart_of%%2Coccurs_in%%2Cregulates",
                   URLencode(ontology, reserved = TRUE))
    response <- content(GET(url, accept("application/json")), "parsed")
    if (!response$results[[1]]$isObsolete) {
      ancestors <- response$results[[1]]$ancestors
      return(setdiff(ancestors, ontology))
    } else {
      return(NULL)
    }
  }
  
  # Obtain ancestors by parallel algorithm
  plan(multisession)
  ancestors <- future_lapply(ontologies, get_ancestors)
  
  # Filter and combine data
  data <- data.frame(Group = groups, Ontology = ontologies, Ancestors = sapply(ancestors, toString))
  data <- data[!is.null(data$Ancestors), ]
  
  return(data)
}

# Function to retrieve children of a GO term
get_children_quickgo <- function(go_id) {
  # Construct URL to retrieve children of a GO term
  base_url <- "https://www.ebi.ac.uk/QuickGO/services/ontology/go/terms/"
  url <- paste0(base_url, go_id, "/children")
  response <- httr::GET(url)
  if (httr::http_type(response) == "application/json") {
    children <- httr::content(response, "parsed")
    return(children)
  } else {
    stop("Error: The response is not in JSON format.")
  }               
}

# Function to retrieve information of children of a GO term
get_children_info <- function(go_term) {
  children_quickgo <- get_children_quickgo(go_term)
  children_list <- children_quickgo$results
  children_df <- data.frame(id = character(), name = character(), stringsAsFactors = FALSE)
  for (child_info in children_list[[1]]$children) {
    child_id <- child_info$id
    child_name <- child_info$name
    child_df <- data.frame(id = child_id, name = child_name, stringsAsFactors = FALSE)
    children_df <- rbind(children_df, child_df)
  }
  return(children_df)
}


# Function to find the first matching ancestor
find_first_matching_ancestor <- function(ancestors) {
  for (ancestor in ancestors) {
    if (ancestor %in% children_info_mf$id) {
      return(ancestor)
    } else if (ancestor %in% children_info_bp$id) {
      return(ancestor)
    } else if (ancestor %in% children_info_cc$id) {
      return(ancestor)
    }
  }
  return(NA)
}

# Define function to get the name of the first ancestor
get_first_ancestor_name <- function(ancestor_id) {
  if (ancestor_id %in% children_info_mf$id) {
    return(children_info_mf$name[children_info_mf$id == ancestor_id])
  } else if (ancestor_id %in% children_info_bp$id) {
    return(children_info_bp$name[children_info_bp$id == ancestor_id])
  } else if (ancestor_id %in% children_info_cc$id) {
    return(children_info_cc$name[children_info_cc$id == ancestor_id])
  } else {
    return(NA)
  }
}

renderTable({
  req(loadedData())
  df <- loadedData()
  
  # Check if df is a data frame and has the selected columns
  if (!is.null(df) && is.data.frame(df) &&
      all(c(input$sample, input$up_down, input$ont_description, input$ontology, input$experimental_group, input$ea_value) %in% colnames(df))) {
    
    df <- df[, c(input$sample, input$up_down, input$ont_description, input$ontology, input$experimental_group, input$ea_value, "Disease")]
    colnames(df) <- c("sample", "UP_DOWN", "ONT_DESCRIPTION", "ONTOLOGY", "GROUP", "EA_VALUE", "Disease")
    
    return(df)
  } else {
    return(NULL)  # Return NULL if df is not a data frame or doesn't have selected columns
  }
})

# Define function remove_duplicate_observations before analyze_regulation
remove_duplicate_observations <- function(data) {
  # Encuentra las observaciones duplicadas por "ontology", "group" y "EA_VALUE"
  duplicated_rows <- duplicated(data[, c("ONTOLOGY", "GROUP", "EA_VALUE")]) | duplicated(data[, c("ONTOLOGY", "GROUP", "EA_VALUE")], fromLast = TRUE)
  
  # Cambia GOEA_up_DOWN a "NEUTRAL" en las observaciones duplicadas
  data[duplicated_rows, "UP_DOWN"] <- "NEUTRAL"
  
  # Elimina las filas duplicadas basadas en "ONTOLOGY", "GROUP_1", "GROUP_2", y "EA_VALUE"
  data <- data[!duplicated(data[, c("ONTOLOGY", "GROUP_1", "GROUP_2", "EA_VALUE")]), ]
  
  return(data)
}


# Define function analyze_regulation
analyze_regulation <- function(data) {
  
  # Split the data into groups based on the combination of "ONTOLOGY" and "GROUP" columns
  # Each unique combination of "ONTOLOGY" and "GROUP" will create a separate subset
  pairs <- split(data, paste(data$ONTOLOGY, data$GROUP))
  
  # Initialize a numeric vector to store the row names of the observations that will be kept
  observations_to_keep <- numeric()
  
  # Iterate over each subset (pair) of observations
  for (pair in pairs) {
    # Convert each pair (subset) into a data frame
    pair_df <- as.data.frame(pair)
    
    # Check if the subset contains exactly two observations
    if (nrow(pair_df) == 2) {
      # Extract the "EA_VALUE" for both observations
      EA_VALUE_1 <- pair_df[["EA_VALUE"]][1]
      EA_VALUE_2 <- pair_df[["EA_VALUE"]][2]
      
      # Compare the "EA_VALUE" of the two observations
      if (EA_VALUE_1 != EA_VALUE_2) {
        # If the values are different, keep the observation with the higher "EA_VALUE"
        observation_to_keep <- ifelse(EA_VALUE_1 > EA_VALUE_2, rownames(pair_df)[1], rownames(pair_df)[2])
        observations_to_keep <- c(observations_to_keep, observation_to_keep)
      } else {
        # If the "EA_VALUE" values are equal, keep both observations
        observations_to_keep <- c(observations_to_keep, rownames(pair_df))
      }
    } else if (nrow(pair_df) == 1) {
      # If there is only one observation in the subset, keep it
      observations_to_keep <- c(observations_to_keep, rownames(pair_df)[1])
    }
  }
  
  # Filter the original data to keep only the observations whose row names are in "observations_to_keep"
  data <- data[rownames(data) %in% observations_to_keep, ]
  
  # Return the filtered data after removing any duplicate observations (assumed functionality)
  return(remove_duplicate_observations(data))
}



# Get children information for different ontology types
children_info_mf <- get_children_info("GO:0003674")
children_info_bp <- get_children_info("GO:0008150")
children_info_cc <- get_children_info("GO:0005575")

