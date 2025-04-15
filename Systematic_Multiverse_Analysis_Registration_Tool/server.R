library(shiny)
library(visNetwork)
library(shinyjs)
library(shinythemes)
library(DT)
library(bslib)
library(bsplus)
library(htmltools)
library(sortable)
library(shinyBS)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(webshot2)
library(pagedown)


source("Source_function.R", local = TRUE)

server <- function(input, output, session) {

  # ✅ Prevent session from timing out due to inactivity
  auto_keep_alive <- reactiveTimer(60000)  # Fires every 5 minutes
  
  observe({
    auto_keep_alive()
    # Invisible ping to keep session alive
  })
  session$allowReconnect(TRUE)
################# Variables Required for Overall Functionality #################  
  
  # ✅ Reactive values for each checkbox & text input
  user_inputs <- reactiveValues(
    aspect1a_1 = FALSE, aspect1a_text_1 = "",
    aspect1a_2 = FALSE, aspect1a_text_2 = "",
    aspect1a_3 = FALSE, aspect1a_text_3 = "",
    aspect1a_4 = FALSE, aspect1a_text_4 = "",
    
    defens1a_1 = FALSE, defens1a_text_1 = "",
    defens1a_2 = FALSE, defens1a_text_2 = "",
    defens1a_3 = FALSE, defens1a_text_3 = "",
    defens1a_4 = FALSE, defens1a_text_4 = ""
  )
  
  # ✅ Observe each checkbox separately and store its value (True/False)
  observeEvent(input$aspect1a_1, { user_inputs$aspect1a_1 <- input$aspect1a_1; print(paste("DEBUG: aspect1a_1 =", input$aspect1a_1)) })
  observeEvent(input$aspect1a_2, { user_inputs$aspect1a_2 <- input$aspect1a_2; print(paste("DEBUG: aspect1a_2 =", input$aspect1a_2)) })
  observeEvent(input$aspect1a_3, { user_inputs$aspect1a_3 <- input$aspect1a_3; print(paste("DEBUG: aspect1a_3 =", input$aspect1a_3)) })
  observeEvent(input$aspect1a_4, { user_inputs$aspect1a_4 <- input$aspect1a_4; print(paste("DEBUG: aspect1a_4 =", input$aspect1a_4)) })
  
  observeEvent(input$defens1a_1, { user_inputs$defens1a_1 <- input$defens1a_1; print(paste("DEBUG: defens1a_1 =", input$defens1a_1)) })
  observeEvent(input$defens1a_2, { user_inputs$defens1a_2 <- input$defens1a_2; print(paste("DEBUG: defens1a_2 =", input$defens1a_2)) })
  observeEvent(input$defens1a_3, { user_inputs$defens1a_3 <- input$defens1a_3; print(paste("DEBUG: defens1a_3 =", input$defens1a_3)) })
  observeEvent(input$defens1a_4, { user_inputs$defens1a_4 <- input$defens1a_4; print(paste("DEBUG: defens1a_4 =", input$defens1a_4)) })
  
  # ✅ Observe each text input separately and store its value (if not empty)
  observeEvent(input$aspect1a_text_1, { user_inputs$aspect1a_text_1 <- input$aspect1a_text_1; print(paste("DEBUG: aspect1a_text_1 =", input$aspect1a_text_1)) })
  observeEvent(input$aspect1a_text_2, { user_inputs$aspect1a_text_2 <- input$aspect1a_text_2; print(paste("DEBUG: aspect1a_text_2 =", input$aspect1a_text_2)) })
  observeEvent(input$aspect1a_text_3, { user_inputs$aspect1a_text_3 <- input$aspect1a_text_3; print(paste("DEBUG: aspect1a_text_3 =", input$aspect1a_text_3)) })
  observeEvent(input$aspect1a_text_4, { user_inputs$aspect1a_text_4 <- input$aspect1a_text_4; print(paste("DEBUG: aspect1a_text_4 =", input$aspect1a_text_4)) })
  
  observeEvent(input$defens1a_text_1, { user_inputs$defens1a_text_1 <- input$defens1a_text_1; print(paste("DEBUG: defens1a_text_1 =", input$defens1a_text_1)) })
  observeEvent(input$defens1a_text_2, { user_inputs$defens1a_text_2 <- input$defens1a_text_2; print(paste("DEBUG: defens1a_text_2 =", input$defens1a_text_2)) })
  observeEvent(input$defens1a_text_3, { user_inputs$defens1a_text_3 <- input$defens1a_text_3; print(paste("DEBUG: defens1a_text_3 =", input$defens1a_text_3)) })
  observeEvent(input$defens1a_text_4, { user_inputs$defens1a_text_4 <- input$defens1a_text_4; print(paste("DEBUG: defens1a_text_4 =", input$defens1a_text_4)) })
  
################################################################################

################################### Welcome ####################################
  # Upload CSV zip folder
  observeEvent(input$upload_construction_doc, {
    req(input$upload_construction_doc)
    
    # ✅ Ensure uploaded file is a ZIP
    if (!grepl("\\.zip$", input$upload_construction_doc$name)) {
      showNotification("Error: Please upload a ZIP file!", type = "error")
      return()
    }
    
    # ✅ Extract files from ZIP
    unzip_dir <- file.path(tempdir(), "uploaded_csvs")
    dir.create(unzip_dir, showWarnings = FALSE)
    unzip(input$upload_construction_doc$datapath, exdir = unzip_dir)
    
    # ✅ Get all extracted CSV files
    csv_files <- list.files(unzip_dir, pattern = "\\.csv$", full.names = TRUE)
    
    if (length(csv_files) == 0) {
      showNotification("Error: No CSV files found in ZIP!", type = "error")
      return()
    }
    
    # ✅ Load each CSV based on expected structure
    for (csv in csv_files) {
      if (grepl("Aspects_Defensibility\\.csv$", csv)) {
        aspects_defensibility_data <- read.csv(csv, stringsAsFactors = FALSE)
        # ✅ Store Aspects & Defensibility data (Implement storage logic here)
      }
      
      if (grepl("Steps\\.csv$", csv)) {
        steps_data <- read.csv(csv, stringsAsFactors = FALSE)
        # ✅ Store Decision Nodes & Options (Implement storage logic here)
      }
      
      if (grepl("Pipelines\\.csv$", csv)) {
        pipelines_data <- read.csv(csv, stringsAsFactors = FALSE)
        restored_pipelines <- list()
        
        for (i in seq_len(nrow(pipelines_data))) {
          pipeline_name <- pipelines_data$Name[i]
          pipeline_structure <- strsplit(pipelines_data$Pipeline[i], " -> ")[[1]]
          
          processed_structure <- c()
          for (item in pipeline_structure) {
            item <- trimws(item)
            
            # ✅ Heuristic: if it starts with "Option", it's an option
            if (grepl("^Option", item, ignore.case = TRUE)) {
              processed_structure <- c(processed_structure, paste0("➝ ", item))
            } else {
              processed_structure <- c(processed_structure, item)
            }
          }
          
          restored_pipelines[[pipeline_name]] <- processed_structure
        }
        
        saved_pipelines$data <- restored_pipelines
      }
      
      if (grepl("Visual_Network\\.csv$", csv)) {
        visual_network_data <- read.csv(csv, stringsAsFactors = FALSE)
        
        # ✅ Convert uploaded network data into Step 1d format
        nodes_df <- visual_network_data[visual_network_data$Type == "Node", ]
        edges_df <- visual_network_data[visual_network_data$Type == "Edge", ]
        
        for (pipeline_name in unique(nodes_df$Name)) {
          pipeline_nodes <- nodes_df[nodes_df$Name == pipeline_name, ]
          pipeline_edges <- edges_df[edges_df$Name == pipeline_name, ]
          
          # Build node list first
          node_ids <- pipeline_nodes$ID
          node_shapes <- setNames(pipeline_nodes$Shape, pipeline_nodes$ID)
          
          # Build edge map
          edge_map <- split(pipeline_edges$To, pipeline_edges$From)
          
          # Initialize all levels to NA
          node_levels <- setNames(rep(NA, length(node_ids)), node_ids)
          
          # Start with step nodes at level 1
          for (id in node_ids) {
            if (node_shapes[[id]] == "circle") {
              node_levels[[id]] <- 1
            }
          }
          
          # Recursively assign levels
          changed <- TRUE
          while (changed) {
            changed <- FALSE
            for (from_id in names(edge_map)) {
              from_level <- node_levels[[from_id]]
              if (!is.na(from_level)) {
                for (to_id in edge_map[[from_id]]) {
                  current_level <- node_levels[[to_id]]
                  new_level <- from_level + 1
                  if (is.na(current_level) || current_level < new_level) {
                    node_levels[[to_id]] <- new_level
                    changed <- TRUE
                  }
                }
              }
            }
          }
          
          # Assign computed levels
          pipeline_nodes$Level <- node_levels[pipeline_nodes$ID]
          
          formatted_nodes <- data.frame(
            id = pipeline_nodes$ID,
            label = pipeline_nodes$Label,
            color = pipeline_nodes$Color,
            shape = pipeline_nodes$Shape,
            level = pipeline_nodes$Level,
            stringsAsFactors = FALSE
          )
          
          
          formatted_edges <- data.frame(
            from = pipeline_edges$From,
            to = pipeline_edges$To,
            stringsAsFactors = FALSE
          )
          
          # ✅ Store in Step 1d format
          saved_pipelines$data[[paste0("graph_", pipeline_name)]] <- list(
            nodes = formatted_nodes,
            edges = formatted_edges
          )
        }
      }
    }
    
    showNotification("Upload successful! Pipelines and visual networks restored.", type = "message")
  })
  
################################################################################  
  
################################ Multiverse 1.0 ################################
  
################# Step 1a: Specify the Scope #################
  
  graph_data_1a <- reactiveValues(
    nodes = data.frame(id = numeric(0), label = character(0), shape = character(0), color = character(0), stringsAsFactors = FALSE),
    edges = data.frame(from = numeric(0), to = numeric(0), stringsAsFactors = FALSE)
  )
    
  # Reactive value to track step count
  step_count <- reactiveVal(0)
  
  # Reactive values to track dynamic options for each step
  step_options <- reactiveValues()
  
  # Create accordion function
  create_accordion <- function(step_id) {
    div(
      class = "panel panel-default",
      div(
        class = "panel-heading",
        role = "tab",
        id = paste0("heading_", step_id),
        h4(
          class = "panel-title",
          tags$a(
            href = paste0("#collapse_", step_id),
            "data-toggle" = "collapse",
            "data-parent" = "#main_accordion",
            "aria-expanded" = "false",
            "aria-controls" = paste0("collapse_", step_id),
            paste("Node", step_id)
          )
        )
      ),
      div(
        id = paste0("collapse_", step_id),
        class = "panel-collapse collapse",
        role = "tabpanel",
        div(
          class = "panel-body",
          textInput(paste0("step_name_", step_id), "Name of the Node", value = ""),
          div(
            id = paste0("options_container_", step_id),
            style = "margin-bottom: 10px;"
          ),
          actionButton(paste0("add_option_", step_id), "Add Option", class = "btn-primary", style = "margin-right: 10px;"),
          actionButton(paste0("done_", step_id), "Done", class = "btn-success")
        )
      )
    )
  }
  
  # Add initial accordion container
  observeEvent(once = TRUE, TRUE, {
    step_count(1)
    step_options[["1"]] <- list()
    insertUI(
      selector = "#accordion_container_step1a",
      where = "beforeEnd",
      ui = div(
        id = "main_accordion",
        class = "panel-group",
        create_accordion(step_count())
      )
    )
  })
  
  # Add new accordion on button click
  observeEvent(input$add_step, {
    new_step_id <- step_count() + 1
    step_count(new_step_id)
    step_options[[as.character(new_step_id)]] <- list()
    insertUI(
      selector = "#main_accordion",
      where = "beforeEnd",
      ui = create_accordion(new_step_id)
    )
  })
  
  # Track if a step is finalized (Done clicked)
  step_finalized <- reactiveValues()
  
  # Add new text input for options dynamically
  observe({
    lapply(1:step_count(), function(step_id) {
      observeEvent(input[[paste0("add_option_", step_id)]], {
        step_key <- as.character(step_id)
        
        if (is.null(step_options[[step_key]])) {
          step_options[[step_key]] <- list()
        }
        
        # ✅ Get a new unique ID that never repeats
        existing_option_ids <- as.numeric(names(step_options[[step_key]]))
        option_id <- ifelse(length(existing_option_ids) == 0, 1, max(existing_option_ids, na.rm = TRUE) + 1)
        
        option_label <- paste0("Option ", option_id)
        unique_option_id <- as.character(option_id)  # ✅ Ensure ID is string-based
        
        step_options[[step_key]][[unique_option_id]] <- option_label
        
        insertUI(
          selector = paste0("#options_container_", step_id),
          where = "beforeEnd",
          ui = div(
            id = paste0("option_wrapper_", step_id, "_", unique_option_id),
            style = "display: flex; align-items: center; margin-bottom: 5px;",
            span(id = paste0("option_label_", step_id, "_", unique_option_id), strong(option_label)),
            textInput(
              paste0("option_", step_id, "_", unique_option_id),
              label = NULL,
              value = "",
              width = "70%"
            ),
            actionButton(
              paste0("delete_option_", step_id, "_", unique_option_id),
              "❌",
              class = "btn-danger btn-sm",
              style = "margin-left: 10px;"
            )
          )
        )
        
        print(paste("DEBUG: Added new option", option_label, "to Step", step_id))
        
      }, ignoreInit = TRUE)
    })
  })
  
  # Delete the selected option dynamically
  observe({
    lapply(1:step_count(), function(step_id) {
      step_key <- as.character(step_id)
      
      observe({
        lapply(names(step_options[[step_key]]), function(option_id) {
          observeEvent(input[[paste0("delete_option_", step_id, "_", option_id)]], {
            removeUI(selector = paste0("#option_wrapper_", step_id, "_", option_id))
            
            # ✅ Remove Option from Reactive Values
            step_options[[step_key]][[option_id]] <- NULL
            
            # ✅ Remove Option Node from Graph
            option_node_id <- paste0(step_id, "_", option_id)
            graph_data_1a$nodes <- graph_data_1a$nodes[graph_data_1a$nodes$id != option_node_id, ]
            graph_data_1a$edges <- graph_data_1a$edges[graph_data_1a$edges$to != option_node_id, ]
            
            print(paste("DEBUG: Deleted option", option_id, "from Step", step_id))
            
          }, ignoreInit = TRUE)
        })
      })
    })
  })
  
  # Delete an accordion dynamically and remove from visual network
  observeEvent(input$delete_step, {
    if (step_count() > 0) {
      last_step_id <- step_count()
      step_key <- as.character(last_step_id)
      
      # ✅ Remove UI Elements
      removeUI(selector = paste0("#heading_", last_step_id), immediate = TRUE)
      removeUI(selector = paste0("#collapse_", last_step_id), immediate = TRUE)
      
      # ✅ Remove from Reactive Values
      step_options[[step_key]] <- NULL
      step_count(last_step_id - 1)
      
      # ✅ Remove Step Node and All Associated Option Nodes from Graph
      graph_data_1a$nodes <- graph_data_1a$nodes[!grepl(paste0("^", last_step_id, "_?"), graph_data_1a$nodes$id), ]
      graph_data_1a$edges <- graph_data_1a$edges[graph_data_1a$edges$from != last_step_id & graph_data_1a$edges$to != last_step_id, ]
      
      print(paste("DEBUG: Step", last_step_id, "deleted from UI and network."))
    }
  })
  
  ensure_node_structure <- function(df) {
    if (nrow(df) == 0) {
      return(data.frame(id = numeric(0), label = character(0), shape = character(0), color = character(0), stringsAsFactors = FALSE))
    }
    return(df)
  }
  
  # Update accordion titles dynamically and create network nodes/edges
  observe({
    lapply(1:step_count(), function(step_id) {
      observeEvent(input[[paste0("done_", step_id)]], {
        step_key <- as.character(step_id)
        step_name <- input[[paste0("step_name_", step_id)]]
        
        # ✅ **Ensure the structure of nodes and edges before adding new data**
        graph_data_1a$nodes <- ensure_node_structure(graph_data_1a$nodes)
        graph_data_1a$edges <- ensure_node_structure(graph_data_1a$edges)
        
        # ✅ **Check if step name is empty**
        if (is.null(step_name) || step_name == "") {
          showNotification("⚠️ Step name cannot be empty! Please enter a step name.", type = "error")
          return()
        }
        
        # ✅ **Retrieve options and ensure unique IDs**
        if (!is.null(step_options[[step_key]]) && length(step_options[[step_key]]) > 0) {
          valid_options <- names(step_options[[step_key]]) # Get all option keys
          options <- sapply(valid_options, function(option_id) input[[paste0("option_", step_id, "_", option_id)]])
          
          # ✅ **Check if any option is empty**
          if (any(options == "")) {
            showNotification("⚠️ Options cannot be empty! Please enter a value or delete empty options.", type = "error")
            return()
          }
          
          options <- options[options != ""] # Remove empty options
          names(options) <- paste0(step_id, "_", valid_options) # Assign unique IDs
        } else {
          options <- character(0)
        }
        
        # ✅ **Remove ALL old nodes & edges for this step before updating**
        graph_data_1a$nodes <- graph_data_1a$nodes[!grepl(paste0("^", step_id, "_?"), graph_data_1a$nodes$id), ]
        graph_data_1a$edges <- graph_data_1a$edges[graph_data_1a$edges$from != step_id & graph_data_1a$edges$to != step_id, ]
        
        # ✅ **Add step node**
        graph_data_1a$nodes <- rbind(
          graph_data_1a$nodes,
          data.frame(id = step_id, label = step_name, shape = "circle", stringsAsFactors = FALSE)
        )
        
        # ✅ **Add option nodes & edges only if options exist**
        if (length(options) > 0) {
          option_nodes <- data.frame(
            id = names(options), 
            label = options, 
            shape = "box", 
            stringsAsFactors = FALSE
          )
          
          graph_data_1a$nodes <- rbind(graph_data_1a$nodes, option_nodes)
          graph_data_1a$edges <- rbind(
            graph_data_1a$edges,
            data.frame(from = step_id, to = option_nodes$id, stringsAsFactors = FALSE)
          )
        }
        
        # ✅ **Update Accordion Title Dynamically**
        runjs(sprintf(
          "$('#heading_%d .panel-title a').text('%s');",
          step_id,
          step_name
        ))
        
        print(paste("DEBUG: Updated graph for Step", step_id, "with options:", paste(options, collapse = ", ")))
        
      }, ignoreInit = TRUE)
    })
  })
  
  # Render the visNetwork graph
  output$pipeline_network_1a <- renderVisNetwork({
    visNetwork(graph_data_1a$nodes, graph_data_1a$edges) %>%
      visEdges(arrows = "to", color = "#000000") %>%
      visLayout(hierarchical = TRUE) %>%
      visPhysics(enabled = TRUE) %>%
      visInteraction(dragNodes = TRUE)
  })

############################################################## 
  
############### Step 1b: Define Defensibility ################
  
  graph_data_1b <- reactiveValues(
    nodes = data.frame(id = numeric(0), label = character(0), shape = character(0), color = character(0), stringsAsFactors = FALSE),
    edges = data.frame(from = numeric(0), to = numeric(0), stringsAsFactors = FALSE)
  )
  
  # Sync Step 1b network with Step 1a whenever Step 1b is accessed
  observe({
    if (!is.null(graph_data_1a$nodes) && nrow(graph_data_1a$nodes) > 0) {
      graph_data_1b$nodes <- graph_data_1a$nodes
      graph_data_1b$edges <- graph_data_1a$edges
    }
    print("DEBUG: Synced Step 1b network with Step 1a")
  })
  
  # ✅ Fix: Ensure `justifications` exists to prevent errors
  justifications <- reactiveValues()
  
  # ✅ Unified storage for ALL step & option justifications and defensibility inputs
  stored_inputs <- reactiveValues(
    step_justifications = list(),
    option_justifications = list(),
    step_defensibility = list(),
    option_defensibility = list()
  )
  
  # ✅ Observe and store step justifications **independently**
  observe({
    req(input)
    for (step_id in 1:step_count()) {
      justification_key <- paste0("justification_step_", step_id)
      justification_input <- input[[justification_key]]
      
      if (!is.null(justification_input)) {
        stored_inputs$step_justifications[[justification_key]] <- justification_input
        justifications[[justification_key]] <- justification_input  # 🔥 Fix: Restore justifications
        print(paste("DEBUG: Stored justification for", justification_key, "->", justification_input))
      }
    }
  })
  
  # ✅ Observe and store option justifications **independently**
  observe({
    req(input)
    for (step_id in 1:step_count()) {
      if (!is.null(step_options[[as.character(step_id)]])) {
        for (option_id in seq_along(step_options[[as.character(step_id)]])) {
          option_key <- paste0("justification_option_", step_id, "_", option_id)
          option_input <- input[[option_key]]
          
          if (!is.null(option_input)) {
            stored_inputs$option_justifications[[option_key]] <- option_input
            justifications[[option_key]] <- option_input  # 🔥 Fix: Restore justifications
            print(paste("DEBUG: Stored justification for", option_key, "->", option_input))
          }
        }
      }
    }
  })
  
  # ✅ Observe and store step defensibility **independently**
  observe({
    req(input)
    for (step_id in 1:step_count()) {
      defensibility_key <- paste0("defensibility_step_", step_id)
      defensibility_input <- input[[defensibility_key]]
      
      if (!is.null(defensibility_input)) {
        stored_inputs$step_defensibility[[defensibility_key]] <- defensibility_input
        print(paste("DEBUG: Stored defensibility for", defensibility_key, "->", defensibility_input))
      }
    }
  })
  
  # ✅ Observe and store option defensibility **independently**
  observe({
    req(input)
    for (step_id in 1:step_count()) {
      if (!is.null(step_options[[as.character(step_id)]])) {
        for (option_id in seq_along(step_options[[as.character(step_id)]])) {
          option_def_key <- paste0("defensibility_option_", step_id, "_", option_id)
          option_def_input <- input[[option_def_key]]
          
          if (!is.null(option_def_input)) {
            stored_inputs$option_defensibility[[option_def_key]] <- option_def_input
            print(paste("DEBUG: Stored defensibility for", option_def_key, "->", option_def_input))
          }
        }
      }
    }
  })
  
  # ✅ Print all stored inputs periodically for debugging
  observe({
    print("DEBUG: Stored Inputs Snapshot ->")
    print(stored_inputs)
  })
  
  # ✅ UI Rendering for Step 1b with independent inputs
  observe({
    removeUI(selector = "#accordion_container_step1b > *", multiple = TRUE)
    
    lapply(1:step_count(), function(step_id) {
      step_key <- as.character(step_id)
      
      if (!is.null(input[[paste0("step_name_", step_id)]])) {
        step_name <- input[[paste0("step_name_", step_id)]]
        
        insertUI(
          selector = "#accordion_container_step1b",
          where = "beforeEnd",
          ui = div(
            class = "panel panel-default",
            div(
              class = "panel-heading",
              role = "tab",
              id = paste0("heading_step1b_", step_id),
              h4(
                class = "panel-title",
                tags$a(
                  href = paste0("#collapse_step1b_", step_id),
                  "data-toggle" = "collapse",
                  "data-parent" = "#accordion_container_step1b",
                  "aria-expanded" = "false",
                  "aria-controls" = paste0("collapse_step1b_", step_id),
                  step_name
                )
              )
            ),
            div(
              id = paste0("collapse_step1b_", step_id),
              class = "panel-collapse collapse",
              role = "tabpanel",
              div(
                class = "panel-body",
                
                # Step Defensibility
                div(
                  h4(strong(step_name)),
                  radioButtons(
                    paste0("defensibility_step_", step_id),
                    label = "Classify Node:",
                    choices = c("Defensible", "Indefensible"),
                    selected = FALSE,
                    inline = TRUE
                  ),
                  textInput(paste0("justification_step_", step_id), "", placeholder = "Enter justification...")
                ),
                
                # Options Defensibility
                div(
                  id = paste0("options_defensibility_container_", step_id),
                  style = "margin-top: 10px;"
                )
              )
            )
          )
        )
        
        # Add dynamic options defensibility inputs
        lapply(1:length(step_options[[step_key]]), function(option_id) {
          if (!is.null(input[[paste0("option_", step_id, "_", option_id)]])) {
            option_name <- input[[paste0("option_", step_id, "_", option_id)]]
            
            insertUI(
              selector = paste0("#options_defensibility_container_", step_id),
              where = "beforeEnd",
              ui = div(
                h5(strong(option_name)),
                radioButtons(
                  paste0("defensibility_option_", step_id, "_", option_id),
                  label = "Classify Option:",
                  choices = c("Defensible", "Indefensible"),
                  selected = FALSE,
                  inline = TRUE
                ),
                textInput(
                  paste0("justification_option_", step_id, "_", option_id),
                  "",
                  placeholder = "Enter justification..."
                )
              )
            )
          }
        })
      }
    })
  })
  
  # Reactive value to store defensible steps and options
  defensible_steps_reactive <- reactiveVal(list())  # ✅ Stores defensible steps
  defensible_options_reactive <- reactiveVal(list())
  
  # Observe changes in Step 1b and update defensible steps
  observe({
    defensible_steps <- list()  # Store defensible steps
    defensible_options <- list()  # Store defensible options
    
    print("DEBUG: Running Step 1b Defensibility Check") 
    print(paste("Total Steps:", step_count()))  
    
    for (step_id in 1:step_count()) {
      step_key <- as.character(step_id)
      
      step_name_input <- input[[paste0("step_name_", step_id)]]
      step_defensibility_input <- input[[paste0("defensibility_step_", step_id)]]
      
      print(paste("DEBUG: Checking Step", step_id, "->", step_name_input))  
      print(paste("DEBUG: Step Defensibility:", step_defensibility_input))  
      
      if (!is.null(step_name_input) && step_name_input != "" &&
          !is.null(step_defensibility_input) && step_defensibility_input == "Defensible") {
        
        print(paste("DEBUG: Adding Defensible Step:", step_name_input))  
        defensible_steps[[as.character(step_id)]] <- step_name_input  # ✅ Store defensible step
        
        # ✅ Now check the step’s options
        step_options_defensible <- list()  # Store defensible options for this step
        
        # ✅ Observe and store option justifications
        observe({
          lapply(1:step_count(), function(step_id) {
            step_key <- as.character(step_id)
            
            if (!is.null(step_options[[step_key]])) {
              for (option_id in seq_along(step_options[[step_key]])) {
                observeEvent(input[[paste0("justification_option_", step_id, "_", option_id)]], {
                  justifications[[paste0("option_", step_id, "_", option_id)]] <- input[[paste0("justification_option_", step_id, "_", option_id)]]
                  print(paste("DEBUG: Stored justification for Option", option_id, "in Step", step_id, "->", input[[paste0("justification_option_", step_id, "_", option_id)]]))
                })
              }
            }
          })
        })
        
        if (!is.null(step_options[[step_key]])) {
          for (option_id in seq_along(step_options[[step_key]])) {
            option_name_input <- input[[paste0("option_", step_id, "_", option_id)]]
            option_defensibility_input <- input[[paste0("defensibility_option_", step_id, "_", option_id)]]
            
            print(paste("DEBUG: Checking Option", option_id, "->", option_name_input))  
            print(paste("DEBUG: Option Defensibility:", option_defensibility_input))  
            
            if (!is.null(option_name_input) && option_name_input != "" &&
                !is.null(option_defensibility_input) && option_defensibility_input == "Defensible") {
              
              print(paste("DEBUG: Adding Defensible Option:", option_name_input))  
              step_options_defensible[[as.character(option_id)]] <- option_name_input  # ✅ Store defensible option
            }
          }
        }
        
        defensible_options[[as.character(step_id)]] <- step_options_defensible  # ✅ Store step’s defensible options
      }
    }
    
    # ✅ Store both steps and options in reactive values
    defensible_steps_reactive(defensible_steps)
    defensible_options_reactive(defensible_options)
    
    # ✅ Print debugging logs
    print("DEBUG: Updated defensible_steps_reactive()")
    print(defensible_steps_reactive())
    print("DEBUG: Updated defensible_options_reactive()")
    print(defensible_options_reactive())
  })
  
  observe({
    lapply(1:step_count(), function(step_id) {
      step_key <- as.character(step_id)  # Define step_key inside the loop
      
      # Update the step node color
      observeEvent(input[[paste0("defensibility_step_", step_id)]], {
        selected_value <- input[[paste0("defensibility_step_", step_id)]]
        
        # Determine color based on selection
        new_color <- if (selected_value == "Defensible") {
          "#5cb85c"  # Green
        } else {
          "#d9534f"  # Red
        }
        
        # Update step node color
        graph_data_1b$nodes$color[graph_data_1b$nodes$id == step_id] <- new_color
        
        # Update visualization
        output$pipeline_network_step1b <- renderVisNetwork({
          visNetwork(graph_data_1b$nodes, graph_data_1b$edges) %>%
            visEdges(arrows = "to", color = "#000000") %>%
            visNodes(color = list(background = graph_data_1b$nodes$color, border = "#000000")) %>%
            visLayout(hierarchical = TRUE) %>%
            visPhysics(enabled = TRUE) %>%
            visInteraction(dragNodes = TRUE)
        })
      }, ignoreInit = TRUE)
      
      # Update option node colors
      lapply(seq_along(step_options[[step_key]]), function(option_id) {
        observeEvent(input[[paste0("defensibility_option_", step_id, "_", option_id)]], {
          selected_value <- input[[paste0("defensibility_option_", step_id, "_", option_id)]]
          
          # Determine color
          new_color <- if (selected_value == "Defensible") {
            "#5cb85c"  # Green
          } else {
            "#d9534f"  # Red
          }
          
          # Find the option node ID
          option_node_id <- paste0(step_id, "_", option_id)
          
          # Update option node color
          graph_data_1b$nodes$color[graph_data_1b$nodes$id == option_node_id] <- new_color
          
          # Update visualization
          output$pipeline_network_step1b <- renderVisNetwork({
            visNetwork(graph_data_1b$nodes, graph_data_1b$edges) %>%
              visEdges(arrows = "to", color = "#000000") %>%
              visNodes(color = list(background = graph_data_1b$nodes$color, border = "#000000")) %>%
              visLayout(hierarchical = TRUE) %>%
              visPhysics(enabled = TRUE) %>%
              visInteraction(dragNodes = TRUE)
          })
        }, ignoreInit = TRUE)
      })
    })
  })
  
  output$pipeline_network_step1b <- renderVisNetwork({
    visNetwork(graph_data_1b$nodes, graph_data_1b$edges) %>%
      visEdges(arrows = "to", color = "#000000") %>%
      visNodes(color = list(background = graph_data_1b$nodes$color)) %>%
      visLayout(hierarchical = TRUE) %>%
      visPhysics(enabled = TRUE) %>%
      visInteraction(dragNodes = TRUE)
  })
  
  
##############################################################
  
############ Step 1c: Create Defensible Pipelines ############
  
  # Reactive values to store copied selections and working pipelines
  copied_selection <- reactiveVal(NULL)
  pipelines <- reactiveVal(list())
  saved_pipelines <- reactiveValues(data = list())
  
  # ✅ Track which pipeline is currently active (expanded)
  active_pipeline <- reactiveVal(NULL)
  
  observeEvent(input$active_pipeline, {
    print(paste("DEBUG: Active Pipeline Changed ->", input$active_pipeline))
    
    pipeline_id <- input$active_pipeline
    
    # ✅ Reset clicked selection when switching pipelines
    clicked_selection(character(0))
    
    print("DEBUG: Clicked Items Reset for new pipeline")
    
    # ✅ Restore saved pipeline order if it exists
    if (!is.null(saved_pipelines$data[[pipeline_id]])) {
      print(paste("DEBUG: Restoring saved pipeline for", pipeline_id))
      restored_pipeline <- saved_pipelines$data[[pipeline_id]]
      
      # ✅ Correctly update the pipelines() reactive value
      updated_pipelines <- pipelines()
      
      # ❌ This line is triggering a reactivity loop leading to unwanted notifications
      updated_pipelines[[pipeline_id]] <- restored_pipeline
      
      # ✅ FIX: Prevent reactive assignment if data hasn't changed
      if (!identical(updated_pipelines[[pipeline_id]], pipelines()[[pipeline_id]])) {
        pipelines(updated_pipelines)
      }
    }
    
    # ✅ Restore visual network if it exists
    if (!is.null(saved_pipelines$data[[paste0("graph_", pipeline_id)]])) {
      print(paste("DEBUG: Restoring saved visual network for", pipeline_id))
      restored_graph <- saved_pipelines$data[[paste0("graph_", pipeline_id)]]
      pipeline_graph_data(restored_graph)
    }
  })
  
  # Function to create a new pipeline accordion
  create_pipeline_accordion <- function(pipeline_id) {
    print(paste("DEBUG: Creating Pipeline Accordion for", pipeline_id))  # ✅ Debugging
    
    defensible_steps <- defensible_steps_reactive()
    defensible_options <- defensible_options_reactive()
    
    if (length(defensible_steps) == 0) {
      showNotification("No defensible steps found!", type = "warning")
      print("DEBUG: No defensible steps to create pipeline")  # ✅ Confirm step check
      return(NULL)
    }
    
    bucket_list_items <- list()
    for (step_id in names(defensible_steps)) {
      step_name <- defensible_steps[[step_id]]
      bucket_list_items[[step_id]] <- step_name
      
      if (!is.null(defensible_options[[step_id]])) {
        for (option_id in names(defensible_options[[step_id]])) {
          option_name <- defensible_options[[step_id]][[option_id]]
          bucket_list_items[[paste0(step_id, "_", option_id)]] <- paste("   ➝", option_name)
        }
      }
    }
    
    print("DEBUG: Bucket list items for new pipeline ->")
    print(bucket_list_items)  # ✅ Ensure bucket list is populated
    
    return(
      div(
        class = "panel panel-default",
        div(
          class = "panel-heading",
          role = "tab",
          id = paste0("heading_", pipeline_id),
          h4(
            class = "panel-title",
            tags$a(
              id = paste0("toggle_", pipeline_id),
              href = paste0("#collapse_", pipeline_id),
              "data-toggle" = "collapse",
              "data-parent" = "#main_pipeline_accordion",
              "aria-expanded" = "false",
              "aria-controls" = paste0("collapse_", pipeline_id),
              paste("Pipeline", gsub("pipeline_", "", pipeline_id)),
              onclick = sprintf("Shiny.setInputValue('active_pipeline', '%s', {priority: 'event'})", pipeline_id)
            )
          )
        ),
        div(
          id = paste0("collapse_", pipeline_id),
          class = "panel-collapse collapse",
          role = "tabpanel",
          div(
            class = "panel-body",
            
            # ✅ Delete Pipeline Button
            div(
              style = "margin-bottom: 10px; text-align: right;",
              actionButton(paste0("delete_pipeline_", pipeline_id), "Delete Pipeline", class = "btn-danger")
            ),
            
            # Copy & Paste Buttons
            div(
              style = "margin-bottom: 10px;",
              actionButton(paste0("copy_pipeline_", pipeline_id), "Copy Selection", class = "btn-info"),
              actionButton(paste0("paste_pipeline_", pipeline_id), "Paste Selection", class = "btn-warning")
            ),
            
            bucket_list(
              header = "Drag and Drop Steps & Options",
              group_name = paste0("pipeline_bucket_", pipeline_id),
              orientation = "horizontal",
              
              add_rank_list(
                input_id = paste0("available_steps_", pipeline_id),
                text = "Available Steps & Options",
                labels = unname(bucket_list_items),
                options = sortable_options(multiDrag = TRUE)
              ),
              
              add_rank_list(
                input_id = paste0("selected_steps_", pipeline_id),
                text = "Pipeline Order (Drag Here)",
                labels = pipelines()[[pipeline_id]] %||% character(0),
                options = sortable_options(multiDrag = TRUE),
                class = paste0("rank-list-clickable-", pipeline_id)  # ✅ Add a unique class to track clicks
              )
            ),
            
            actionButton(paste0("save_pipeline_", pipeline_id), "Save Pipeline", class = "btn-success")
          )
        )
      )
    )
  }
  
  # Add a new pipeline (dynamically creates independent accordions)
  observeEvent(input$new_pipeline, {
    print("DEBUG: New Pipeline button clicked")
    
    # ✅ Get all existing pipeline names and extract numeric part
    existing_pipeline_ids <- names(pipelines())
    existing_numbers <- as.numeric(gsub("pipeline_", "", existing_pipeline_ids))
    
    # ✅ Determine the next pipeline number (start from 1)
    next_number <- ifelse(length(existing_numbers) > 0, max(existing_numbers, na.rm = TRUE) + 1, 1)
    new_pipeline_id <- paste0("pipeline_", next_number)
    
    print(paste("DEBUG: New Pipeline ID ->", new_pipeline_id))
    
    step_data <- defensible_steps_reactive()
    if (length(step_data) == 0) {
      showNotification("No defensible steps found!", type = "warning")
      print("DEBUG: No defensible steps available")
      return()
    }
    
    # ✅ Ensure pipelines() is initialized before modification
    existing_pipelines <- pipelines()
    if (is.null(existing_pipelines)) existing_pipelines <- list()
    
    existing_pipelines[[new_pipeline_id]] <- list()
    pipelines(existing_pipelines)  # ✅ Store new pipeline
    
    print("DEBUG: Pipelines() after adding new pipeline ->")
    print(pipelines())
    
    # ✅ Insert pipeline accordion without removing existing ones
    insertUI(
      selector = "#main_pipeline_accordion",
      where = "beforeEnd",
      ui = create_pipeline_accordion(new_pipeline_id)
    )
    
    # ✅ Automatically expand new pipeline accordion
    runjs(sprintf("$('#toggle_%s').click();", new_pipeline_id))
    
    print("DEBUG: Pipeline UI inserted successfully")
  })
  
  # ✅ Handle Copying Selection 
  clicked_selection <- reactiveVal(character(0))
  
  observeEvent(input$clicked_pipeline_item, {
    selected_now <- input$clicked_pipeline_item
    
    if (!is.null(selected_now) && selected_now != "") {
      prev_selection <- clicked_selection()  # ✅ Store previous clicks
      new_selection <- unique(c(prev_selection, selected_now))  # ✅ Append new item
      
      clicked_selection(new_selection)  # ✅ Update reactive value
      
      print("DEBUG: Clicked Items Updated ->")
      print(new_selection)
    }
  })
  
  observeEvent(input[[paste0("copy_pipeline_", input$active_pipeline)]], {
    req(input$active_pipeline)
    
    selected_to_copy <- isolate(clicked_selection())
    
    if (is.null(selected_to_copy) || length(selected_to_copy) == 0) {
      showNotification("No items selected to copy!", type = "warning")
      return()
    }
    
    copied_selection(selected_to_copy)
    
    print(paste("DEBUG: Copied selection for", input$active_pipeline, "->"))
    print(selected_to_copy)
    
    showNotification("Selected items copied successfully!", type = "message")
  }, ignoreInit = TRUE)
  
  # ✅ Handle Pasting Selection 
  observeEvent(input[[paste0("paste_pipeline_", input$active_pipeline)]], {
    req(input$active_pipeline, copied_selection())
    
    pipeline_id <- input$active_pipeline
    existing_pipelines <- pipelines()
    
    if (!is.null(existing_pipelines[[pipeline_id]])) {
      # ✅ Get current selection inside the active pipeline
      current_selection <- input[[paste0("selected_steps_", pipeline_id)]]
      
      # ✅ Append copied items (ensuring no duplicates)
      new_selection <- unique(c(current_selection, copied_selection()))
      
      print("DEBUG: New Selection After Paste ->")
      print(new_selection)
      
      # ✅ Update the pipeline's stored selection
      existing_pipelines[[pipeline_id]] <- new_selection
      pipelines(existing_pipelines)
      
      # ✅ Preserve Available Steps & Options but REMOVE pasted items
      available_steps <- defensible_steps_reactive()
      available_options <- defensible_options_reactive()
      
      bucket_list_items <- list()
      for (step_id in names(available_steps)) {
        step_name <- available_steps[[step_id]]
        
        # ✅ Remove pasted steps
        if (!(step_name %in% new_selection)) {
          bucket_list_items[[step_id]] <- step_name
        }
        
        if (!is.null(available_options[[step_id]])) {
          for (option_id in names(available_options[[step_id]])) {
            option_name <- available_options[[step_id]][[option_id]]
            
            # ✅ Remove pasted options
            # ✅ Strip "→ " prefix before checking if the option was pasted
            cleaned_option_name <- gsub("^\\s*➝\\s*", "", option_name)
            cleaned_new_selection <- gsub("^\\s*➝\\s*", "", new_selection)
            
            if (!(cleaned_option_name %in% cleaned_new_selection)) {
              bucket_list_items[[paste0(step_id, "_", option_id)]] <- paste("   ➝", option_name)
            }
          }
        }
      }
      
      # ✅ Remove UI for bucket list and reinsert with updated selection
      removeUI(selector = paste0("#collapse_", pipeline_id))
      
      insertUI(
        selector = paste0("#heading_", pipeline_id),
        where = "afterEnd",
        ui = div(
          id = paste0("collapse_", pipeline_id),
          class = "panel-collapse collapse in",
          role = "tabpanel",
          div(
            class = "panel-body",
            
            # ✅ Ensure Delete Button is included even after pasting
            div(
              style = "margin-bottom: 10px; text-align: right;",
              actionButton(paste0("delete_pipeline_", pipeline_id), "Delete Pipeline", class = "btn-danger")
            ),
            
            # Copy & Paste Buttons
            div(
              style = "margin-bottom: 10px;",
              actionButton(paste0("copy_pipeline_", pipeline_id), "Copy Selection", class = "btn-info"),
              actionButton(paste0("paste_pipeline_", pipeline_id), "Paste Selection", class = "btn-warning")
            ),
            
            bucket_list(
              header = "Drag and Drop Steps & Options",
              group_name = paste0("pipeline_bucket_", pipeline_id),
              orientation = "horizontal",
              
              add_rank_list(
                input_id = paste0("available_steps_", pipeline_id),
                text = "Available Steps & Options",
                labels = unname(bucket_list_items),  # ✅ Updated to exclude pasted items
                options = sortable_options(multiDrag = TRUE)
              ),
              
              add_rank_list(
                input_id = paste0("selected_steps_", pipeline_id),
                text = "Pipeline Order (Drag Here)",
                labels = new_selection,  # ✅ Paste only copied selection
                options = sortable_options(multiDrag = TRUE)
              )
            ),
            
            actionButton(paste0("save_pipeline_", pipeline_id), "Save Pipeline", class = "btn-success")
          )
        )
      )
      
      showNotification("Selection pasted successfully!", type = "message")
    } else {
      showNotification("Error: Pipeline not found!", type = "error")
    }
  })
  
  # ✅ Handle Save Pipeline Logic 
  observeEvent(input[[paste0("save_pipeline_", input$active_pipeline)]], {
    req(input$active_pipeline)
    
    pipeline_id <- isolate(input$active_pipeline)
    existing_pipelines <- pipelines()
    
    print("DEBUG: Attempting to Save Pipeline")
    print(paste("Pipeline ID:", pipeline_id))
    
    selected_steps <- isolate(input[[paste0("selected_steps_", pipeline_id)]])  
    
    if (is.null(selected_steps) || length(selected_steps) == 0) {
      showNotification("Error: Cannot save an empty pipeline!", type = "error")
      return()
    }
    
    print("Existing Pipeline Data:")
    print(selected_steps)
    
    # ✅ Allow overwriting saved pipeline
    existing_pipelines[[pipeline_id]] <- selected_steps
    pipelines(existing_pipelines)
    
    # ✅ Store pipeline data in saved_pipelines
    saved_pipelines$data[[pipeline_id]] <- selected_steps  # Save pipeline order
    
    # ✅ Save visual network data
    saved_pipeline_graph <- pipeline_graph_data()  # Capture network state
    saved_pipelines$data[[paste0("graph_", pipeline_id)]] <- saved_pipeline_graph
    
    print("DEBUG: Successfully saved pipeline ->")
    print(saved_pipelines$data)
    
    showNotification(paste("Pipeline", gsub("pipeline_", "", pipeline_id), "saved successfully!"), type = "message")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Handle Delete Pipeline
  # ✅ Track which pipeline is selected for deletion
  pipeline_to_delete <- reactiveVal(NULL)
  
  # ✅ Observe delete button clicks
  observe({
    for (pipeline_id in names(pipelines())) {
      observeEvent(input[[paste0("delete_pipeline_", pipeline_id)]], {
        # ✅ Prevent multiple triggers
        if (!is.null(pipeline_to_delete())) return()
        
        print(paste("DEBUG: Delete button clicked for pipeline ->", pipeline_id))
        
        # ✅ Set the pipeline ID to be deleted
        pipeline_to_delete(pipeline_id)
        
        # ✅ Show confirmation modal
        showModal(
          modalDialog(
            title = "Confirm Pipeline Deletion",
            p(paste("Are you sure you want to delete", gsub("pipeline_", "Pipeline ", pipeline_id), "?")),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("confirm_delete", "Delete", class = "btn-danger")
            )
          )
        )
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }
  })
  
  # ✅ Handle confirmed deletion
  observeEvent(input$confirm_delete, {
    req(pipeline_to_delete(), cancelOutput = TRUE)  # ✅ Ensure valid input
    
    pipeline_id <- pipeline_to_delete()
    print(paste("DEBUG: Confirmed deletion of", pipeline_id))
    
    # ✅ Check if the pipeline exists before deleting
    existing_pipelines <- pipelines()
    saved_data <- saved_pipelines$data
    
    if (is.null(existing_pipelines[[pipeline_id]])) {
      print(paste("WARNING: Tried to delete non-existent pipeline:", pipeline_id))
      removeModal()
      pipeline_to_delete(NULL)  # Reset selection
      return()
    }
    
    # ✅ Remove pipeline from reactive lists
    existing_pipelines[[pipeline_id]] <- NULL
    pipelines(existing_pipelines)
    
    if (!is.null(saved_data[[pipeline_id]])) {
      saved_data[[pipeline_id]] <- NULL
      saved_pipelines$data <- saved_data
    }
    
    # ✅ Dynamically remove pipeline UI safely
    removeUI(selector = paste0("#heading_", pipeline_id))
    removeUI(selector = paste0("#collapse_", pipeline_id))
    
    # ✅ Reset active pipeline if it was the one deleted
    if (!is.null(active_pipeline()) && active_pipeline() == pipeline_id) {
      active_pipeline(NULL)
    }
    
    # ✅ Reset deletion tracking to avoid infinite loops
    pipeline_to_delete(NULL)
    
    # ✅ Close modal and show success message
    removeModal()
    showNotification(paste("Pipeline", gsub("pipeline_", "", pipeline_id), "deleted successfully!"), type = "message")
    
    print("DEBUG: Pipeline successfully removed")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # Reactive value to store pipeline graph data
  pipeline_graph_data <- reactiveVal(list(nodes = data.frame(), edges = data.frame()))
  
  # Update the network when steps/options are added to the pipeline
  observe({
    req(input$active_pipeline)
    pipeline_id <- input$active_pipeline
    
    all_pipelines <- pipelines()
    
    nodes <- data.frame(id = character(), label = character(), color = character(), shape = character(), level = numeric(), stringsAsFactors = FALSE)
    edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
    
    selected_items <- input[[paste0("selected_steps_", pipeline_id)]]
    
    if (!is.null(selected_items) && length(selected_items) > 0) {
      prev_selected_option <- NULL
      level_counter <- 1
      
      for (i in seq_along(selected_items)) {
        step_label <- selected_items[i]
        
        step_id <- names(defensible_steps_reactive())[defensible_steps_reactive() == step_label]
        if (length(step_id) == 0) next
        
        step_name <- defensible_steps_reactive()[[step_id]]
        options <- defensible_options_reactive()[[step_id]]
        
        nodes <- rbind(nodes, data.frame(
          id = paste0(pipeline_id, "_", step_id),
          label = step_name,
          color = "#5cb85c",
          shape = "circle",
          level = level_counter
        ))
        
        option_selected <- NULL
        if (!is.null(options)) {
          option_level <- level_counter + 1
          
          for (option_id in names(options)) {
            option_name <- options[[option_id]]
            option_node_id <- paste0(pipeline_id, "_", step_id, "_", option_id)
            
            cleaned_selected_items <- gsub("➝ ", "", selected_items)
            option_color <- ifelse(option_name %in% cleaned_selected_items, "#5cb85c", "#d3d3d3")
            
            nodes <- rbind(nodes, data.frame(
              id = option_node_id,
              label = option_name,
              color = option_color,
              shape = "box",
              level = option_level
            ))
            
            edges <- rbind(edges, data.frame(from = paste0(pipeline_id, "_", step_id), to = option_node_id))
            if (option_color == "#5cb85c") option_selected <- option_node_id
          }
        }
        
        if (!is.null(prev_selected_option)) {
          # ✅ If previous step had a selected option, connect this step FROM that option
          edges <- rbind(edges, data.frame(from = prev_selected_option, to = paste0(pipeline_id, "_", step_id)))
        } 
        
        if (is.null(options) || length(options) == 0) {
          # ✅ If this step has NO options, connect it TO the next step
          if (i < length(selected_items)) {
            next_step_id <- paste0(pipeline_id, "_", selected_items[i + 1])
            edges <- rbind(edges, data.frame(from = paste0(pipeline_id, "_", step_id), to = next_step_id))
          }
        }
        
        # ✅ Update prev_selected_option to the currently selected option (if any)
        prev_selected_option <- if (!is.null(option_selected)) option_selected else paste0(pipeline_id, "_", step_id)
        level_counter <- if (!is.null(option_selected)) option_level + 1 else level_counter + 2
      }
    }
    
    pipeline_graph_data(list(nodes = nodes, edges = edges))
  })
  
  # Render the visual network
  output$pipeline_network_1c <- renderVisNetwork({
    req(input$active_pipeline)
    graph_data <- pipeline_graph_data()
    
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visNodes(font = list(size = 20), borderWidth = 2) %>%
      visEdges(arrows = "to", color = list(color = "#000000")) %>%
      visLayout(hierarchical = TRUE)
  })
  
##############################################################  
  
################ Step 1d: Your Multiverse 1.0 ################
  
  # ✅ Dynamically update dropdown choices when pipelines are saved
  observe({
    req(length(saved_pipelines$data) > 0)  # Ensure pipelines exist
    
    # Extract pipeline names (exclude graph-related entries)
    pipeline_ids <- names(saved_pipelines$data)
    pipeline_ids <- pipeline_ids[!grepl("^graph_", pipeline_ids)]
    
    # Update dropdown choices
    updateSelectInput(session, "selected_pipeline", choices = pipeline_ids, selected = NULL)
  })
  
  # ✅ Render table based on selected pipeline
  output$pipeline_table <- DT::renderDataTable({
    req(input$selected_pipeline)  # Ensure a selection is made
    
    pipeline_data <- saved_pipelines$data[[input$selected_pipeline]]  # Get selected pipeline data
    
    # Convert to DataFrame (Step/Option column)
    df <- data.frame(
      Pipeline = pipeline_data,
      stringsAsFactors = FALSE
    )
    
    DT::datatable(df, options = list(pageLength = 50, dom = 't'))
  })
  
  observe({
    req(length(saved_pipelines$data) > 0)
    
    print("DEBUG: saved_pipelines$data structure:")
    print(str(saved_pipelines$data))  # Shows structure while app is running
  })
  
  # ✅ Render network visualization based on selected pipeline
  output$selected_pipeline_network <- renderVisNetwork({
    req(input$selected_pipeline)  # Ensure a pipeline is selected
    
    selected_pipeline <- isolate(input$selected_pipeline)
    if (length(selected_pipeline) != 1 || is.null(selected_pipeline)) {
      return(NULL)  # Ensure it's a single valid selection
    }
    
    graph_key <- paste0("graph_", selected_pipeline)
    if (!graph_key %in% names(saved_pipelines$data)) {
      return(NULL)  # Handle missing graph data gracefully
    }
    
    graph_data <- saved_pipelines$data[[graph_key]]
    if (is.null(graph_data) || is.null(graph_data$nodes) || is.null(graph_data$edges)) {
      return(NULL)
    }
    
    # ✅ Display the network in the app
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visNodes(font = list(size = 20), borderWidth = 2) %>%
      visEdges(arrows = "to", color = list(color = "#000000")) %>%
      visLayout(hierarchical = TRUE)
  })
  
  # ✅ Download Preprocessing Documentation 
  output$download_preprocessing_doc <- downloadHandler(
    filename = function() {
      nickname <- input$user_nickname
      if (is.null(nickname) || nickname == "") {
        paste0("Preprocessing_Documentation_", Sys.Date(), ".zip")
      } else {
        paste0(nickname, "_Preprocessing_Documentation_", Sys.Date(), ".zip")
      }
    },
    content = function(file) {
      
      showModal(modalDialog(
        title = "Generating download...",
        "This may take a while depending on the number of pipelines.",
        easyClose = FALSE,
        footer = NULL
      ))
      
      nickname <- input$user_nickname
      correct_filename <- if (is.null(nickname) || nickname == "") {
        paste0("Preprocessing_Documentation_", Sys.Date(), ".zip")
      } else {
        paste0(nickname, "_Preprocessing_Documentation_", Sys.Date(), ".zip")
      }
      
      tempDir <- file.path(tempdir(), "preprocessing_doc")
      dir.create(tempDir, showWarnings = FALSE)
      tempReport <- file.path(tempDir, "Preprocessing_Documentation.Rmd")
      pdf_file <- file.path(tempDir, "Preprocessing_Documentation.pdf")
      
      aspects <- c()
      if (user_inputs$aspect1a_1) aspects <- c(aspects, paste("* Measurement:", user_inputs$aspect1a_text_1))
      if (user_inputs$aspect1a_2) aspects <- c(aspects, paste("* Preprocessing:", user_inputs$aspect1a_text_2))
      if (user_inputs$aspect1a_3) aspects <- c(aspects, paste("* Model Specification:", user_inputs$aspect1a_text_3))
      if (user_inputs$aspect1a_4) aspects <- c(aspects, paste("* Estimation Methods:", user_inputs$aspect1a_text_4))
      if (length(aspects) == 0) aspects <- "No aspects selected."
      aspects_text <- paste(aspects, collapse = "\n")
      
      methods <- c()
      if (user_inputs$defens1a_1) methods <- c(methods, paste("* Literature Review:", user_inputs$defens1a_text_1))
      if (user_inputs$defens1a_2) methods <- c(methods, paste("* Expertise:", user_inputs$defens1a_text_2))
      if (user_inputs$defens1a_3) methods <- c(methods, paste("* Crowdsourcing:", user_inputs$defens1a_text_3))
      if (user_inputs$defens1a_4) methods <- c(methods, paste("* Other methods:", user_inputs$defens1a_text_4))
      if (length(methods) == 0) methods <- "No identification method for defensibility provided."
      methods_text <- paste(methods, collapse = "\n")
      
      decisions <- c()
      for (step_id in names(reactiveValuesToList(step_options))) {
        step_name <- input[[paste0("step_name_", step_id)]]
        decisions <- c(decisions, paste0("**Decision Node:** ", step_name))
        options <- c()
        step_opts <- reactiveValuesToList(step_options)[[step_id]]
        if (length(step_opts) > 0) {
          for (opt_id in names(step_opts)) {
            opt_name <- input[[paste0("option_", step_id, "_", opt_id)]]
            options <- c(options, paste0("- ", opt_name))
          }
          decisions <- c(decisions, "**Options:**", options)
        } else {
          decisions <- c(decisions, "  No options listed.")
        }
        decisions <- c(decisions, "")
      }
      if (length(decisions) == 0) decisions <- "No decision nodes recorded."
      decision_text <- paste(decisions, collapse = "\n")
      
      defensibility <- c()
      for (step_id in names(reactiveValuesToList(step_options))) {
        step_name <- input[[paste0("step_name_", step_id)]]
        step_def <- input[[paste0("defensibility_step_", step_id)]]
        step_just <- input[[paste0("justification_step_", step_id)]]
        defensibility <- c(defensibility, 
                           paste0("**", step_name, ": ", step_def, "** Justification: ", step_just),
                           "")  # Blank line for spacing
        
        step_opts <- reactiveValuesToList(step_options)[[step_id]]
        if (length(step_opts) > 0) {
          for (opt_id in names(step_opts)) {
            opt_name <- input[[paste0("option_", step_id, "_", opt_id)]]
            opt_def <- input[[paste0("defensibility_option_", step_id, "_", opt_id)]]
            opt_just <- input[[paste0("justification_option_", step_id, "_", opt_id)]]
            defensibility <- c(defensibility,
                               paste0("&nbsp;&nbsp;&nbsp;&nbsp;• **", opt_name, ": ", opt_def, "** Justification: ", opt_just),
                               "")  # Blank line after each
          }
        }
      }
      
      if (length(defensibility) == 0) defensibility <- "No defensibility classifications recorded."
      defensibility_text <- paste(defensibility, collapse = "\n")
      
      pipelines <- c()
      counter <- 1
      for (pipeline_name in names(saved_pipelines$data)) {
        if (grepl("^graph_", pipeline_name)) next
        pipeline_data <- saved_pipelines$data[[pipeline_name]]
        line <- paste0("## Pipeline ", counter)
        counter <- counter + 1
        if (!is.null(pipeline_data) && length(pipeline_data) > 0) {
          steps <- c()
          for (step in pipeline_data) {
            step <- gsub("➝", "->", step, fixed = TRUE)
            if (grepl("->", step)) {
              step <- gsub(".*->\\s*", "", step)
            } else {
              step <- paste0("**", step, "**")
            }
            steps <- c(steps, step)
          }
          line <- paste0(line, "\n", paste(steps, collapse = " -> "))
        } else {
          line <- paste0(line, "\n**[No data available for this pipeline]**")
        }
        pipelines <- c(pipelines, line, "")
      }
      if (length(pipelines) == 0) pipelines <- "No pipelines recorded."
      pipelines_text <- paste(pipelines, collapse = "\n")
      
      writeLines(c(
        "---",
        "title: \"Multiverse 1.0\"",
        "author: \"Generated by Systematic Multiverse Analysis Registration Tool\"",
        "date: \"`r format(Sys.time(), '%d %B %Y, %H:%M')`\"",
        "output:",
        "  pdf_document:",
        "    number_sections: false",
        "params:",
        "  aspects_text: ''",
        "  methods_text: ''",
        "  decision_text: ''",
        "  defensibility_text: ''",
        "  pipelines_text: ''",
        "---",
        "",
        "# Scope of the Multiverse",
        "## Aspects of Your Research",
        "`r params$aspects_text`",
        "",
        "## Identification methods of your research decisions",
        "`r params$methods_text`",
        "",
        "## List of the Decision Nodes and Options",
        "`r params$decision_text`",
        "",
        "# Define Defensibility",
        "`r params$defensibility_text`",
        "",
        "# Your Multiverse 1.0",
        "Below are all saved pipelines.\n",
        "`r params$pipelines_text`",
        "",
        "---",
        "Generated by Systematic Multiverse Analysis Registration Tool on `r format(Sys.time(), '%d %B %Y, %H:%M')`"
      ), tempReport)
      
      tryCatch({
        rmarkdown::render(
          input = tempReport,
          output_file = pdf_file,
          output_format = "pdf_document",
          params = list(
            aspects_text = aspects_text,
            methods_text = methods_text,
            decision_text = decision_text,
            defensibility_text = defensibility_text,
            pipelines_text = pipelines_text
          ),
          envir = new.env()
        )
      }, error = function(e) {
        showNotification("\u274C Error generating PDF!", type = "error")
        print(e$message)
        return(NULL)
      })
      
      for (pipeline_name in names(saved_pipelines$data)) {
        graph_key <- paste0("graph_", pipeline_name)
        if (!graph_key %in% names(saved_pipelines$data)) next
        graph_data <- saved_pipelines$data[[graph_key]]
                                           if (is.null(graph_data) || is.null(graph_data$nodes) || is.null(graph_data$edges)) next
                                           network_png <- file.path(tempDir, paste0(pipeline_name, "_network.png"))
                                           html_path <- file.path(tempDir, paste0(pipeline_name, "_network.html"))
                                           htmlwidgets::saveWidget(
                                             visNetwork(graph_data$nodes, graph_data$edges) %>%
                                               visNodes(font = list(size = 20), borderWidth = 2) %>%
                                               visEdges(arrows = "to", color = list(color = "#000000")) %>%
                                               visLayout(hierarchical = TRUE),
                                             html_path, selfcontained = TRUE
                                           )
                                           max_attempts <- 10; attempt <- 1
                                           while (!file.exists(html_path) && attempt <= max_attempts) {
                                             Sys.sleep(1); attempt <- attempt + 1
                                           }
                                           if (file.exists(html_path)) {
                                             webshot2::webshot(
                                               url = paste0("file:///", normalizePath(html_path)),
                                               file = network_png,
                                               vwidth = 1600, vheight = 1000, delay = 3
                                             )
                                           }
      }
      
      for (pipeline_name in names(saved_pipelines$data)) {
        if (grepl('^graph_', pipeline_name)) next
        pipeline_data <- saved_pipelines$data[[pipeline_name]]
        if (!is.null(pipeline_data) && length(pipeline_data) > 0) {
          table_png <- file.path(tempDir, paste0(pipeline_name, "_table.png"))
          tempTableFile <- file.path(tempdir(), paste0(pipeline_name, "_table.png"))
          png(tempTableFile, width = 1200, height = 600, res = 150)
          gridExtra::grid.table(
            data.frame(Pipeline = pipeline_data, stringsAsFactors = FALSE),
            theme = gridExtra::ttheme_default(
              core = list(fg_params = list(cex = 1.5)),
              colhead = list(fg_params = list(cex = 1.8, fontface = "bold"))
            )
          )
          dev.off()
          file.copy(tempTableFile, table_png, overwrite = TRUE)
        }
      }
      
      allowed_files <- list.files(tempDir, pattern = "Preprocessing_Documentation.pdf|_network.png$|_table.png$", full.names = TRUE)
      zip::zipr(zipfile = file, files = allowed_files)
      
      unlink(list.files(tempDir, pattern = "_network.html$", full.names = TRUE))
      unlink(list.files(tempDir, pattern = "_network_files$", full.names = TRUE), recursive = TRUE)
      upload_to_cloud(file, correct_filename)
      
      removeModal()
    }
  )


  # Download Construction Documentation as a ZIP
  output$download_construction_doc <- downloadHandler(
    filename = function() {
      nickname <- input$user_nickname
      if (is.null(nickname) || nickname == "") {
        paste0("Construction_Documentation_", Sys.Date(), ".zip")
      } else {
        paste0(nickname, "_Construction_Documentation_", Sys.Date(), ".zip")
      }
    },
    content = function(file) {
      
      showModal(modalDialog(
        title = "Generating download...",
        "This may take a while depending on the number of pipelines.",
        easyClose = FALSE,
        footer = NULL
      ))
      
      nickname <- input$user_nickname
      correct_filename <- if (is.null(nickname) || nickname == "") {
        paste0("Construction_Documentation_", Sys.Date(), ".zip")
      } else {
        paste0(nickname, "_Construction_Documentation_", Sys.Date(), ".zip")
      }
      
      # ✅ Create a clean temporary directory
      tempDir <- file.path(tempdir(), "pipeline_data")
      dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
      
      ###### ✅ 1. Save Aspects & Defensibility Combined CSV ######
      aspects_defensibility_data <- data.frame(
        Category = c(
          rep("Aspect", 4),
          rep("Defensibility", 4)
        ),
        Name = c(
          "Measurement", "Preprocessing", "Model Specification", "Estimation Methods",
          "Literature Review", "Expertise", "Crowdsourcing", "Other"
        ),
        Checked = c(
          user_inputs$aspect1a_1, user_inputs$aspect1a_2, user_inputs$aspect1a_3, user_inputs$aspect1a_4,
          user_inputs$defens1a_1, user_inputs$defens1a_2, user_inputs$defens1a_3, user_inputs$defens1a_4
        ),
        Description = c(
          ifelse(user_inputs$aspect1a_text_1 != "", user_inputs$aspect1a_text_1, "NULL"),
          ifelse(user_inputs$aspect1a_text_2 != "", user_inputs$aspect1a_text_2, "NULL"),
          ifelse(user_inputs$aspect1a_text_3 != "", user_inputs$aspect1a_text_3, "NULL"),
          ifelse(user_inputs$aspect1a_text_4 != "", user_inputs$aspect1a_text_4, "NULL"),
          ifelse(user_inputs$defens1a_text_1 != "", user_inputs$defens1a_text_1, "NULL"),
          ifelse(user_inputs$defens1a_text_2 != "", user_inputs$defens1a_text_2, "NULL"),
          ifelse(user_inputs$defens1a_text_3 != "", user_inputs$defens1a_text_3, "NULL"),
          ifelse(user_inputs$defens1a_text_4 != "", user_inputs$defens1a_text_4, "NULL")
        ),
        stringsAsFactors = FALSE
      )
      write.csv(aspects_defensibility_data, file.path(tempDir, "Aspects_Defensibility.csv"), row.names = FALSE)
      
      ###### ✅ 2. Save Steps CSV ######
      steps_data <- list()
      if (length(reactiveValuesToList(step_options)) > 0) {
        for (step_id in names(reactiveValuesToList(step_options))) {
          step_name <- input[[paste0("step_name_", step_id)]]
          step_defensibility <- input[[paste0("defensibility_step_", step_id)]]
          step_justification <- input[[paste0("justification_step_", step_id)]]
          
          steps_data <- append(steps_data, list(
            list(
              "Name" = step_name,
              "Defensibility" = step_defensibility,
              "Justification" = ifelse(step_justification != "", step_justification, "NULL"),
              "Type" = "Node"
            )
          ))
          
          # Process step options
          step_options_list <- reactiveValuesToList(step_options)[[step_id]]
          if (length(step_options_list) > 0) {
            for (option_id in names(step_options_list)) {
              option_name <- gsub("^[^a-zA-Z0-9]+|[^a-zA-Z0-9]+$", "", input[[paste0("option_", step_id, "_", option_id)]])
              option_defensibility <- input[[paste0("defensibility_option_", step_id, "_", option_id)]]
              option_justification <- input[[paste0("justification_option_", step_id, "_", option_id)]]
              
              steps_data <- append(steps_data, list(
                list(
                  "Name" = option_name,
                  "Defensibility" = option_defensibility,
                  "Justification" = ifelse(option_justification != "", option_justification, "NULL"),
                  "Type" = "Option"
                )
              ))
            }
          }
        }
      }
      steps_df <- do.call(rbind, lapply(steps_data, as.data.frame))
      write.csv(steps_df, file.path(tempDir, "Steps.csv"), row.names = FALSE)
      
      ###### ✅ 3. Save Pipelines CSV ######
      pipelines_data <- list()
      for (pipeline_name in names(saved_pipelines$data)) {
        if (grepl("^graph_", pipeline_name)) next  # Skip graph-related data
        
        pipeline_data <- saved_pipelines$data[[pipeline_name]]
        if (!is.null(pipeline_data) && length(pipeline_data) > 0) {
          pipelines_data <- append(pipelines_data, list(
            list(
              "Name" = pipeline_name,
              "Pipeline" = paste(gsub("➝", "", pipeline_data), collapse = " -> ")
            )
          ))
        }
      }
      pipelines_df <- do.call(rbind, lapply(pipelines_data, as.data.frame))
      write.csv(pipelines_df, file.path(tempDir, "Pipelines.csv"), row.names = FALSE)
      
      ###### ✅ 4. Save Visual Network Data CSV ######
      network_nodes <- list()
      network_edges <- list()
      
      for (pipeline_name in names(saved_pipelines$data)) {
        graph_key <- paste0("graph_", pipeline_name)
        if (!graph_key %in% names(saved_pipelines$data)) next  # Skip if no network data
        
        graph_data <- saved_pipelines$data[[graph_key]]
        if (is.null(graph_data) || is.null(graph_data$nodes) || is.null(graph_data$edges)) next
        
        # ✅ Save nodes (Ensure consistent column structure)
        for (node in seq_len(nrow(graph_data$nodes))) {
          network_nodes <- append(network_nodes, list(
            list(
              "Name" = pipeline_name,
              "Type" = "Node",
              "ID" = graph_data$nodes$id[node],
              "Label" = graph_data$nodes$label[node],
              "Color" = graph_data$nodes$color[node],
              "Shape" = graph_data$nodes$shape[node],
              "From" = NA,  # Ensure the column exists (for rbind compatibility)
              "To" = NA
            )
          ))
        }
        
        # ✅ Save edges (Ensure consistent column structure)
        for (edge in seq_len(nrow(graph_data$edges))) {
          network_edges <- append(network_edges, list(
            list(
              "Name" = pipeline_name,
              "Type" = "Edge",
              "ID" = NA,  # Ensure the column exists (for rbind compatibility)
              "Label" = NA,
              "Color" = NA,
              "Shape" = NA,
              "From" = graph_data$edges$from[edge],
              "To" = graph_data$edges$to[edge]
            )
          ))
        }
      }
      
      # ✅ Convert lists to data frames
      nodes_df <- do.call(rbind, lapply(network_nodes, as.data.frame))
      edges_df <- do.call(rbind, lapply(network_edges, as.data.frame))
      
      # ✅ Ensure nodes and edges have the same columns before rbind
      all_columns <- c("Pipeline Name", "Type", "ID", "Label", "Color", "Shape", "From", "To")
      nodes_df[setdiff(all_columns, colnames(nodes_df))] <- NA
      edges_df[setdiff(all_columns, colnames(edges_df))] <- NA
      
      # ✅ Combine and write to CSV
      network_df <- rbind(nodes_df, edges_df)
      write.csv(network_df, file.path(tempDir, "Visual_Network.csv"), row.names = FALSE)
      
      ###### ✅ 5. ZIP only the Four CSVs ######
      csv_files <- list.files(tempDir, pattern = "*.csv", full.names = TRUE)
      zip::zipr(zipfile = file, files = csv_files)
      
      # Upload to the cloud
      upload_to_cloud(file, correct_filename)
      
      removeModal()
      # ✅ Cleanup temp directory
      unlink(tempDir, recursive = TRUE)
    }
  )
  
############################################################## 

################################################################################

################################ Multiverse 2.0 ################################

############# Step 2a: Criterion for Equivalance #############
  # ✅ Update dropdown choices when pipelines are available
  observe({
    if (length(saved_pipelines$data) > 0) {
      valid_pipelines <- names(saved_pipelines$data)
      valid_pipelines <- valid_pipelines[!grepl("^graph_", valid_pipelines)]  # Remove graph-related entries
      
      updateSelectInput(session, "selected_equivalence_pipeline", 
                        choices = valid_pipelines)
    }
  })
  
  # ✅ Render table properly instead of just listing steps
  output$equivalence_pipeline_table <- DT::renderDataTable({
    req(input$selected_equivalence_pipeline)  # Correct input ID
    
    selected_pipeline <- input$selected_equivalence_pipeline  # Correct variable
    if (is.null(saved_pipelines$data[[selected_pipeline]])) return(NULL)
    
    pipeline_data <- data.frame(
      Ind = seq_along(saved_pipelines$data[[selected_pipeline]]),
      "Pipeline" = saved_pipelines$data[[selected_pipeline]]
    )
    
    DT::datatable(
      pipeline_data,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 50,
        ordering = FALSE,
        autoWidth = FALSE
      ),
      class = "display nowrap"
    )
  })
  
  # ✅ Render Visual Network of Selected Pipeline
  output$equivalence_pipeline_network <- renderVisNetwork({
    req(input$selected_equivalence_pipeline)
    
    selected_pipeline <- isolate(input$selected_equivalence_pipeline)
    graph_key <- paste0("graph_", selected_pipeline)
    
    if (!graph_key %in% names(saved_pipelines$data)) {
      return(NULL)  # Handle missing graph data gracefully
    }
    
    graph_data <- saved_pipelines$data[[graph_key]]
    if (is.null(graph_data) || is.null(graph_data$nodes) || is.null(graph_data$edges)) {
      return(NULL)
    }
    
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visNodes(font = list(size = 20), borderWidth = 2) %>%
      visEdges(arrows = "to", color = list(color = "#000000")) %>%
      visLayout(hierarchical = TRUE)
  })
  
  # ✅ Dynamic Inputs for Quality Metric & Type Selection
  output$equivalence_inputs_ui <- renderUI({
    req(input$selected_equivalence_pipeline)
    
    selected_pipeline <- input$selected_equivalence_pipeline
    
    tagList(
      br(),  # Small spacing adjustment
      fluidRow(
        column(6, h5("Quality Metric Value:", style = "text-align: center; margin-bottom: 5px;")),
        column(6, h5("Equivalence Type:", style = "text-align: left; margin-bottom: 5px;"))
      ),
      fluidRow(
        column(
          6,
          textInput(
            paste0("quality_metric_value_", selected_pipeline), 
            label = NULL, width = "100%"
          )
        ),
        column(
          6,
          div(
            style = "display: flex; align-items: center;",
            radioButtons(
              paste0("equivalence_type_", selected_pipeline), 
              label = NULL,
              choices = c("Type E", "Type N"),
              inline = TRUE,
              selected = FALSE
            )
          )
        )
      )
    )
  })
  
##############################################################  

############ Step 2b: Your Principled Multiverse #############
  # ✅ Reactive value to store Type E pipelines
  type_e_pipelines <- reactiveValues(data = c())
  
  # ✅ Observe when a user classifies a pipeline as "Type E" in Step 2a
  observe({
    if (length(saved_pipelines$data) > 0) {
      valid_pipelines <- names(saved_pipelines$data)
      valid_pipelines <- valid_pipelines[!grepl("^graph_", valid_pipelines)]  # Remove graph-related entries
      
      # Check for Type E pipelines safely
      selected_type_e <- sapply(valid_pipelines, function(pipeline) {
        if (!is.null(input[[paste0("equivalence_type_", pipeline)]])) {
          input[[paste0("equivalence_type_", pipeline)]] == "Type E"
        } else {
          FALSE
        }
      })
      
      # Convert list to logical vector to avoid invalid subscript error
      selected_type_e <- unlist(selected_type_e, use.names = FALSE)
      
      # Store only pipelines classified as Type E
      type_e_pipelines$data <- valid_pipelines[selected_type_e]
      
      updateSelectInput(session, "selected_principled_pipeline", 
                        choices = type_e_pipelines$data)
    }
  })
  
  # ✅ Render table for selected Type E pipeline
  output$principled_pipeline_table <- DT::renderDataTable({
    req(input$selected_principled_pipeline)
    
    selected_pipeline <- input$selected_principled_pipeline
    if (is.null(saved_pipelines$data[[selected_pipeline]])) return(NULL)
    
    pipeline_data <- data.frame(
      Ind = seq_along(saved_pipelines$data[[selected_pipeline]]),
      "Pipeline" = saved_pipelines$data[[selected_pipeline]]
    )
    
    DT::datatable(
      pipeline_data,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 50,
        ordering = FALSE,
        autoWidth = FALSE
      ),
      class = "display nowrap"
    )
  })
  
  # ✅ Render Visual Network of Selected Type E Pipeline
  output$principled_pipeline_network <- renderVisNetwork({
    req(input$selected_principled_pipeline)
    
    selected_pipeline <- isolate(input$selected_principled_pipeline)
    graph_key <- paste0("graph_", selected_pipeline)
    
    if (!graph_key %in% names(saved_pipelines$data)) {
      return(NULL)  # Handle missing graph data gracefully
    }
    
    graph_data <- saved_pipelines$data[[graph_key]]
    if (is.null(graph_data) || is.null(graph_data$nodes) || is.null(graph_data$edges)) {
      return(NULL)
    }
    
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visNodes(font = list(size = 20), borderWidth = 2) %>%
      visEdges(arrows = "to", color = list(color = "#000000")) %>%
      visLayout(hierarchical = TRUE)
  })
  
  # Download Multiverse 2.0
  output$download_principled_multiverse <- downloadHandler(
    filename = function() {
      nickname <- input$user_nickname
      if (is.null(nickname) || nickname == "") {
        paste0("Principled_Multiverse_", Sys.Date(), ".pdf")
      } else {
        paste0(nickname, "_Principled_Multiverse_", Sys.Date(), ".pdf")
      }
    },
    content = function(file) {
      
      showModal(modalDialog(
        title = "Generating download...",
        "This may take a while depending on the number of pipelines.",
        easyClose = FALSE,
        footer = NULL
      ))
      
      nickname <- input$user_nickname
      correct_filename <- if (is.null(nickname) || nickname == "") {
        paste0("Principled_Multiverse_", Sys.Date(), ".pdf")
      } else {
        paste0(nickname, "_Principled_Multiverse_", Sys.Date(), ".pdf")
      }
      
      tempDir <- tempdir()
      tempReport <- file.path(tempDir, "Principled_Multiverse_Report.Rmd")
      pdf_file <- file.path(tempDir, "Principled_Multiverse_Report.pdf")
      
      print("Generating Principled Multiverse PDF...")
      
      if (length(type_e_pipelines$data) == 0) {
        showNotification("No principled pipelines found!", type = "error", duration = 5)
        print("❌ No Type E pipelines found.")
        return()
      }
      
      # ✅ Pre-process and flatten pipeline text for PDF insertion
      pipeline_text <- if (length(type_e_pipelines$data) > 0) {
        text_chunks <- c()
        counter <- 1
        for (pipeline_name in type_e_pipelines$data) {
          pipeline_data <- saved_pipelines$data[[pipeline_name]]
          quality_metric <- input[[paste0("quality_metric_value_", pipeline_name)]]
          if (is.null(quality_metric) || quality_metric == "") quality_metric <- "[Not provided]"
          line <- paste0("\n## Pipeline ", counter, "  (Quality Metric: ", quality_metric, ")\n")
          counter <- counter + 1
          if (!is.null(pipeline_data) && length(pipeline_data) > 0) {
            steps <- c()
            for (step in pipeline_data) {
              step <- trimws(step)
              step <- gsub("➝", "->", step, fixed = TRUE)  # Replace arrows safely
              if (grepl("->", step)) {
                step <- gsub(".*->\\s*", "", step)
                steps <- c(steps, step)
              } else {
                step <- paste0("**", step, "**")
                steps <- c(steps, step)
              }
            }
            line <- paste0(line, paste(steps, collapse = " -> "), "\n")
          } else {
            line <- paste0(line, "\n**[No data available]**\n")
          }
          text_chunks <- c(text_chunks, line)
        }
        paste(text_chunks, collapse = "\n")
      } else {
        "No pipelines recorded."
      }
      
      # ✅ Write the Rmd file, using `params` for pipeline content
      writeLines(c(
        "---",
        "title: \"Multiverse 2.0\"",
        "author: \"Generated by Systematic Multiverse Analysis Registration Tool\"",
        "date: \"`r format(Sys.time(), '%d %B %Y, %H:%M')`\"",
        "output:",
        "  pdf_document:",
        "    number_sections: false",
        "params:",
        "  processed_multiverse: ''",
        "---",
        "",
        "# Criterion for Equivalence",
        "`r if (!is.null(input$EquvalanceCriterion) && input$EquvalanceCriterion != '') { input$EquvalanceCriterion } else { 'No criterion provided.' }`",
        "",
        "# Threshold for Equivalence",
        "`r if (!is.null(input$ThresholdEquvalance) && input$ThresholdEquvalance != '') { input$ThresholdEquvalance } else { 'No threshold provided.' }`",
        "",
        "# Subsample for Equivalence Testing",
        "`r if (!is.null(input$SubsampleEquvalance) && input$SubsampleEquvalance != '') { input$SubsampleEquvalance } else { 'No subsample provided.' }`",
        "",
        "# Principled Multiverse",
        "`r params$processed_multiverse`",
        "---",
        "Generated by Systematic Multiverse Analysis Registration Tool on `r format(Sys.time(), '%d %B %Y, %H:%M')`"
      ), tempReport)
      
      # ✅ Render the PDF with pre-computed pipeline_text
      tryCatch({
        rmarkdown::render(
          input = tempReport,
          output_file = pdf_file,
          output_format = "pdf_document",
          params = list(processed_multiverse = pipeline_text),
          envir = new.env()
        )
        file.copy(pdf_file, file)
        showNotification("PDF generated successfully!", type = "message", duration = 5)
        print("✅ PDF rendered and copied.")
        upload_to_cloud(file, correct_filename)
      }, error = function(e) {
        print(paste("❌ ERROR rendering PDF:", e$message))
        showNotification("Error generating PDF!", type = "error", duration = 5)
      })
      removeModal()
    }
  )
##############################################################  

}
