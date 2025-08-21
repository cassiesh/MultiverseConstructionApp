library(shiny)
library(shinyWidgets)
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
library(gridExtra)

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

source("Source_function.R", local = TRUE)

server <- function(input, output, session) {

  # ✅ Prevent session from timing out due to inactivity
  auto_keep_alive <- reactiveTimer(60000)  # Fires every minute
  
  observe({
    auto_keep_alive()
    # Invisible ping to keep session alive
  })
  session$allowReconnect(TRUE)
################# Variables Required for Overall Functionality #################  
  # Write a CSV even when empty (ensures header row)
  write_csv_safely <- function(df, path) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    if (!nrow(df)) {
      # write just headers
      utils::write.csv(df[0, , drop = FALSE], path,
                       row.names = FALSE, na = "", fileEncoding = "UTF-8")
      return(invisible(TRUE))
    }
    # normalize all character columns to UTF-8
    for (nm in names(df)) if (is.character(df[[nm]])) df[[nm]] <- enc2utf8(df[[nm]])
    tryCatch({
      utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      TRUE
    }, error = function(e) {
      message("write_csv_safely failed for ", path, ": ", conditionMessage(e))
      FALSE
    })
  }
  
  # --- Arrow normalizer so "➝" survives roundtrips
  fix_arrow <- function(x) {
    if (is.null(x)) return(x)
    x <- enc2utf8(x)
    x <- gsub("(<U\\+279D>|\\\\u279d)", "➝", x, perl = TRUE, ignore.case = TRUE)
    x
  }
  
  # --- Step 2a state for meta + per-pipeline metric/type
  step2a_state <- reactiveValues(
    meta    = NULL,     # list(equivalence_criterion, threshold, subsample)
    metrics = list()    # named by pipeline_id -> list(quality_metric_value, equivalence_type)
  )
  # prevents observers from overwriting restored values during import
  step2a_restoring <- reactiveVal(FALSE)
  
  # Minimal manifest writer (local to server)
  write_manifest_json <- function(dir, exported_step, csvs) {
    man <- list(
      app_version  = "1.0.0",
      exported_step = exported_step,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      csvs = unname(csvs)
    )
    jsonlite::write_json(man, file.path(dir, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  }
  
  # Build the four Step 1a DataFrames from current app state
  build_step1a_dfs <- function() {
    # Scope
    scope_cb <- (saved_step1a$scope$checkboxes) %||% rep(FALSE, 4)
    scope_tx <- (saved_step1a$scope$texts)      %||% rep("",    4)
    step1a_scope <- data.frame(
      category    = c("Measurement","Preprocessing","Model Specification","Estimation Methods"),
      checked     = as.logical(scope_cb),
      description = as.character(scope_tx),
      stringsAsFactors = FALSE
    )
    
    # Methods
    methods_cb <- (saved_step1a$methods$checkboxes) %||% rep(FALSE, 4)
    methods_tx <- (saved_step1a$methods$texts)      %||% rep("",    4)
    step1a_methods <- data.frame(
      method      = c("Literature Review","Expertise","Crowdsourcing","Other"),
      checked     = as.logical(methods_cb),
      description = as.character(methods_tx),
      stringsAsFactors = FALSE
    )
    
    # Nodes & options from your dynamic UI
    ids <- active_steps()
    if (length(ids) == 0) {
      step1a_nodes   <- data.frame(node_index = integer(0), node_title = character(0))
      step1a_options <- data.frame(node_index = integer(0), option_index = integer(0), option_label = character(0))
    } else {
      step1a_nodes <- do.call(rbind, lapply(seq_along(ids), function(i){
        id <- ids[i]
        data.frame(node_index = i,
                   node_title = trimws(input[[paste0("step_name_", id)]] %||% ""),
                   stringsAsFactors = FALSE)
      }))
      # options (keep stable indices per node)
      opt_rows <- list()
      for (i in seq_along(ids)) {
        id <- ids[i]
        oks <- names(step_options[[as.character(id)]])
        if (!is.null(oks) && length(oks)) {
          for (j in seq_along(oks)) {
            ok <- oks[j]
            lbl <- trimws(input[[paste0("option_", id, "_", ok)]] %||% "")
            opt_rows[[length(opt_rows)+1]] <- data.frame(
              node_index = i, option_index = j, option_label = lbl, stringsAsFactors = FALSE
            )
          }
        }
      }
      step1a_options <- if (length(opt_rows)) do.call(rbind, opt_rows) else
        data.frame(node_index = integer(0), option_index = integer(0), option_label = character(0))
    }
    
    # Analysis plan (use saved copy if present; otherwise read live inputs)
    step1a_analysis <- if (!is.null(saved_step1a$analysis)) {
      as.data.frame(saved_step1a$analysis, stringsAsFactors = FALSE)
    } else {
      desc <- trimws(input$step1a_descriptive_analysis %||% "")
      infer <- trimws(input$step1a_inferential_analysis %||% "")
      mcc   <- trimws(input$step1a_multiple_correction %||% "")
      data.frame(
        field = c("Descriptive analysis", "Inferential analysis", "Multiple comparisons correction"),
        value = c(desc, infer, mcc),
        stringsAsFactors = FALSE
      )
    }
    
    list(
      scope   = step1a_scope,
      methods = step1a_methods,
      nodes   = step1a_nodes,
      options = step1a_options,
      analysis= step1a_analysis
    )
  }
  
  # Build the four Step 1b Data Frames from current app state
  build_step1b_dfs <- function() {
    ids <- active_steps()
    # Nodes table
    if (length(ids) == 0) {
      step1b_nodes <- data.frame(
        node_index = integer(0),
        node_title = character(0),
        defensibility = character(0),
        justification = character(0),
        stringsAsFactors = FALSE
      )
      step1b_options <- data.frame(
        node_index = integer(0),
        option_index = integer(0),
        option_label = character(0),
        defensibility = character(0),
        justification = character(0),
        stringsAsFactors = FALSE
      )
      return(list(nodes = step1b_nodes, options = step1b_options))
    }
    
    # nodes
    step1b_nodes <- do.call(rbind, lapply(seq_along(ids), function(i) {
      sid <- ids[i]
      data.frame(
        node_index   = i,
        node_title   = input[[paste0("step_name_", sid)]] %||% "",
        defensibility= input[[paste0("defensibility_step_", sid)]] %||% "",
        justification= input[[paste0("justification_step_", sid)]] %||% "",
        stringsAsFactors = FALSE
      )
    }))
    
    # options (match 1a option order -> option_index = j)
    opt_rows <- list()
    for (i in seq_along(ids)) {
      sid <- ids[i]
      oks <- names(step_options[[as.character(sid)]])
      if (!is.null(oks) && length(oks)) {
        for (j in seq_along(oks)) {
          ok <- oks[j]
          opt_rows[[length(opt_rows)+1]] <- data.frame(
            node_index    = i,
            option_index  = j,
            option_label  = input[[paste0("option_", sid, "_", ok)]] %||% "",
            defensibility = input[[paste0("defensibility_option_", sid, "_", ok)]] %||% "",
            justification = input[[paste0("justification_option_", sid, "_", ok)]] %||% "",
            stringsAsFactors = FALSE
          )
        }
      }
    }
    step1b_options <- if (length(opt_rows)) do.call(rbind, opt_rows) else
      data.frame(
        node_index = integer(0),
        option_index = integer(0),
        option_label = character(0),
        defensibility = character(0),
        justification = character(0),
        stringsAsFactors = FALSE
      )
    
    list(nodes = step1b_nodes, options = step1b_options)
  }
  
  # Build the four Step 1c Data Frames from current app state
  build_step1c_dfs <- function() {
    # 1) Pipelines (order-preserving)
    p_ids <- names(saved_pipelines$data)
    p_ids <- p_ids[!grepl("^graph_", p_ids)]

    pipelines_df <- do.call(
      rbind,
      lapply(p_ids, function(pid) {
        labs <- saved_pipelines$data[[pid]] %||% character(0)
        # KEEP option arrows; only normalize ASCII arrow to pretty arrow
        normalize_arrow <- function(x) {
          x <- enc2utf8(x)
          # turn leading "-> " into "➝ " but DO NOT strip existing "➝ "
          sub("^\\s*->\\s*", "➝ ", x, perl = TRUE)
        }
        labs <- vapply(labs, normalize_arrow, "", USE.NAMES = FALSE)
        if (!length(labs)) {
          data.frame(pipeline_id = pid, order_index = integer(0), label = character(0))
        } else {
          data.frame(
            pipeline_id = pid,
            order_index = seq_along(labs),
            label       = labs,
            stringsAsFactors = FALSE
          )
        }
      })
    )
    
    if (is.null(pipelines_df)) {
      pipelines_df <- data.frame(pipeline_id=character(0), order_index=integer(0), label=character(0))
    }
    
    # 2) Graph nodes
    nodes_list <- list()
    for (pid in p_ids) {
      gkey <- paste0("graph_", pid)
      g <- saved_pipelines$data[[gkey]]
      if (is.null(g) || is.null(g$nodes) || !nrow(g$nodes)) next
      nd <- g$nodes
      nd$pipeline_id <- pid
      # ensure expected cols exist
      for (nm in c("id","label","color","shape","level")) if (!(nm %in% names(nd))) nd[[nm]] <- NA
      nodes_list[[length(nodes_list)+1]] <- nd[, c("pipeline_id","id","label","color","shape","level")]
    }
    nodes_df <- if (length(nodes_list)) do.call(rbind, nodes_list) else
      data.frame(pipeline_id=character(0), id=character(0), label=character(0),
                 color=character(0), shape=character(0), level=numeric(0),
                 stringsAsFactors = FALSE)
    
    # 3) Graph edges
    edges_list <- list()
    for (pid in p_ids) {
      gkey <- paste0("graph_", pid)
      g <- saved_pipelines$data[[gkey]]
      if (is.null(g) || is.null(g$edges) || !nrow(g$edges)) next
      ed <- g$edges
      ed$pipeline_id <- pid
      for (nm in c("from","to")) if (!(nm %in% names(ed))) ed[[nm]] <- NA
      edges_list[[length(edges_list)+1]] <- ed[, c("pipeline_id","from","to")]
    }
    edges_df <- if (length(edges_list)) do.call(rbind, edges_list) else
      data.frame(pipeline_id=character(0), from=character(0), to=character(0),
                 stringsAsFactors = FALSE)
    
    list(pipelines = pipelines_df, graph_nodes = nodes_df, graph_edges = edges_df)
  }
  
  # Restore saved_pipelines$data from the three CSV data.frames
  restore_step1c_from_dfs <- function(pipelines_df, nodes_df, edges_df) {
    req(is.data.frame(pipelines_df), is.data.frame(nodes_df), is.data.frame(edges_df))
    
    # --- (A) normalize character columns to UTF-8 so we don't see <U+279D> ---
    norm_utf8_df <- function(d) {
      if (!nrow(d)) return(d)
      for (nm in names(d)) if (is.character(d[[nm]])) d[[nm]] <- enc2utf8(d[[nm]])
      d
    }
    pipelines_df <- norm_utf8_df(pipelines_df)
    if (nrow(pipelines_df) && "label" %in% names(pipelines_df)) {
      pipelines_df$label <- fix_arrow(pipelines_df$label)  # turn <U+279D> / \u279d into "➝"
    }
    nodes_df     <- norm_utf8_df(nodes_df)
    edges_df     <- norm_utf8_df(edges_df)
    
    saved <- list()
    
    # 1) Pipelines (order by order_index)
    if (nrow(pipelines_df)) {
      split_p <- split(pipelines_df, pipelines_df$pipeline_id)
      
      # (B) re-add pretty arrow ONLY for display (what we persist in CSV stays plain)
      re_decorate_arrow <- function(x) {
        x <- enc2utf8(x)
        # if a label already begins with ASCII '-> ' or pretty '➝ ', normalize it to '➝ '
        sub("^\\s*(->|➝)\\s*", "➝ ", x, perl = TRUE)
      }
      
      for (pid in names(split_p)) {
        df <- split_p[[pid]][order(split_p[[pid]]$order_index), , drop = FALSE]
        labs <- as.character(df$label %||% character(0))
        
        # If you stored plain labels in CSV (recommended), this keeps steps untouched
        # and only normalizes any arrowed entries back to the pretty arrow.
        labs <- vapply(labs, re_decorate_arrow, "", USE.NAMES = FALSE)
        
        # Save the restored pipeline sequence for the app to use
        saved[[pid]] <- labs
      }
    }
    
    # 2) Graphs (unchanged logic, with UTF-8 already normalized above)
    if (nrow(nodes_df) || nrow(edges_df)) {
      pids <- unique(c(as.character(nodes_df$pipeline_id), as.character(edges_df$pipeline_id)))
      pids <- pids[nzchar(pids)]
      for (pid in pids) {
        nd <- nodes_df[nodes_df$pipeline_id == pid, , drop = FALSE]
        ed <- edges_df[edges_df$pipeline_id == pid, , drop = FALSE]
        nd <- if (nrow(nd)) nd[, c("id","label","color","shape","level"), drop = FALSE] else
          data.frame(id=character(0), label=character(0), color=character(0),
                     shape=character(0), level=numeric(0), stringsAsFactors = FALSE)
        ed <- if (nrow(ed)) ed[, c("from","to"), drop = FALSE] else
          data.frame(from=character(0), to=character(0), stringsAsFactors = FALSE)
        
        saved[[paste0("graph_", pid)]] <- list(nodes = nd, edges = ed)
      }
    }
    
    # Commit all restored pipelines + graphs
    saved_pipelines$data <- saved
    
    # Refresh Step 1d dropdown, if mounted
    if (!is.null(session$input$selected_pipeline)) {
      choices <- names(saved_pipelines$data)
      choices <- choices[!grepl("^graph_", choices)]
      updateSelectInput(session, "selected_pipeline", choices = choices, selected = NULL)
    }
  }
  
  # ---- Step 2a-only ZIP loader (independent of Steps 1a/1b/1c) ----
  observeEvent(input$load_step2a_zip, {
    req(input$load_step2a_zip)
    fpath <- input$load_step2a_zip$datapath
    fname <- tolower(input$load_step2a_zip$name)
    if (!grepl("\\.zip$", fname)) {
      showNotification("Please upload a .zip exported from Step 2a.", type = "error")
      return()
    }
    
    tmpdir <- file.path(tempdir(), paste0("imp2a_", as.integer(runif(1,1e6,1e7))))
    dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
    
    # Unzip
    ok <- tryCatch({
      utils::unzip(fpath, exdir = tmpdir)
      TRUE
    }, error = function(e) {
      showNotification("Could not read ZIP.", type = "error")
      FALSE
    })
    if (!ok) return()
    
    # Helper: tolerant CSV reader w/ UTF-8 normalize
    read_safely_utf8 <- function(p) {
      out <- tryCatch(utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
                      error = function(e) NULL)
      if (is.null(out)) return(NULL)
      for (nm in names(out)) if (is.character(out[[nm]])) out[[nm]] <- enc2utf8(out[[nm]])
      out
    }
    
    # Expected Step 2a files (pipelines + graphs are needed for table/network;
    # meta + metrics prefill the text areas and per-pipeline inputs)
    p_meta  <- file.path(tmpdir, "step2a_meta.csv")
    p_mx    <- file.path(tmpdir, "step2a_pipeline_metrics.csv")
    p_seq   <- file.path(tmpdir, "step2a_pipelines.csv")
    p_nodes <- file.path(tmpdir, "step2a_graph_nodes.csv")
    p_edges <- file.path(tmpdir, "step2a_graph_edges.csv")
    
    # Read available CSVs
    meta_df    <- if (file.exists(p_meta))  read_safely_utf8(p_meta)  else NULL
    metrics_df <- if (file.exists(p_mx))    read_safely_utf8(p_mx)    else NULL
    pipes_df   <- if (file.exists(p_seq))   read_safely_utf8(p_seq)   else NULL
    nodes_df   <- if (file.exists(p_nodes)) read_safely_utf8(p_nodes) else NULL
    edges_df   <- if (file.exists(p_edges)) read_safely_utf8(p_edges) else NULL
    
    # Minimal presence checks: allow meta-only loads, but warn if we can't update view
    if (is.null(pipes_df) && (is.null(nodes_df) || is.null(edges_df))) {
      # nothing to render in the table/network; still restore meta/metrics if present
      if (!is.null(meta_df) || !is.null(metrics_df)) {
        restore_step2a_from_dfs(meta_df, metrics_df, NULL, NULL, NULL)
        showNotification("Step 2a meta/metrics loaded. No pipelines/graph data found in ZIP.", type = "warning", duration = 6)
        updateNavbarPage(session, "main_nav", selected = "mv2")
        shinyjs::delay(0, updateTabsetPanel(session, "mv2_tabs", selected = "2a"))
      } else {
        showNotification("ZIP does not contain Step 2a CSVs.", type = "error")
      }
      return()
    }
    
    # Optional schema checks (tolerant but informative)
    if (!is.null(pipes_df)) {
      req_cols_p <- c("pipeline_id","order_index","label")
      if (!all(req_cols_p %in% names(pipes_df))) {
        showNotification("step2a_pipelines.csv schema is invalid; skipping pipeline restore.", type = "warning")
        pipes_df <- NULL
      }
    }
    if (!is.null(nodes_df)) {
      req_cols_n <- c("pipeline_id","id","label","color","shape","level")
      if (!all(req_cols_n %in% names(nodes_df))) {
        showNotification("step2a_graph_nodes.csv schema is invalid; skipping network nodes.", type = "warning")
        nodes_df <- NULL
      }
    }
    if (!is.null(edges_df)) {
      req_cols_e <- c("pipeline_id","from","to")
      if (!all(req_cols_e %in% names(edges_df))) {
        showNotification("step2a_graph_edges.csv schema is invalid; skipping network edges.", type = "warning")
        edges_df <- NULL
      }
    }
    
    # Normalize arrows in labels (so no <U+279D> shows up)
    if (!is.null(pipes_df) && "label" %in% names(pipes_df))  pipes_df$label  <- fix_arrow(pipes_df$label)
    if (!is.null(nodes_df) && "label" %in% names(nodes_df))  nodes_df$label  <- fix_arrow(nodes_df$label)
    
    # Restore
    restore_step2a_from_dfs(meta_df, metrics_df, pipes_df, nodes_df, edges_df)
    
    # Jump user to Step 2a
    updateNavbarPage(session, "main_nav", selected = "mv2")
    shinyjs::delay(0, updateTabsetPanel(session, "mv2_tabs", selected = "2a"))
    
    showNotification("Step 2a progress loaded.", type = "message")
  })
  
  # Build Step 2a CSVs from current UI state
  build_step2a_dfs <- function() {
    # Derive mode + details safely
    mode <- as.character(input$equivalence_assessment_mode %||% "Subsampling")
    sim_details <- if (mode %in% c("Simulation","Both"))
      as.character(input$simulation_details_link %||% "")
    else
      ""
    
    # Meta (single row)
    step2a_meta <- data.frame(
      equivalence_criterion = as.character(input$EquvalanceCriterion %||% ""),
      threshold             = as.character(input$ThresholdEquvalance %||% ""),
      subsample             = as.character(input$SubsampleEquvalance %||% ""),
      assessment_mode       = mode,          # <-- NEW
      simulation_details    = sim_details,   # <-- NEW
      stringsAsFactors = FALSE
    )
    
    # Per-pipeline metrics (one row per pipeline)
    pl_ids <- names(saved_pipelines$data)
    pl_ids <- pl_ids[!grepl("^graph_", pl_ids)]
    
    if (length(pl_ids) == 0) {
      step2a_metrics <- data.frame(
        pipeline_id = character(0),
        quality_metric_value = character(0),
        equivalence_type     = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      rows <- lapply(pl_ids, function(pid) {
        data.frame(
          pipeline_id = pid,
          quality_metric_value = as.character(input[[paste0("quality_metric_value_", pid)]] %||% ""),
          equivalence_type     = as.character(input[[paste0("equivalence_type_",     pid)]] %||% ""),
          stringsAsFactors = FALSE
        )
      })
      step2a_metrics <- do.call(rbind, rows)
    }
    
    list(meta = step2a_meta, metrics = step2a_metrics)
  }
  
  # Restore Step 2a UI from CSVs
  restore_step2a_from_dfs <- function(meta_df=NULL, metrics_df=NULL,
                                      pipelines_df=NULL, nodes_df=NULL, edges_df=NULL) {
    step2a_restoring(TRUE)  # <-- block sync observer while we restore
    on.exit(step2a_restoring(FALSE), add = TRUE)
    
    # --- 2a meta -> prefill textareas
    if (!is.null(meta_df) && nrow(meta_df)) {
      step2a_state$meta <- list(
        equivalence_criterion = as.character(meta_df$equivalence_criterion[1] %||% ""),
        threshold             = as.character(meta_df$threshold[1] %||% ""),
        subsample             = as.character(meta_df$subsample[1] %||% ""),
        assessment_mode       = as.character((meta_df$assessment_mode %||% "Subsampling")[1]),
        simulation_details    = as.character((meta_df$simulation_details %||% "")[1])
      )
      updateTextAreaInput(session, "EquvalanceCriterion", value = step2a_state$meta$equivalence_criterion)
      updateTextAreaInput(session, "ThresholdEquvalance", value = step2a_state$meta$threshold)
      updateTextAreaInput(session, "SubsampleEquvalance", value = step2a_state$meta$subsample)
      
      mode <- step2a_state$meta$assessment_mode %||% "Subsampling"
      if (!is.null(mode) && nzchar(mode)) {
        updateRadioButtons(session, "equivalence_assessment_mode", selected = mode)
      }
      
      # ensure the inline textInput exists (rendered only for Simulation/Both) before updating
      if (mode %in% c("Simulation","Both")) {
        # wait a tick so renderUI has created the input
        shinyjs::delay(0, {
          updateTextInput(session, "simulation_details_link",
                          value = step2a_state$meta$simulation_details %||% "")
        })
      }
      
    }
    
    # --- Pipelines + graphs
    if (!is.null(pipelines_df) && nrow(pipelines_df)) {
      pipelines_df$label <- fix_arrow(pipelines_df$label)
      saved <- list()
      split_p <- split(pipelines_df, pipelines_df$pipeline_id)
      for (pid in names(split_p)) {
        df <- split_p[[pid]][order(split_p[[pid]]$order_index), , drop = FALSE]
        saved[[pid]] <- as.character(df$label)
      }
      if (!is.null(nodes_df) || !is.null(edges_df)) {
        pids <- unique(c(as.character(nodes_df$pipeline_id %||% character()),
                         as.character(edges_df$pipeline_id %||% character())))
        for (pid in pids) {
          nd <- nodes_df[nodes_df$pipeline_id == pid, , drop = FALSE]
          ed <- edges_df[edges_df$pipeline_id == pid, , drop = FALSE]
          if (nrow(nd)) nd$label <- fix_arrow(nd$label)
          nd <- if (nrow(nd)) nd[, c("id","label","color","shape","level"), drop=FALSE] else
            data.frame(id=character(), label=character(), color=character(), shape=character(), level=numeric(), stringsAsFactors=FALSE)
          ed <- if (nrow(ed)) ed[, c("from","to"), drop=FALSE] else
            data.frame(from=character(), to=character(), stringsAsFactors=FALSE)
          saved[[paste0("graph_", pid)]] <- list(nodes = nd, edges = ed)
        }
      }
      saved_pipelines$data <- saved
      
      # update Step 2a dropdown
      choices <- names(saved_pipelines$data)
      choices <- choices[!grepl("^graph_", choices)]
      updateSelectInput(session, "selected_equivalence_pipeline",
                        choices = choices, selected = if (length(choices)) choices[1] else NULL)
    }
    
    # --- Per-pipeline metric/type (STORE ONLY; UI will read from this)
    step2a_state$metrics <- list()
    if (!is.null(metrics_df) && nrow(metrics_df)) {
      for (i in seq_len(nrow(metrics_df))) {
        pid <- as.character(metrics_df$pipeline_id[i])
        step2a_state$metrics[[pid]] <- list(
          quality_metric_value = as.character(metrics_df$quality_metric_value[i] %||% ""),
          equivalence_type     = as.character(metrics_df$equivalence_type[i] %||% "")
        )
      }
    }
  }
  
  # Clear and rebuild Step 1a UI/graph from DataFrames
  restore_step1a_from_dfs <- function(step1a_nodes, step1a_options,
                                      step1a_scope = NULL, step1a_methods = NULL,
                                      step1a_analysis = NULL) {
    # 0) Safety
    req(is.data.frame(step1a_nodes), is.data.frame(step1a_options))
    
    # 1) Clear Step 1a UI and state
    removeUI(selector = "#accordion_container_step1a > *", multiple = TRUE)
    active_steps(character(0))
    next_step_id(0)
    step_options <<- reactiveValues()
    
    # 2) Recreate the wrapper your app expects so Add Node keeps working
    insertUI(
      selector = "#accordion_container_step1a",
      where   = "beforeEnd",
      ui = div(id = "main_accordion", class = "panel-group")
    )
    
    # 3) Restore Scope / Methods (values + checkboxes)
    if (!is.null(step1a_scope) && nrow(step1a_scope)) {
      saved_step1a$scope <- list(
        checkboxes = as.logical(step1a_scope$checked),
        texts      = as.character(step1a_scope$description)
      )
      # reflect into UI
      updateCheckboxInput(session, "aspect1a_1", value = saved_step1a$scope$checkboxes[1])
      updateCheckboxInput(session, "aspect1a_2", value = saved_step1a$scope$checkboxes[2])
      updateCheckboxInput(session, "aspect1a_3", value = saved_step1a$scope$checkboxes[3])
      updateCheckboxInput(session, "aspect1a_4", value = saved_step1a$scope$checkboxes[4])
      updateTextInput(session, "aspect1a_text_1", value = saved_step1a$scope$texts[1])
      updateTextInput(session, "aspect1a_text_2", value = saved_step1a$scope$texts[2])
      updateTextInput(session, "aspect1a_text_3", value = saved_step1a$scope$texts[3])
      updateTextInput(session, "aspect1a_text_4", value = saved_step1a$scope$texts[4])
    }
    if (!is.null(step1a_methods) && nrow(step1a_methods)) {
      saved_step1a$methods <- list(
        checkboxes = as.logical(step1a_methods$checked),
        texts      = as.character(step1a_methods$description)
      )
      updateCheckboxInput(session, "defens1a_1", value = saved_step1a$methods$checkboxes[1])
      updateCheckboxInput(session, "defens1a_2", value = saved_step1a$methods$checkboxes[2])
      updateCheckboxInput(session, "defens1a_3", value = saved_step1a$methods$checkboxes[3])
      updateCheckboxInput(session, "defens1a_4", value = saved_step1a$methods$checkboxes[4])
      updateTextInput(session, "defens1a_text_1", value = saved_step1a$methods$texts[1])
      updateTextInput(session, "defens1a_text_2", value = saved_step1a$methods$texts[2])
      updateTextInput(session, "defens1a_text_3", value = saved_step1a$methods$texts[3])
      updateTextInput(session, "defens1a_text_4", value = saved_step1a$methods$texts[4])
    }
    
    if (!is.null(step1a_analysis) && is.data.frame(step1a_analysis) && nrow(step1a_analysis)) {
      # Normalize to UTF-8
      for (nm in names(step1a_analysis)) if (is.character(step1a_analysis[[nm]])) step1a_analysis[[nm]] <- enc2utf8(step1a_analysis[[nm]])
      
      saved_step1a$analysis <- data.frame(
        field = as.character(step1a_analysis$field %||% character(0)),
        value = as.character(step1a_analysis$value %||% character(0)),
        stringsAsFactors = FALSE
      )
      
      # tolerant matching on field labels
      low_fields <- tolower(saved_step1a$analysis$field)
      get_val <- function(keys) {
        ix <- which(low_fields %in% keys)
        if (length(ix)) saved_step1a$analysis$value[ix[1]] else ""
      }
      
      desc  <- get_val(c("descriptive analysis","descriptive","descriptive_analysis"))
      infer <- get_val(c("inferential analysis","inferential","inferential_analysis","inference"))
      mcc   <- get_val(c("multiple comparisons correction","multiple comparisons","multiplicity correction","mcc"))
      
      updateTextInput(session, "step1a_descriptive_analysis",  value = desc)
      updateTextInput(session, "step1a_inferential_analysis",  value = infer)
      updateTextInput(session, "step1a_multiple_correction",   value = mcc)
    }
    
    # 4) Insert panels + inputs and set headings (do NOT depend on "Done")
    #    Map CSV row i -> new step id i (string) so ids are stable.
    if (nrow(step1a_nodes) > 0) {
      for (i in seq_len(nrow(step1a_nodes))) {
        new_id <- as.character(i)
        next_step_id(as.integer(new_id))
        active_steps(c(active_steps(), new_id))
        step_options[[new_id]] <- list()
        
        insertUI(
          selector = "#main_accordion",
          where   = "beforeEnd",
          ui = create_accordion(new_id)
        )
        
        # options for this node
        opts_i <- step1a_options[step1a_options$node_index == i, , drop = FALSE]
        if (nrow(opts_i)) {
          for (j in seq_len(nrow(opts_i))) {
            existing <- suppressWarnings(as.numeric(names(step_options[[new_id]])))
            option_id <- if (length(existing) == 0 || all(is.na(existing))) 1 else max(existing, na.rm = TRUE) + 1
            unique_option_id <- as.character(option_id)
            step_options[[new_id]][[unique_option_id]] <- paste0("Option ", option_id)
            
            insertUI(
              selector = paste0("#options_container_", new_id),
              where   = "beforeEnd",
              ui = div(
                id    = paste0("option_wrapper_", new_id, "_", unique_option_id),
                style = "display: flex; align-items: center; margin-bottom: 5px;",
                span(id = paste0("option_label_", new_id, "_", unique_option_id),
                     strong(paste0("Option ", option_id))),
                textInput(paste0("option_", new_id, "_", unique_option_id),
                          label = NULL,
                          value = as.character(opts_i$option_label[j] %||% ""),
                          width = "70%"),
                actionButton(paste0("delete_option_", new_id, "_", unique_option_id),
                             "❌", class = "btn-danger btn-sm", style = "margin-left: 10px;")
              )
            )
          }
        }
      }
    }
    
    # After all accordions are inserted, set their titles once the DOM is bound
    session$onFlushed(function() {
      if (nrow(step1a_nodes) > 0) {
        for (i in seq_len(nrow(step1a_nodes))) {
          sid <- as.character(i)
          node_title_i <- as.character(step1a_nodes$node_title[i] %||% "")
          # set the textInput value
          updateTextInput(session, paste0("step_name_", sid), value = node_title_i)
          # set the accordion header text
          runjs(sprintf(
            "$('#heading_%s .panel-title a').text(%s);",
            sid,
            jsonlite::toJSON(node_title_i, auto_unbox = TRUE)  # safe escaping
          ))
        }
      }
    }, once = TRUE)
    
    
    # 5) Rebuild Step 1a graph **from the CSVs** (no timing race with inputs)
    nodes_df <- data.frame(id = character(0), label = character(0),
                           shape = character(0), color = character(0),
                           stringsAsFactors = FALSE)
    edges_df <- data.frame(from = character(0), to = character(0),
                           stringsAsFactors = FALSE)
    
    if (nrow(step1a_nodes) > 0) {
      for (i in seq_len(nrow(step1a_nodes))) {
        sid   <- as.character(i)
        label <- as.character(step1a_nodes$node_title[i] %||% "")
        nodes_df <- rbind(nodes_df,
                          data.frame(id = sid, label = label, shape = "circle",
                                     color = NA, stringsAsFactors = FALSE))
        
        opts_i <- step1a_options[step1a_options$node_index == i, , drop = FALSE]
        if (nrow(opts_i)) {
          for (j in seq_len(nrow(opts_i))) {
            oid   <- paste0(sid, "_", j)
            olbl  <- as.character(opts_i$option_label[j] %||% "")
            nodes_df <- rbind(nodes_df,
                              data.frame(id = oid, label = olbl, shape = "box",
                                         color = NA, stringsAsFactors = FALSE))
            edges_df <- rbind(edges_df,
                              data.frame(from = sid, to = oid, stringsAsFactors = FALSE))
          }
        }
      }
    }
    graph_data_1a$nodes <- nodes_df
    graph_data_1a$edges <- edges_df
    
    # 6) Give the input bindings a tick before Step 1b observes rebuild
    if (requireNamespace("later", quietly = TRUE)) {
      later::later(function(){}, delay = 0.05)
    }
  }
  
  # Clear and rebuild Step 1b UI/graph from Data Frames
  restore_step1b_from_dfs <- function(step1b_nodes = NULL, step1b_options = NULL) {
    if ((is.null(step1b_nodes)   || !nrow(step1b_nodes)) &&
        (is.null(step1b_options) || !nrow(step1b_options))) {
      return(invisible())
    }
    
    session$onFlushed(function() {
      # --- Seed step-level radios and justifications
      if (!is.null(step1b_nodes) && nrow(step1b_nodes)) {
        for (i in seq_len(nrow(step1b_nodes))) {
          sid <- as.character(step1b_nodes$node_index[i])
          def <- as.character(step1b_nodes$defensibility[i] %||% "")
          jus <- as.character(step1b_nodes$justification[i] %||% "")
          if (nzchar(def)) updateRadioButtons(session, paste0("defensibility_step_", sid), selected = def)
          updateTextInput(session, paste0("justification_step_", sid), value = jus)
        }
      }
      
      # --- Seed option-level radios and justifications
      if (!is.null(step1b_options) && nrow(step1b_options)) {
        for (r in seq_len(nrow(step1b_options))) {
          sid <- as.character(step1b_options$node_index[r])
          ok  <- as.character(step1b_options$option_index[r])
          def <- as.character(step1b_options$defensibility[r] %||% "")
          jus <- as.character(step1b_options$justification[r] %||% "")
          if (nzchar(def)) updateRadioButtons(session, paste0("defensibility_option_", sid, "_", ok), selected = def)
          updateTextInput(session, paste0("justification_option_", sid, "_", ok), value = jus)
        }
      }
      
      # --- Recolor the Step 1b network to reflect imported defensibility
      isolate({
        # work on local copies to avoid partial updates
        nodes_df <- graph_data_1b$nodes
        
        if (!is.null(step1b_nodes) && nrow(step1b_nodes) && nrow(nodes_df)) {
          for (i in seq_len(nrow(step1b_nodes))) {
            sid <- as.character(step1b_nodes$node_index[i])
            col <- if (identical(step1b_nodes$defensibility[i], "Defensible")) "#5cb85c"
            else if (identical(step1b_nodes$defensibility[i], "Indefensible")) "#d9534f"
            else NA
            if (!is.na(col)) nodes_df$color[nodes_df$id == sid] <- col
          }
        }
        if (!is.null(step1b_options) && nrow(step1b_options) && nrow(nodes_df)) {
          for (r in seq_len(nrow(step1b_options))) {
            sid <- as.character(step1b_options$node_index[r])
            ok  <- as.character(step1b_options$option_index[r])
            oid <- paste0(sid, "_", ok)
            col <- if (identical(step1b_options$defensibility[r], "Defensible")) "#5cb85c"
            else if (identical(step1b_options$defensibility[r], "Indefensible")) "#d9534f"
            else NA
            if (!is.na(col)) nodes_df$color[nodes_df$id == oid] <- col
          }
        }
        
        # assign the updated copy back once
        graph_data_1b$nodes <- nodes_df
      })
      
      # --- Re-render the network safely (read via isolate)
      output$pipeline_network_step1b <- renderVisNetwork({
        nodes <- isolate(graph_data_1b$nodes)
        edges <- isolate(graph_data_1b$edges)
        if (is.null(nodes) || is.null(edges)) return(NULL)
        
        visNetwork(nodes, edges) %>%
          visEdges(arrows = "to", color = "#000000") %>%
          visNodes(color = list(background = nodes$color, border = "#000000")) %>%
          visLayout(hierarchical = TRUE) %>%
          visPhysics(enabled = FALSE) %>%
          visInteraction(
            dragNodes = FALSE, dragView = FALSE, zoomView = FALSE,
            selectable = FALSE, multiselect = FALSE, hover = FALSE,
            keyboard = FALSE, navigationButtons = FALSE
          )
      })
    }, once = TRUE)
  }
  
  
  
  
################################################################################

################################### Welcome ####################################
  # ---------- Welcome ----------
  observeEvent(input$btn_next_info, {
    updateTabsetPanel(session, "welcome_tabs", selected = "start")
  })
  
  observeEvent(input$btn_prev_start, {
    updateTabsetPanel(session, "welcome_tabs", selected = "info")
  })
  
  observeEvent(input$btn_next_start, {
    updateNavbarPage(session, "main_nav", selected = "mv1")
    shinyjs::delay(0, updateTabsetPanel(session, "mv1_tabs", selected = "1a"))
  })
  # Save username
  observeEvent(input$save_username, {
    
    username <- input$user_nickname
    
    # Validation
    if (grepl("^[A-Za-z0-9_-]+$", username) && nchar(username) > 0) {
      showNotification(
        paste0("Username '", username, "' saved successfully."),
        type = "message",
        duration = 3
      )
      # Save to reactiveVal or other storage
      # saved_username(username)
      
    } else {
      showNotification(
        "Invalid username: only letters, numbers, underscores (_) and hyphens (-) are allowed. No spaces.",
        type = "error",
        duration = 5
      )
    }
  })
  
  # Upload Progress ZIP Folder
  observeEvent(input$load_progress, {
    req(input$progress_file)
    fpath <- input$progress_file$datapath
    fname <- tolower(input$progress_file$name)
    
    # Unpack ZIP or read CSV(s)
    if (grepl("\\.zip$", fname)) {
      tmpdir <- file.path(tempdir(), paste0("imp_", as.integer(runif(1,1e6,1e7))))
      dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(fpath, exdir = tmpdir)
      # Try to read manifest (optional but preferred)
      man_path <- file.path(tmpdir, "manifest.json")
      if (file.exists(man_path)) {
        man <- tryCatch(jsonlite::fromJSON(man_path), error = function(e) NULL)
      } else {
        man <- NULL
      }
      
      # Collect any Step 1a CSVs present
      scope_path   <- file.path(tmpdir, "step1a_scope.csv")
      methods_path <- file.path(tmpdir, "step1a_methods.csv")
      nodes_path   <- file.path(tmpdir, "step1a_nodes.csv")
      opts_path    <- file.path(tmpdir, "step1a_options.csv")
      step1a_analysis_path <- file.path(tmpdir, "step1a_analysis.csv")
      step1b_nodes_path   <- file.path(tmpdir, "step1b_nodes.csv")
      step1b_options_path <- file.path(tmpdir, "step1b_options.csv")
      step1c_pipelines_path   <- file.path(tmpdir, "step1c_pipelines.csv")
      step1c_graph_nodes_path <- file.path(tmpdir, "step1c_graph_nodes.csv")
      step1c_graph_edges_path <- file.path(tmpdir, "step1c_graph_edges.csv")
      p2a_meta  <- file.path(tmpdir, "step2a_meta.csv")
      p2a_mx    <- file.path(tmpdir, "step2a_pipeline_metrics.csv")
      p2a_seq   <- file.path(tmpdir, "step2a_pipelines.csv")
      p2a_nodes <- file.path(tmpdir, "step2a_graph_nodes.csv")
      p2a_edges <- file.path(tmpdir, "step2a_graph_edges.csv")
      
      # If it's a Step 2a-only ZIP (no Step 1a CSVs), still restore cleanly:
      has_1a <- file.exists(file.path(tmpdir, "step1a_nodes.csv")) &&
        file.exists(file.path(tmpdir, "step1a_options.csv"))
      has_2a_only <- any(file.exists(c(p2a_meta, p2a_mx, p2a_seq, p2a_nodes, p2a_edges)))
      
      if (!has_1a && has_2a_only) {
        showNotification(
          "Step 2a uploads are not supported here. Please use the upload area in the Step 2a tab.",
          type = "error", duration = 8000
        )
        unlink(tmpdir, recursive = TRUE)
        return()
      }
      
      if (!has_1a) {
        showNotification("ZIP is missing Step 1a node/option CSVs; nothing to load for Step 1a.", 
                         type = "error", duration = 6)
        unlink(tmpdir, recursive = TRUE, force = TRUE)
        return()
      }
      
      # Read with forgiving defaults
      read_safely <- function(p) {
        out <- tryCatch(utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE,
                                        fileEncoding = "UTF-8"),
                        error = function(e) NULL)
        if (is.null(out)) return(NULL)
        for (nm in names(out)) if (is.character(out[[nm]])) out[[nm]] <- enc2utf8(out[[nm]])
        out
      }
      
      step1a_nodes   <- read_safely(nodes_path)
      step1a_options <- read_safely(opts_path)
      step1a_scope   <- if (file.exists(scope_path))   read_safely(scope_path)   else NULL
      step1a_methods <- if (file.exists(methods_path)) read_safely(methods_path) else NULL
      step1a_analysis <- if (file.exists(step1a_analysis_path)) read_safely(step1a_analysis_path) else NULL
      step1b_nodes   <- if (file.exists(step1b_nodes_path))   read_safely(step1b_nodes_path)   else NULL
      step1b_options <- if (file.exists(step1b_options_path)) read_safely(step1b_options_path) else NULL
      step1c_pipelines   <- if (file.exists(step1c_pipelines_path))   read_safely(step1c_pipelines_path)   else NULL
      step1c_graph_nodes <- if (file.exists(step1c_graph_nodes_path)) read_safely(step1c_graph_nodes_path) else NULL
      step1c_graph_edges <- if (file.exists(step1c_graph_edges_path)) read_safely(step1c_graph_edges_path) else NULL
      step2a_meta    <- if (file.exists(p2a_meta))  read_safely(p2a_meta)  else NULL
      step2a_metrics <- if (file.exists(p2a_mx))    read_safely(p2a_mx)    else NULL
      step2a_pipes   <- if (file.exists(p2a_seq))   read_safely(p2a_seq)   else NULL
      step2a_gnodes  <- if (file.exists(p2a_nodes)) read_safely(p2a_nodes) else NULL
      step2a_gedges  <- if (file.exists(p2a_edges)) read_safely(p2a_edges) else NULL
      
      if (is.null(step1a_nodes) || is.null(step1a_options)) {
        showNotification("Could not parse Step 1a CSVs.", type = "error", duration = 6)
        return()
      }
      
      # Basic schema checks
      required_nodes <- c("node_index","node_title")
      required_opts  <- c("node_index","option_index","option_label")
      if (!all(required_nodes %in% names(step1a_nodes))) {
        showNotification("step1a_nodes.csv missing required columns.", type = "error"); return()
      }
      if (!all(required_opts %in% names(step1a_options))) {
        showNotification("step1a_options.csv missing required columns.", type = "error"); return()
      }
      if (!is.null(step1a_analysis)) {
        req_cols_a <- c("field","value")
        if (!all(req_cols_a %in% names(step1a_analysis))) {
          showNotification("step1a_analysis.csv schema is invalid; skipping analysis restore.", type = "warning")
          step1a_analysis <- NULL
        }
      }
      
      # Restore Step 1a
      restore_step1a_from_dfs(step1a_nodes, step1a_options, step1a_scope, step1a_methods, step1a_analysis)
      showNotification("Saved progress uploaded.", type = "message")
      restore_step1b_from_dfs(step1b_nodes, step1b_options)
      
      # Restore 1c if present
      if (!is.null(step1c_pipelines) && !is.null(step1c_graph_nodes) && !is.null(step1c_graph_edges)) {
        # Basic schema checks
        req_cols_p <- c("pipeline_id","order_index","label")
        req_cols_n <- c("pipeline_id","id","label","color","shape","level")
        req_cols_e <- c("pipeline_id","from","to")
        if (all(req_cols_p %in% names(step1c_pipelines)) &&
            all(req_cols_n %in% names(step1c_graph_nodes)) &&
            all(req_cols_e %in% names(step1c_graph_edges))) {
          restore_step1c_from_dfs(step1c_pipelines, step1c_graph_nodes, step1c_graph_edges)
        } else {
          showNotification("Found 1c CSVs but schema was invalid; skipping 1c restore.", type = "warning")
        }
      }
      
      # NEW (store): keep imported node titles for later header fix
      session$userData$last_import_nodes <- step1a_nodes
      
      # NEW (one-time header fix): when user opens 1b, stamp titles onto 1b headers
      observeEvent(input$mv1_tabs, {
        if (identical(input$mv1_tabs, "1b")) {
          shinyjs::delay(0, {
            ln <- session$userData$last_import_nodes
            if (!is.null(ln) && nrow(ln) > 0) {
              for (i in seq_len(nrow(ln))) {
                lbl <- as.character(ln$node_title[i])
                if (!nzchar(lbl)) lbl <- paste0("Node ", i)
                runjs(sprintf(
                  "$('#heading_step1b_%s .panel-title a').text(%s);",
                  i,
                  jsonlite::toJSON(lbl, auto_unbox = TRUE)
                ))
              }
            }
          })
        }
      }, once = TRUE, ignoreInit = TRUE)
      
      # Jump user to 1a for clarity
      updateNavbarPage(session, "main_nav", selected = "mv1")
      shinyjs::delay(0, updateTabsetPanel(session, "mv1_tabs", selected = "1a"))
       
      # Jump user to 1a for clarity
      updateNavbarPage(session, "main_nav", selected = "mv1")
      shinyjs::delay(0, updateTabsetPanel(session, "mv1_tabs", selected = "1a"))
      
    } else if (grepl("\\.csv$", fname)) {
      # Single CSV import(s). Allow users to upload in any order; we accumulate until we have both.
      # For simplicity: accept either nodes/options combined (rare) or one at a time.
      df <- tryCatch(utils::read.csv(fpath, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
      if (is.null(df)) { showNotification("Could not read CSV.", type = "error"); return() }
      
      # Store temporarily in session for combination
      if (all(c("node_index","node_title") %in% names(df))) {
        session$userData$imp_nodes <- df
        showNotification("Loaded step1a_nodes.csv. Now add step1a_options.csv (or import ZIP).", type = "message")
      } else if (all(c("node_index","option_index","option_label") %in% names(df))) {
        session$userData$imp_opts <- df
        showNotification("Loaded step1a_options.csv. If you also provide step1a_nodes.csv, I’ll rebuild the UI.", type = "message")
      } else if ("method" %in% names(df) && "checked" %in% names(df) && "description" %in% names(df)) {
        session$userData$imp_methods <- df
        showNotification("Loaded step1a_methods.csv.", type = "message")
      } else if ("category" %in% names(df) && "checked" %in% names(df) && "description" %in% names(df)) {
        session$userData$imp_scope <- df
        showNotification("Loaded step1a_scope.csv.", type = "message")
      } else {
        showNotification("CSV does not match any supported Step 1a schema.", type = "error")
        return()
      }
      
      if (!is.null(session$userData$imp_nodes) && !is.null(session$userData$imp_opts)) {
        restore_step1a_from_dfs(
          session$userData$imp_nodes,
          session$userData$imp_opts,
          session$userData$imp_scope %||% NULL,
          session$userData$imp_methods %||% NULL
        )
        showNotification("Step 1a progress loaded from CSVs.", type = "message")
        
        # NEW (store)
        session$userData$last_import_nodes <- session$userData$imp_nodes
        
        # NEW (one-time header fix)
        observeEvent(input$mv1_tabs, {
          if (identical(input$mv1_tabs, "1b")) {
            shinyjs::delay(0, {
              ln <- session$userData$last_import_nodes
              if (!is.null(ln) && nrow(ln) > 0) {
                for (i in seq_len(nrow(ln))) {
                  lbl <- as.character(ln$node_title[i])
                  if (!nzchar(lbl)) lbl <- paste0("Node ", i)
                  runjs(sprintf(
                    "$('#heading_step1b_%s .panel-title a').text(%s);",
                    i,
                    jsonlite::toJSON(lbl, auto_unbox = TRUE)
                  ))
                }
              }
            })
          }
        }, once = TRUE, ignoreInit = TRUE)
        
        updateNavbarPage(session, "main_nav", selected = "mv1")
        shinyjs::delay(0, updateTabsetPanel(session, "mv1_tabs", selected = "1a"))
      }
    } else {
      showNotification("Unsupported file type. Please upload a ZIP or CSV.", type = "error")
    }
  })
  
################################################################################  
  
################################ Multiverse 1.0 ################################
  # ---------- Multiverse 1.0 ----------
  observeEvent(input$btn_prev_1a, {
    updateNavbarPage(session, "main_nav", selected = "welcome")
    shinyjs::delay(0, updateTabsetPanel(session, "welcome_tabs", selected = "start"))
  })
  observeEvent(input$btn_next_1a, {
    updateTabsetPanel(session, "mv1_tabs", selected = "1b")
  })
  
  observeEvent(input$btn_prev_1b, {
    updateTabsetPanel(session, "mv1_tabs", selected = "1a")
  })
  observeEvent(input$btn_next_1b, {
    updateTabsetPanel(session, "mv1_tabs", selected = "1c")
  })
  
  observeEvent(input$btn_prev_1c, {
    updateTabsetPanel(session, "mv1_tabs", selected = "1b")
  })
  observeEvent(input$btn_next_1c, {
    updateTabsetPanel(session, "mv1_tabs", selected = "1d")
  })
  
  observeEvent(input$btn_prev_1d, {
    updateTabsetPanel(session, "mv1_tabs", selected = "1c")
  })
  observeEvent(input$btn_next_1d, {
    updateNavbarPage(session, "main_nav", selected = "mv2")
    shinyjs::delay(0, updateTabsetPanel(session, "mv2_tabs", selected = "2a"))
  })
  
################# Step 1a: Define Multiverse Space #################
  
  graph_data_1a <- reactiveValues(
    nodes = data.frame(id = character(0), label = character(0), shape = character(0), color = character(0), stringsAsFactors = FALSE),
    edges = data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  )
  
  # Ensure stores exist
  justifications <- reactiveValues()
  stored_inputs <- reactiveValues(
    step_justifications = list(),
    option_justifications = list(),
    step_defensibility = list(),
    option_defensibility = list()
  )
  
  # --- Safe helpers for nested read/write on reactiveValues ---
  safe_set_nested <- function(rv, slot, key, value) {
    cur <- isolate(rv[[slot]])
    if (is.null(cur)) cur <- list()
    cur[[as.character(key)]] <- value
    rv[[slot]] <- cur
  }
  
  safe_get_nested <- function(rv, slot, key, default = "") {
    cur <- isolate(rv[[slot]])
    if (is.null(cur)) return(default)
    val <- cur[[as.character(key)]]
    if (is.null(val) || (is.character(val) && !nzchar(val))) default else val
  }
  
  # ---- Save answers for Scope and Methods
  saved_step1a <- reactiveValues(
    scope = NULL,
    methods = NULL,
    analysis = NULL
  )
  
  observeEvent(input$save_scope, {
    # Validate: at least one scope input is filled
    scope_inputs <- c(
      input$aspect1a_1, input$aspect1a_2, input$aspect1a_3, input$aspect1a_4,
      input$aspect1a_text_1, input$aspect1a_text_2, input$aspect1a_text_3, input$aspect1a_text_4
    )
    
    if (any(!is.null(scope_inputs) & scope_inputs != "" & scope_inputs != FALSE)) {
      saved_step1a$scope <- list(
        checkboxes = c(input$aspect1a_1, input$aspect1a_2, input$aspect1a_3, input$aspect1a_4),
        texts = c(input$aspect1a_text_1, input$aspect1a_text_2, input$aspect1a_text_3, input$aspect1a_text_4)
      )
      showNotification("✅ Scope answers saved successfully.", type = "message")
    } else {
      showNotification("⚠️ Please complete at least one Scope item before saving.", type = "error")
    }
  })
  
  observeEvent(input$save_methods, {
    method_inputs <- c(
      input$defens1a_1, input$defens1a_2, input$defens1a_3, input$defens1a_4,
      input$defens1a_text_1, input$defens1a_text_2, input$defens1a_text_3, input$defens1a_text_4
    )
    
    if (any(!is.null(method_inputs) & method_inputs != "" & method_inputs != FALSE)) {
      saved_step1a$methods <- list(
        checkboxes = c(input$defens1a_1, input$defens1a_2, input$defens1a_3, input$defens1a_4),
        texts = c(input$defens1a_text_1, input$defens1a_text_2, input$defens1a_text_3, input$defens1a_text_4)
      )
      showNotification("✅ Identification methods saved successfully.", type = "message")
    } else {
      showNotification("⚠️ Please complete at least one Identification Method before saving.", type = "error")
    }
  })
  
  observeEvent(input$save_analysis_plan_1a, {
    # pull & trim
    desc <- trimws(input$step1a_descriptive_analysis %||% "")
    infer <- trimws(input$step1a_inferential_analysis %||% "")
    mcc <- trimws(input$step1a_multiple_correction %||% "")
    
    # require at least one non-empty entry
    if (nzchar(desc) || nzchar(infer) || nzchar(mcc)) {
      saved_step1a$analysis <- data.frame(
        field = c("Descriptive analysis", "Inferential analysis", "Multiple comparisons correction"),
        value = c(desc, infer, mcc),
        stringsAsFactors = FALSE
      )
      showNotification("✅ Analysis plan saved.", type = "message")
    } else {
      showNotification("⚠️ Please enter at least one item before saving.", type = "error")
    }
  })
  
  #-------
  
  # Unique id counter (monotonic, never reused) and active step list
  next_step_id <- reactiveVal(0)          # e.g. 0 -> 1 -> 2 -> 3 ...
  active_steps <- reactiveVal(character(0))  # vector of character ids of steps currently on screen
  
  # Reactive values to track dynamic options for each step
  step_options <- reactiveValues()
  # Convenience alias used across 1a/1b to iterate existing nodes
  step_ids <- reactive({ active_steps() })
  
  # Infobox for Defensibility Methods
  observeEvent(input$open_methods_help, {
    showModal(
      modalDialog(
        title = "Identify Defensible Pipelines",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tagList(
          p("In this section, provide a brief description of the method you will use to determine which options, combinations of options, or pipelines are defensible in your multiverse analysis."),
          p("This might include criteria such as:"),
          tags$ul(
            tags$li("Inclusion in the peer‑reviewed literature"),
            tags$li("Considered defensible by a single expert"),
            tags$li("Considered defensible by a consortium of experts"),
            tags$li("Sampled as a defensible pipeline through crowdsourcing")
          ),
          p("Or using other criteria."),
          p("Please use the boxes below to report which criteria will be used to justify decisions regarding defensibility in your multiverse analysis. Be as specific as possible about the source guiding these judgments."),
          tags$hr(),
          strong("Example"),
          p("If you will judge defensibility solely based on inclusion in the peer‑reviewed literature, specify under ‘Literature review’ whether this will be via a new systematic review, an existing systematic review or knowledge space, or a non‑systematic summary.")
        )
      )
    )
  })
  
  # Infobox for Listing Decision Nodes and Options
  observeEvent(input$info_nodes, {
    showModal(modalDialog(
      title = "List Decision Nodes and Options to be Considered – Guidance",
      easyClose = TRUE,
      size = "l",
      tagList(
        p("In this section, you will define the decision nodes and all available options that will form part of your multiverse analysis."),
        h4("What is a decision node?"),
        p("A decision node represents a point in your research workflow where different methodological choices are possible."),
        h4("Typical examples of decision nodes include:"),
        tags$ul(
          tags$li(strong("Preprocessing:"), " Outlier detection, data transformation, missing value handling, feature scaling."),
          tags$li(strong("Measurement:"), " Choice of measurement instrument, operational definition of variables."),
          tags$li(strong("Model specification:"), " Choice of statistical model, inclusion/exclusion of covariates, interaction terms."),
          tags$li(strong("Estimation methods:"), " Maximum likelihood estimation, Bayesian estimation, bootstrapping.")
        ),
        h4("How to list options:"),
        tags$ul(
          tags$li("Use one text box per decision node to name the node (e.g., 'Preprocessing')."),
          tags$li("Under each node, use the smaller text boxes to enter all possible options (e.g., 'Z-score standardization', 'Log transformation')."),
          tags$li("Include all options, regardless of defensibility; defensibility will be addressed later."),
          tags$li("Include all nodes, even those with only one option or none at all."),
          tags$li("Each click on 'Add Option' creates one new option. If you only need one option for a node, add Option 1 and stop."),
          tags$li("If you add extra options by mistake, remove them using the ❌ button.")
        ),
        h4("Example entry:"),
        tags$ul(
          tags$li(
            strong("Node:"), " Outlier Detection",
            tags$br(),
            strong("Options:"), 
            " +/- 3 SD from group mean, +/- 2.5 SD from group mean, 
          Interquartile Range (IQR) method, Cook's distance > 4/n"
          ),
          tags$li(
            strong("Node:"), " Outlier Handling",
            tags$br(),
            strong("Options:"), 
            " Remove cases, Winsorize values, Apply robust statistical method"
          ),
          tags$li(
            strong("Node:"), " Missing Data Handling",
            tags$br(),
            strong("Options:"), 
            " Listwise deletion, Mean imputation, Multiple imputation, 
          Predictive mean matching"
          ),
          tags$li(
            strong("Node:"), " Covariate Inclusion",
            tags$br(),
            strong("Options:"), 
            " Include age only, Include age and gender, Include all measured covariates"
          ),
          tags$li(
            strong("Node:"), " Model Specification",
            tags$br(),
            strong("Options:"), 
            " Linear regression, Mixed-effects model, Generalized additive model (GAM)"
          )
        )
      )
    ))
  })
  
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
  
  # Add initial accordion container (run once)
  observeEvent(TRUE, {
    new_id <- as.character(next_step_id() + 1)
    next_step_id(as.integer(new_id))
    active_steps(c(active_steps(), new_id))
    step_options[[new_id]] <- list()
    
    insertUI(
      selector = "#accordion_container_step1a",
      where = "beforeEnd",
      ui = div(
        id = "main_accordion",
        class = "panel-group",
        create_accordion(new_id)
      )
    )
  }, once = TRUE)
  
  # Add new accordion on button click
  observeEvent(input$add_step, {
    new_id <- as.character(next_step_id() + 1)
    next_step_id(as.integer(new_id))
    active_steps(c(active_steps(), new_id))
    step_options[[new_id]] <- list()
    
    insertUI(
      selector = "#main_accordion",
      where = "beforeEnd",
      ui = create_accordion(new_id)
    )
  })
  
  # Add new text input for options dynamically
  observe({
    lapply(step_ids(), function(step_id) {
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
    lapply(step_ids(), function(step_id) {
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
    current <- active_steps()
    if (length(current) == 0) return()
    
    # delete the most recently added active node
    last_id <- tail(current, 1)
    
    removeUI(selector = paste0("#heading_", last_id), immediate = TRUE)
    removeUI(selector = paste0("#collapse_", last_id), immediate = TRUE)
    
    step_options[[last_id]] <- NULL
    active_steps(head(current, -1))
    
    graph_data_1a$nodes <- graph_data_1a$nodes[!grepl(paste0("^", last_id, "_?"), graph_data_1a$nodes$id), ]
    graph_data_1a$edges <- graph_data_1a$edges[graph_data_1a$edges$from != last_id & graph_data_1a$edges$to != last_id, ]
    
    print(paste("DEBUG: Step", last_id, "deleted from UI and network."))
  })
  
  ensure_node_df <- function(df) {
    if (!is.data.frame(df) || ncol(df) == 0) {
      return(data.frame(
        id = character(0), label = character(0),
        shape = character(0), color = character(0),
        stringsAsFactors = FALSE
      ))
    }
    for (nm in c("id","label","shape","color")) if (!(nm %in% names(df))) df[[nm]] <- NA_character_
    df[, c("id","label","shape","color")]
  }
  
  ensure_edge_df <- function(df) {
    if (!is.data.frame(df) || ncol(df) == 0) {
      return(data.frame(
        from = character(0), to = character(0),
        stringsAsFactors = FALSE
      ))
    }
    for (nm in c("from","to")) if (!(nm %in% names(df))) df[[nm]] <- NA_character_
    df[, c("from","to")]
  }
  
  # Update accordion titles dynamically and create network nodes/edges
  observe({
    lapply(step_ids(), function(step_id) {
      observeEvent(input[[paste0("done_", step_id)]], {
        step_key <- as.character(step_id)
        step_name <- input[[paste0("step_name_", step_id)]]
        
        # ✅ **Ensure the structure of nodes and edges before adding new data**
        graph_data_1a$nodes <- ensure_node_df(graph_data_1a$nodes)
        graph_data_1a$edges <- ensure_edge_df(graph_data_1a$edges)
        
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
          data.frame(id = step_id, label = step_name, shape = "circle",
                     color = NA_character_, stringsAsFactors = FALSE)
        )
        
        # ✅ **Add option nodes & edges only if options exist**
        if (length(options) > 0) {
          option_nodes <- data.frame(
            id = names(options),
            label = options,
            shape = "box",
            color = NA_character_,
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
          "$('#heading_%s .panel-title a').text('%s');",
          step_id,
          step_name
        ))
        
        print(paste("DEBUG: Updated graph for Step", step_id, "with options:", paste(options, collapse = ", ")))
        
      }, ignoreInit = TRUE)
    })
  })
  
  # Save All Decision Nodes and Options Button
  observeEvent(input$save_nodes_options, {
    ids <- active_steps()
    if (length(ids) == 0) {
      showNotification("⚠️ No nodes present to save.", type = "error")
      return()
    }
    
    # Build a clean snapshot of current UI values
    records <- lapply(ids, function(id) {
      node_name <- trimws(input[[paste0("step_name_", id)]] %||% "")
      opts <- character(0)
      
      node_opts <- step_options[[id]]
      if (!is.null(node_opts) && length(node_opts) > 0) {
        opt_ids <- names(node_opts)
        vals <- vapply(opt_ids, function(oid) {
          val <- input[[paste0("option_", id, "_", oid)]]
          if (is.null(val)) "" else trimws(val)
        }, FUN.VALUE = character(1))
        opts <- vals[vals != ""]
      }
      
      list(id = id, node = node_name, options = opts)
    })
    names(records) <- ids
    
    # Validation: at least one non-empty node name OR one non-empty option
    any_name   <- any(vapply(records, function(x) nchar(x$node) > 0, logical(1)))
    any_option <- any(vapply(records, function(x) length(x$options) > 0, logical(1)))
    
    if (!(any_name || any_option)) {
      showNotification("⚠️ Please enter at least one node name or one option before saving.", type = "error")
      return()
    }
    
    # Save snapshot
    saved_step1a$structure   <- records
    saved_step1a$graph_nodes <- graph_data_1a$nodes
    saved_step1a$graph_edges <- graph_data_1a$edges
    
    # Soft warning if some nodes have no name
    nameless <- which(vapply(records, function(x) identical(x$node, ""), logical(1)))
    if (length(nameless) > 0) {
      showNotification("ℹ️ Saved, but some nodes have no name. Consider naming them for clarity.", type = "warning")
    } else {
      showNotification("✅ All nodes and options saved.", type = "message")
    }
  })
  
  # Render the visNetwork graph
  output$pipeline_network_1a <- renderVisNetwork({
    visNetwork(graph_data_1a$nodes, graph_data_1a$edges) %>%
      visEdges(arrows = "to", color = "#000000") %>%
      visLayout(hierarchical = TRUE) %>%
      # Freeze the layout so nothing moves
      visPhysics(enabled = FALSE) %>%
      # Make it static: no zoom, no pan, no drag, no selection
      visInteraction(
        dragNodes = FALSE,
        dragView  = FALSE,
        zoomView  = FALSE,
        selectable = FALSE,
        multiselect = FALSE,
        hover = FALSE,
        keyboard = FALSE,
        navigationButtons = FALSE
      )
  })

  # Download Save Progress Step 1a
  output$export_zip_step1a <- downloadHandler(
    filename = function() {
      nickname <- input$user_nickname
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      if (is.null(nickname) || nickname == "") paste0("mv_step1a_", ts, ".zip")
      else paste0(nickname, "_mv_step1a_", ts, ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      # ---- Hard guards: make sure required packages exist
      if (!requireNamespace("zip", quietly = TRUE)) {
        stop("The 'zip' package is not installed on this server. Please install.packages('zip').")
      }
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("The 'jsonlite' package is not installed on this server. Please install.packages('jsonlite').")
      }
      
      # ---- Build data frames from current UI state
      dfs <- build_step1a_dfs()
      
      # ---- Prepare export directory and files (relative names)
      outdir <- tempfile("mv1a_")
      dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
      on.exit({ try(unlink(outdir, recursive = TRUE, force = TRUE), silent = TRUE) }, add = TRUE)
      
      csv_files <- c(
        "step1a_scope.csv",
        "step1a_methods.csv",
        "step1a_nodes.csv",
        "step1a_options.csv",
        "step1a_analysis.csv"
      )
      rel_files <- c(csv_files, "manifest.json")
      
      scope_path    <- file.path(outdir, "step1a_scope.csv")
      methods_path  <- file.path(outdir, "step1a_methods.csv")
      nodes_path    <- file.path(outdir, "step1a_nodes.csv")
      opts_path     <- file.path(outdir, "step1a_options.csv")
      analysis_path <- file.path(outdir, "step1a_analysis.csv")
      
      # Always write CSVs — even if empty
      write_csv_safely(dfs$scope,   scope_path)
      write_csv_safely(dfs$methods, methods_path)
      write_csv_safely(dfs$nodes,   nodes_path)
      write_csv_safely(dfs$options, opts_path)
      write_csv_safely(dfs$analysis, analysis_path)
      
      # Manifest
      write_manifest_json(outdir, exported_step = "1a", csvs = rel_files[1:4])
      
      # Sanity check: everything exists and is non-empty schema-wise
      for (p in file.path(outdir, rel_files)) {
        if (!file.exists(p)) stop("Internal error: expected file not found: ", basename(p))
      }
      
      # ---- Zip using RELATIVE paths from inside outdir
      old_wd <- getwd()
      setwd(outdir)
      on.exit({ try(setwd(old_wd), silent = TRUE) }, add = TRUE)
      
      tryCatch({
        zip::zipr(zipfile = file, files = rel_files, include_directories = FALSE)
        if (!file.exists(file) || isTRUE(file.size(file) == 0)) {
          stop("ZIP creation failed — produced empty file.")
        }
      }, error = function(e) {
        stop("Could not create ZIP: ", conditionMessage(e))
      })
    }
  )
  
############################################################## 
  
############### Step 1b: Define Defensibility ################
  
  graph_data_1b <- reactiveValues(
    nodes = data.frame(id = character(0), label = character(0), shape = character(0), color = character(0), stringsAsFactors = FALSE),
    edges = data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  )
  
  # Tracks the last rendered structure of Step 1b so we don't rebuild unnecessarily
  last_step1b_layout <- reactiveVal("")
  
  # Sync Step 1b network with Step 1a whenever Step 1b is accessed
  observe({
    if (!is.null(graph_data_1a$nodes) && nrow(graph_data_1a$nodes) > 0) {
      graph_data_1b$nodes <- graph_data_1a$nodes
      graph_data_1b$edges <- graph_data_1a$edges
    }
    print("DEBUG: Synced Step 1b network with Step 1a")
  })
  
  # --- Store step justifications ---
  observe({
    req(input)
    for (step_id in step_ids()) {
      key <- paste0("justification_step_", step_id)
      val <- input[[key]]
      if (!is.null(val)) {
        stored_inputs$step_justifications[[key]] <- val
        justifications[[key]] <- val
      }
    }
  })
  
  # --- Store option justifications (use REAL option keys) ---
  observe({
    req(input)
    for (step_id in step_ids()) {
      step_key <- as.character(step_id)
      opt_keys <- names(step_options[[step_key]])
      if (!is.null(opt_keys) && length(opt_keys) > 0) {
        for (ok in opt_keys) {
          key <- paste0("justification_option_", step_id, "_", ok)
          val <- input[[key]]
          if (!is.null(val)) {
            stored_inputs$option_justifications[[key]] <- val
            justifications[[key]] <- val
          }
        }
      }
    }
  })
  
  # --- Store step defensibility ---
  observe({
    req(input)
    for (step_id in step_ids()) {
      key <- paste0("defensibility_step_", step_id)
      val <- input[[key]]
      if (!is.null(val)) {
        stored_inputs$step_defensibility[[key]] <- val
      }
    }
  })
  
  # --- Store option defensibility (use REAL option keys) ---
  observe({
    req(input)
    for (step_id in step_ids()) {
      step_key <- as.character(step_id)
      opt_keys <- names(step_options[[step_key]])
      if (!is.null(opt_keys) && length(opt_keys) > 0) {
        for (ok in opt_keys) {
          key <- paste0("defensibility_option_", step_id, "_", ok)
          val <- input[[key]]
          if (!is.null(val)) {
            stored_inputs$option_defensibility[[key]] <- val
          }
        }
      }
    }
  })
  
  # --- Build Step 1b UI (use REAL option keys) ---
  observe({
    # Clear current 1b accordion
    removeUI(selector = "#accordion_container_step1b > *", multiple = TRUE)
    
    ids <- step_ids()
    if (!length(ids)) return()
    
    # helpers: pull labels from graph first; fall back to inputs; then to "Node X"
    get_step_label <- function(step_id) {
      sid <- as.character(step_id)
      
      # 1) from input (works even before "Done" is clicked)
      from_input <- isolate(input[[paste0("step_name_", sid)]])
      if (!is.null(from_input) && nzchar(from_input)) return(from_input)
      
      # 2) from graph (available after "Done"/restore)
      from_graph <- tryCatch({
        v <- graph_data_1a$nodes$label[graph_data_1a$nodes$id == sid]
        if (length(v) && nzchar(v[1])) v[1] else NA_character_
      }, error = function(e) NA_character_)
      if (is.character(from_graph) && nzchar(from_graph)) return(from_graph)
      
      # 3) fallback
      paste0("Node ", sid)
    }
    
    get_option_label <- function(step_id, ok) {
      sid <- as.character(step_id)
      oid <- paste0(sid, "_", ok)
      
      # 1) from input (works even before "Done" is clicked)
      from_input <- isolate(input[[paste0("option_", sid, "_", ok)]])
      if (!is.null(from_input) && nzchar(from_input)) return(from_input)
      
      # 2) from graph (available after "Done"/restore)
      from_graph <- tryCatch({
        v <- graph_data_1a$nodes$label[graph_data_1a$nodes$id == oid]
        if (length(v) && nzchar(v[1])) v[1] else NA_character_
      }, error = function(e) NA_character_)
      if (is.character(from_graph) && nzchar(from_graph)) return(from_graph)
      
      # 3) fallback
      paste0("Option ", ok)
    }
    
    for (i in seq_along(ids)) {
      step_id  <- ids[i]
      step_key <- as.character(step_id)
      
      step_label <- get_step_label(step_id)
      is_first <- (i == 1)
      
      insertUI(
        selector = "#accordion_container_step1b",
        where    = "beforeEnd",
        ui = div(
          class = "panel panel-default",
          div(
            class = "panel-heading",
            role  = "tab",
            id    = paste0("heading_step1b_", step_id),
            h4(
              class = "panel-title",
              tags$a(
                href          = paste0("#collapse_step1b_", step_id),
                "data-toggle" = "collapse",
                "data-parent" = "#accordion_container_step1b",
                "aria-expanded" = if (is_first) "true" else "false",
                "aria-controls" = paste0("collapse_step1b_", step_id),
                step_label
              )
            )
          ),
          div(
            id    = paste0("collapse_step1b_", step_id),
            class = if (is_first) "panel-collapse collapse in" else "panel-collapse collapse",
            role  = "tabpanel",
            div(
              class = "panel-body",
              
              # Node classification
              div(
                h4(strong(step_label)),
                radioButtons(
                  paste0("defensibility_step_", step_id),
                  label = "Classify Node:",
                  choices  = c("Defensible", "Indefensible"),
                  selected = character(0),
                  inline   = TRUE
                ),
                textInput(
                  paste0("justification_step_", step_id),
                  "",
                  placeholder = "Enter justification...",
                  value = ""
                )
              ),
              
              # Options container (filled just below)
              div(id = paste0("options_defensibility_container_", step_id), style = "margin-top: 10px;")
            )
          )
        )
      )
      
      # Option blocks with robust labels
      opt_keys <- names(step_options[[step_key]])
      if (!is.null(opt_keys) && length(opt_keys) > 0) {
        for (ok in opt_keys) {
          option_label <- get_option_label(step_id, ok)
          insertUI(
            selector = paste0("#options_defensibility_container_", step_id),
            where    = "beforeEnd",
            ui = div(
              h5(strong(option_label)),
              radioButtons(
                paste0("defensibility_option_", step_id, "_", ok),
                label   = "Classify Option:",
                choices = c("Defensible", "Indefensible"),
                selected = character(0),
                inline = TRUE
              ),
              textInput(
                paste0("justification_option_", step_id, "_", ok),
                "",
                value = "",
                placeholder = "Enter justification..."
              )
            )
          )
        }
      }
    }
    
    runjs(sprintf(
      "$('#heading_step1b_%s .panel-title a').text(%s);",
      step_id,
      jsonlite::toJSON(step_label, auto_unbox = TRUE)
    ))
    
    # After the DOM binds, force a final pass to make sure headings show the labels
    session$onFlushed(function() {
      for (sid in ids) {
        lbl <- get_step_label(sid)
        runjs(sprintf("$('#heading_step1b_%s .panel-title a').text(%s);",
                      sid,
                      jsonlite::toJSON(lbl, auto_unbox = TRUE)))
      }
    }, once = TRUE)
  })
  
  # --- Compute defensible sets (uses REAL keys) ---
  defensible_steps_reactive <- reactiveVal(list())
  defensible_options_reactive <- reactiveVal(list())
  
  observe({
    defensible_steps <- list()
    defensible_options <- list()
    
    for (step_id in step_ids()) {
      step_key <- as.character(step_id)
      step_name_input <- input[[paste0("step_name_", step_id)]]
      step_def_val     <- input[[paste0("defensibility_step_", step_id)]]
      
      if (!is.null(step_name_input) && nzchar(step_name_input) &&
          !is.null(step_def_val)   && step_def_val == "Defensible") {
        
        defensible_steps[[step_key]] <- step_name_input
        
        step_opts <- list()
        opt_keys <- names(step_options[[step_key]])
        if (!is.null(opt_keys) && length(opt_keys) > 0) {
          for (ok in opt_keys) {
            opt_name <- input[[paste0("option_", step_id, "_", ok)]]
            opt_def  <- input[[paste0("defensibility_option_", step_id, "_", ok)]]
            if (!is.null(opt_name) && nzchar(opt_name) &&
                !is.null(opt_def)  && opt_def == "Defensible") {
              step_opts[[ok]] <- opt_name
            }
          }
        }
        defensible_options[[step_key]] <- step_opts
      }
    }
    
    defensible_steps_reactive(defensible_steps)
    defensible_options_reactive(defensible_options)
  })
  
  # --- Color the network nodes in 1b (use REAL keys) ---
  observe({
    lapply(step_ids(), function(step_id) {
      step_key <- as.character(step_id)
      
      # Step node color
      observeEvent(input[[paste0("defensibility_step_", step_id)]], {
        sel <- input[[paste0("defensibility_step_", step_id)]]
        new_color <- if (identical(sel, "Defensible")) "#5cb85c" else "#d9534f"
        graph_data_1b$nodes$color[graph_data_1b$nodes$id == step_key] <- new_color
        
        # Auto-propagate indefensible to all options of this node
        if (!is.null(step_options[[step_key]])) {
          opt_keys <- names(step_options[[step_key]])
          if (identical(sel, "Indefensible")) {
            for (option_id in opt_keys) {
              updateRadioButtons(
                session,
                inputId  = paste0("defensibility_option_", step_id, "_", option_id),
                selected = "Indefensible"
              )
              runjs(sprintf(
                "$('input[name=\"%s\"]').prop('disabled', true);",
                paste0("defensibility_option_", step_id, "_", option_id)
              ))
            }
          } else if (identical(sel, "Defensible")) {
            for (option_id in opt_keys) {
              runjs(sprintf(
                "$('input[name=\"%s\"]').prop('disabled', false);",
                paste0("defensibility_option_", step_id, "_", option_id)
              ))
            }
          }
        }
        
        output$pipeline_network_step1b <- renderVisNetwork({
          visNetwork(graph_data_1b$nodes, graph_data_1b$edges) %>%
            visEdges(arrows = "to", color = "#000000") %>%
            visNodes(color = list(background = graph_data_1b$nodes$color, border = "#000000")) %>%
            visLayout(hierarchical = TRUE) %>%
            visPhysics(enabled = FALSE) %>%
            visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE,
                           selectable = FALSE, multiselect = FALSE, hover = FALSE,
                           keyboard = FALSE, navigationButtons = FALSE)
        })
      }, ignoreInit = TRUE)
      
      # Option node colors
      opt_keys <- names(step_options[[step_key]])
      if (!is.null(opt_keys) && length(opt_keys) > 0) {
        lapply(opt_keys, function(ok) {
          observeEvent(input[[paste0("defensibility_option_", step_id, "_", ok)]], {
            sel <- input[[paste0("defensibility_option_", step_id, "_", ok)]]
            new_color <- if (identical(sel, "Defensible")) "#5cb85c" else "#d9534f"
            option_node_id <- paste0(step_key, "_", ok)  # matches Step 1a graph ids
            graph_data_1b$nodes$color[graph_data_1b$nodes$id == option_node_id] <- new_color
            
            output$pipeline_network_step1b <- renderVisNetwork({
              visNetwork(graph_data_1b$nodes, graph_data_1b$edges) %>%
                visEdges(arrows = "to", color = "#000000") %>%
                visNodes(color = list(background = graph_data_1b$nodes$color, border = "#000000")) %>%
                visLayout(hierarchical = TRUE) %>%
                visPhysics(enabled = FALSE) %>%
                visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE,
                               selectable = FALSE, multiselect = FALSE, hover = FALSE,
                               keyboard = FALSE, navigationButtons = FALSE)
            })
          }, ignoreInit = TRUE)
        })
      }
    })
  })
  
  # Save Defensibility and Justifications Button
  observeEvent(input$save_step1b_all, {
    ids <- step_ids()
    if (length(ids) == 0) {
      showNotification("⚠️ Nothing to save yet. First add nodes in Step 1a and click “Save Nodes and Options”.", type = "error")
      return()
    }
    
    # Is there ANY selection/entry made in 1b?
    has_any <- FALSE
    for (step_id in ids) {
      # node radio or justification?
      if (!is.null(input[[paste0("defensibility_step_", step_id)]]) ||
          nzchar(input[[paste0("justification_step_", step_id)]] %||% "")) {
        has_any <- TRUE
        break
      }
      # any option radio or justification?
      ok <- names(step_options[[as.character(step_id)]])
      if (length(ok)) {
        for (k in ok) {
          if (!is.null(input[[paste0("defensibility_option_", step_id, "_", k)]]) ||
              nzchar(input[[paste0("justification_option_", step_id, "_", k)]] %||% "")) {
            has_any <- TRUE
            break
          }
        }
      }
      if (has_any) break
    }
    
    if (!has_any) {
      showNotification("⚠️ No classifications or justifications entered yet.", type = "error")
      return()
    }
    
    # Go through all steps and options
    for (step_id in step_ids()) {
      step_key <- as.character(step_id)
      
      # Save step defensibility
      def_key <- paste0("defensibility_step_", step_id)
      def_val <- input[[def_key]]
      if (!is.null(def_val)) {
        stored_inputs$step_defensibility[[def_key]] <- def_val
      }
      
      # Save step justification
      just_key <- paste0("justification_step_", step_id)
      just_val <- input[[just_key]]
      if (!is.null(just_val)) {
        stored_inputs$step_justifications[[just_key]] <- just_val
      }
      
      # Save options
      opt_keys <- names(step_options[[step_key]])
      if (!is.null(opt_keys) && length(opt_keys) > 0) {
        for (ok in opt_keys) {
          opt_def_key  <- paste0("defensibility_option_", step_id, "_", ok)
          opt_def_val  <- input[[opt_def_key]]
          if (!is.null(opt_def_val)) {
            stored_inputs$option_defensibility[[opt_def_key]] <- opt_def_val
          }
          
          opt_just_key <- paste0("justification_option_", step_id, "_", ok)
          opt_just_val <- input[[opt_just_key]]
          if (!is.null(opt_just_val)) {
            stored_inputs$option_justifications[[opt_just_key]] <- opt_just_val
          }
        }
      }
    }
    
    # Notify the user
    showNotification("✅ All Step 1b classifications saved.", type = "message", duration = 3)
  })
  
  
  # Download Button for Save Progress Step 1b
  output$export_zip_step1b <- downloadHandler(
    filename = function() {
      nickname <- input$user_nickname
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      if (is.null(nickname) || nickname == "") paste0("mv_step1a1b_", ts, ".zip")
      else paste0(nickname, "_mv_step1a1b_", ts, ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      if (!requireNamespace("zip", quietly = TRUE)) stop("Package 'zip' is required.")
      if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.")
      
      dfs1a <- build_step1a_dfs()
      dfs1b <- build_step1b_dfs()
      
      outdir <- tempfile("mv1ab_")
      dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
      on.exit(try(unlink(outdir, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
      
      rel_files <- c(
        "step1a_scope.csv",
        "step1a_methods.csv",
        "step1a_nodes.csv",
        "step1a_options.csv",
        "step1a_analysis.csv",
        "step1b_nodes.csv",
        "step1b_options.csv",
        "manifest.json"
      )
      
      write_csv_safely(dfs1a$scope,   file.path(outdir, "step1a_scope.csv"))
      write_csv_safely(dfs1a$methods, file.path(outdir, "step1a_methods.csv"))
      write_csv_safely(dfs1a$nodes,   file.path(outdir, "step1a_nodes.csv"))
      write_csv_safely(dfs1a$options, file.path(outdir, "step1a_options.csv"))
      write_csv_safely(dfs1a$analysis, file.path(outdir, "step1a_analysis.csv"))
      write_csv_safely(dfs1b$nodes,   file.path(outdir, "step1b_nodes.csv"))
      write_csv_safely(dfs1b$options, file.path(outdir, "step1b_options.csv"))
      
      write_manifest_json(outdir, exported_step = "1a-1b", csvs = rel_files[1:7])
      
      old_wd <- getwd(); setwd(outdir)
      on.exit(try(setwd(old_wd), silent = TRUE), add = TRUE)
      zip::zipr(zipfile = file, files = rel_files, include_directories = FALSE)
      if (!file.exists(file) || isTRUE(file.size(file) == 0)) stop("ZIP creation failed.")
    }
  )
  
  # Initial static render
  output$pipeline_network_step1b <- renderVisNetwork({
    visNetwork(graph_data_1b$nodes, graph_data_1b$edges) %>%
      visEdges(arrows = "to", color = "#000000") %>%
      visLayout(hierarchical = TRUE) %>%
      visPhysics(enabled = FALSE) %>%
      visInteraction(
        dragNodes = FALSE, dragView = FALSE, zoomView = FALSE,
        selectable = FALSE, multiselect = FALSE, hover = FALSE,
        keyboard = FALSE, navigationButtons = FALSE
      )
  })
  
##############################################################
  
  
############ Step 1c: Create Defensible Pipelines ############
  
  # Show modal when a keyboard refresh is attempted
  observeEvent(input$refresh_attempt, {
    showModal(modalDialog(
      title = "Refresh will discard your progress",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_refresh", "Continue & Refresh", class = "btn-danger")
      ),
      tagList(
        p("If you didn't complete Multiverse 1.0 and export your ",
          strong("Preprocessing documentation"),
          " and ",
          strong("Construction documentation"),
          ", your progress will be lost."),
        p("Do you want to refresh the page now?")
      ),
      size = "m"
    ))
  })
  
  # If user confirms, trigger a hard reload on the client
  observeEvent(input$confirm_refresh, {
    removeModal()
    session$sendCustomMessage("doHardRefresh", TRUE)
  })
  
  # Keep track of which mode was chosen (optional, but handy)
  start_mode_choice_1c <- reactiveVal(NULL)
  
  # Warn when the user turns on permutations (Advanced)
  observeEvent(input$permute_nodes_1c, {
    if (isTRUE(input$permute_nodes_1c)) {
      showModal(modalDialog(
        title = "Heads up — this can explode in size",
        easyClose = TRUE,
        footer = modalButton("OK"),
        tagList(
          p("Including all node permutations creates many more pipelines."),
          tags$ul(
            tags$li("Depending on your nodes/options, calculating the Cartesian product can take a long time."),
            tags$li("Visual network exports are disabled for multiverses larger than ", tags$b("50 pipelines"), ".")
          )
        )
      ))
    }
  })
  
  observeEvent(input$start_manual_1c, {
    start_mode_choice_1c("Empty slate")
    
    shinyjs::show("work_area_1c")
    # Hide AUTO controls
    shinyjs::hide("auto_mode_controls")
    shinyjs::hide("auto_pipeline_selector_container")
    # Hide Advanced checkbox (we only want it in Auto mode)
    shinyjs::hide("permute_nodes_1c")
    shinyjs::hide("auto_view_panel")
    # Show manual-only button
    shinyjs::show("new_pipeline")
    shinyjs::show("manual_left")
    shinyjs::hide("auto_left")
    
    # Clear any pipeline UI/content (fresh start)
    runjs("$('#main_pipeline_accordion').empty();")
    
    # Reset manual state
    pipelines(list())
    saved_pipelines$data <- list()
    active_pipeline(NULL)
    clicked_selection(character(0))
    
    # Reset auto state (correct API for pickerInput)
    auto_generated_pipelines(list())
    shinyWidgets::updatePickerInput(session, "auto_pipeline_picker",
                                    choices = list(), selected = NULL
    )
    shinyjs::hide("auto_pipeline_selector_container")
    
    # Hide auto view/buttons
    shinyjs::hide("auto_left")
    shinyjs::hide("auto_actions_row")
  })
  
  observeEvent(input$start_auto_1c, {
    start_mode_choice_1c("Cartesian product")
    
    shinyjs::show("work_area_1c")
    shinyjs::show("auto_mode_controls")
    shinyjs::show("permute_nodes_1c")
    shinyjs::hide("new_pipeline")  # no manual “New Pipeline” in auto mode
    shinyjs::hide("auto_view_panel")
    shinyjs::hide("manual_left")
    shinyjs::hide("auto_left")
    shinyjs::hide("auto_actions_row")
    
    # Clear any visible manual pipelines
    runjs("$('#main_pipeline_accordion').empty();")
    
    # Reset stores
    pipelines(list())
    saved_pipelines$data <- list()
    active_pipeline(NULL)
    clicked_selection(character(0))
    
    auto_generated_pipelines(list())
    shinyWidgets::updatePickerInput(session, "auto_pipeline_picker",
                                    choices = list(), selected = NULL
    )
    shinyjs::hide("auto_pipeline_selector_container")
  })
  
  #----- Manual / Empty Slate --------
  # Reactive values to store copied selections and working pipelines
  copied_selection <- reactiveVal(NULL)
  pipelines <- reactiveVal(list())
  saved_pipelines <- reactiveValues(data = list())
  # Stores ALL auto-generated pipelines as a list of character vectors
  auto_generated_pipelines <- reactiveVal(list())
  
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
    
    existing_pipelines[[new_pipeline_id]] <- character(0)
    pipelines(existing_pipelines)  # ✅ Store new pipeline
    
    print("DEBUG: Pipelines() after adding new pipeline ->")
    print(pipelines())
    
    # ✅ Insert pipeline accordion without removing existing ones
    insertUI(
      selector = "#main_pipeline_accordion",
      where = "beforeEnd",
      ui = create_pipeline_accordion(new_pipeline_id)
    )
    
    active_pipeline(new_pipeline_id)
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
  
  # Cartesian Product Generation
  observeEvent(input$generate_pipelines_1c, {
    req(identical(start_mode_choice_1c(), "Cartesian product"))
    
    steps <- defensible_steps_reactive()
    opts  <- defensible_options_reactive()
    
    if (length(steps) == 0) {
      showNotification("No defensible nodes available. Complete Steps 1a/1b first.", type = "warning")
      return()
    }
    
    node_ids_in_order <- names(steps)
    option_sets <- lapply(node_ids_in_order, function(nid) {
      o <- opts[[nid]]
      if (is.null(o) || length(o) == 0) NA_character_ else unname(unlist(o))
    })
    names(option_sets) <- node_ids_in_order
    
    build_for_order <- function(order_ids) {
      prep <- lapply(option_sets[order_ids], function(v) if (all(is.na(v))) "__NOOPT__" else v)
      grid <- do.call(expand.grid, c(prep, stringsAsFactors = FALSE))
      if (nrow(grid) == 0) return(list())
      out <- vector("list", nrow(grid))
      for (r in seq_len(nrow(grid))) {
        seq_labels <- character(0)
        for (k in seq_along(order_ids)) {
          nid <- order_ids[k]
          node_label <- steps[[nid]]
          val <- grid[r, k][[1]]
          seq_labels <- c(seq_labels, node_label)
          if (!identical(val, "__NOOPT__")) seq_labels <- c(seq_labels, paste0("➝ ", val))
        }
        out[[r]] <- seq_labels
      }
      out
    }
    
    pipelines_to_add <- if (isTRUE(input$permute_nodes_1c)) {
      perms <- gtools::permutations(length(node_ids_in_order), length(node_ids_in_order), node_ids_in_order)
      # build list for each permutation row
      as.list(apply(perms, 1, function(row) build_for_order(as.character(row)))) |>
        unlist(recursive = FALSE)
    } else {
      build_for_order(node_ids_in_order)
    }
    
    if (length(pipelines_to_add) == 0) {
      showNotification("No pipelines could be generated.", type = "warning")
      return()
    }
    
    # Name once and store
    named <- setNames(pipelines_to_add, paste0("Pipeline ", seq_along(pipelines_to_add)))
    auto_generated_pipelines(named)
    
    # ---- Build bulk groups ----
    ap <- auto_generated_pipelines()
    
    # first node per pipeline (first non-option label)
    first_node <- vapply(ap, function(seq) {
      nodes <- seq[!grepl("^\\s*➝\\s*", seq)]
      if (length(nodes)) nodes[1] else NA_character_
    }, character(1))
    
    fn_groups <- split(names(ap), first_node[!is.na(first_node)])
    
    bulk_first <- setNames(
      paste0("grp:first:", names(fn_groups)),
      paste0("All with first node: ", names(fn_groups))
    )
    
    # node-pair (adjacent node order) per pipeline
    pair_index <- list()
    for (nm in names(ap)) {
      seq <- ap[[nm]]
      nodes <- gsub("^\\s*➝\\s*", "", seq[!grepl("^\\s*➝\\s*", seq)])
      if (length(nodes) >= 2) {
        pairs <- paste(head(nodes, -1), tail(nodes, -1), sep = " > ")
        for (p in unique(pairs)) pair_index[[p]] <- c(pair_index[[p]], nm)
      }
    }
    if (length(pair_index) > 0) {
      pair_keys  <- sort(names(pair_index))
      bulk_pairs <- setNames(paste0("grp:pair:", pair_keys),
                             paste0("All with node pair: ", pair_keys))
    } else {
      bulk_pairs <- character(0)
    }
    
    choices_list <- list(
      "Bulk: First node" = bulk_first,
      "Bulk: Node pair"  = bulk_pairs,
      "Pipelines"        = setNames(names(ap), names(ap))
    )
    
    # show the selector with groups + the action buttons
    shinyjs::show("auto_pipeline_selector_container")
    shinyjs::show("auto_left")
    shinyjs::show("auto_actions_row")   # <-- this was missing
    
    shinyWidgets::updatePickerInput(
      session, "auto_pipeline_picker",
      choices  = choices_list,
      selected = names(ap)[1]
    )
    
    # Clear any manual UI
    runjs("$('#main_pipeline_accordion').empty();")
    
    showNotification(sprintf(
      "Generated %d pipeline(s). Use the grouped picker to select and prune.",
      length(ap)
    ), type = "message")
  })
  
  # Display a Single Pipeline From Dropdown in Auto Generation
  observeEvent(input$auto_pipeline_picker, {
    req(identical(start_mode_choice_1c(), "Cartesian product"))
    ap <- auto_generated_pipelines(); req(length(ap) > 0)
    
    sel <- input$auto_pipeline_picker
    req(length(sel) >= 1)
    
    # ignore virtual group tokens for viewing
    real <- sel[!grepl("^grp:", sel)]
    if (!length(real)) return()   # user picked only bulk tokens so far
    
    pid <- tail(real, 1)          # last real choice
    req(pid %in% names(ap))
    
    seq_labels <- ap[[pid]]
    
    # TABLE
    df <- data.frame(
      Type  = ifelse(grepl("^\\s*➝\\s*", seq_labels), "Option", "Node"),
      Label = gsub("^\\s*➝\\s*", "", seq_labels),
      stringsAsFactors = FALSE
    )
    output$auto_pipeline_title <- renderText(pid)
    output$auto_pipeline_table <- renderTable(
      df, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s"
    )
    shinyjs::show("auto_left")
    shinyjs::show("auto_actions_row")
    
    # NETWORK
    view_id <- "pipeline_auto_view"
    pipelines(setNames(list(seq_labels), view_id))
    active_pipeline(view_id)
    
    runjs("$('#main_pipeline_accordion').empty();")
  })
  
  # Delete Selected Pipeline Auto Mode
  observeEvent(input$delete_auto_pipeline, {
    req(identical(start_mode_choice_1c(), "Cartesian product"))
    ap <- auto_generated_pipelines(); req(length(ap) > 0)
    
    sel <- input$auto_pipeline_picker
    if (is.null(sel) || !length(sel)) {
      showNotification("Select one or more pipelines (or a bulk selector) to delete.", type = "warning")
      return()
    }
    
    # expand virtual tokens into concrete pipeline names
    to_remove <- character(0)
    
    # 1) direct pipeline names
    to_remove <- c(to_remove, sel[sel %in% names(ap)])
    
    # 2) bulk: first node
    first_map <- vapply(ap, function(seq) {
      nodes <- seq[!grepl("^\\s*➝\\s*", seq)]
      if (length(nodes)) nodes[1] else NA_character_
    }, character(1))
    for (tok in sel[grepl("^grp:first:", sel)]) {
      key <- sub("^grp:first:", "", tok, perl = TRUE)
      to_remove <- c(to_remove, names(ap)[first_map == key])
    }
    
    # 3) bulk: node pair
    node_seq <- lapply(ap, function(seq) gsub("^\\s*➝\\s*", "", seq[!grepl("^\\s*➝\\s*", seq)]))
    has_pair <- function(v, pair) {
      parts <- strsplit(pair, " > ", fixed = TRUE)[[1]]
      if (length(parts) != 2) return(FALSE)
      if (length(v) < 2) return(FALSE)
      any(paste(head(v, -1), tail(v, -1), sep = " > ") == pair)
    }
    for (tok in sel[grepl("^grp:pair:", sel)]) {
      key <- sub("^grp:pair:", "", tok, perl = TRUE)
      hits <- names(ap)[vapply(node_seq, has_pair, logical(1), pair = key)]
      to_remove <- c(to_remove, hits)
    }
    
    to_remove <- unique(to_remove)
    if (!length(to_remove)) {
      showNotification("Nothing matched that selection.", type = "message")
      return()
    }
    
    ap[to_remove] <- NULL
    auto_generated_pipelines(ap)
    
    # refresh picker
    if (length(ap) == 0) {
      shinyWidgets::updatePickerInput(
        session, "auto_pipeline_picker",
        choices = list(), selected = NULL
      )
      shinyjs::hide("auto_left")
      shinyjs::hide("auto_actions_row")
      shinyjs::hide("auto_pipeline_selector_container")
      pipelines(list()); active_pipeline(NULL)
      runjs("$('#main_pipeline_accordion').empty();")
      showNotification("Deleted selected pipelines. No pipelines remain.", type = "message")
      return()
    }
    
    ## rebuild groups (same logic as after generation)
    first_node <- vapply(ap, function(seq) {
      nodes <- seq[!grepl("^\\s*➝\\s*", seq)]
      if (length(nodes)) nodes[1] else NA_character_
    }, character(1))
    fn_groups <- split(names(ap), first_node[!is.na(first_node)])
    bulk_first <- setNames(paste0("grp:first:", names(fn_groups)),
                           paste0("All with first node: ", names(fn_groups)))
    
    pair_index <- list()
    for (nm in names(ap)) {
      seq <- ap[[nm]]
      nodes <- gsub("^\\s*➝\\s*", "", seq[!grepl("^\\s*➝\\s*", seq)])
      if (length(nodes) >= 2) {
        pairs <- paste(head(nodes, -1), tail(nodes, -1), sep = " > ")
        for (p in unique(pairs)) pair_index[[p]] <- c(pair_index[[p]], nm)
      }
    }
    if (length(pair_index) > 0) {
      pair_keys  <- sort(names(pair_index))
      bulk_pairs <- setNames(paste0("grp:pair:", pair_keys),
                             paste0("All with node pair: ", pair_keys))
    } else {
      bulk_pairs <- character(0)
    }
    
    choices_list <- list(
      "Bulk: First node" = bulk_first,
      "Bulk: Node pair"  = bulk_pairs,
      "Pipelines"        = setNames(names(ap), names(ap))
    )
    
    # select the first remaining real pipeline to keep the view live
    first_real <- names(ap)[1]
    shinyWidgets::updatePickerInput(
      session, "auto_pipeline_picker",
      choices  = choices_list,
      selected = first_real
    )
    
    # render that one immediately
    seq_labels <- ap[[first_real]]
    df <- data.frame(
      Type  = ifelse(grepl("^\\s*➝\\s*", seq_labels), "Option", "Node"),
      Label = gsub("^\\s*➝\\s*", "", seq_labels),
      stringsAsFactors = FALSE
    )
    output$auto_pipeline_title <- renderText(first_real)
    output$auto_pipeline_table <- renderTable(df, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s")
    shinyjs::show("auto_left")
    shinyjs::show("auto_actions_row")
    
    view_id <- "pipeline_auto_view"
    pipelines(setNames(list(seq_labels), view_id))
    active_pipeline(view_id)
    
    showNotification(sprintf("Deleted %d pipeline(s).", length(to_remove)), type = "message")
  })
  
  # Saving Visual Network Graphs Auto Mode
  build_graph_from_sequence <- function(seq_labels, pid) {
    steps  <- defensible_steps_reactive()
    opts   <- defensible_options_reactive()
    
    nodes <- data.frame(id=character(), label=character(), color=character(),
                        shape=character(), level=numeric(), stringsAsFactors=FALSE)
    edges <- data.frame(from=character(), to=character(), stringsAsFactors=FALSE)
    
    if (length(seq_labels) == 0) return(list(nodes=nodes, edges=edges))
    
    prev_selected_option <- NULL
    level_counter <- 1
    cleaned_for_match <- gsub("^\\s*➝\\s*", "", seq_labels)
    
    for (i in seq_along(seq_labels)) {
      step_label <- seq_labels[i]
      step_id <- names(steps)[steps == step_label]
      if (length(step_id) == 0) next
      
      step_name <- steps[[step_id]]
      options   <- opts[[step_id]]
      
      nodes <- rbind(nodes, data.frame(
        id    = paste0(pid, "_", step_id),
        label = step_name,
        color = "#5cb85c",
        shape = "circle",
        level = level_counter
      ))
      
      option_selected <- NULL
      if (!is.null(options)) {
        option_level <- level_counter + 1
        for (option_id in names(options)) {
          option_name    <- options[[option_id]]
          option_node_id <- paste0(pid, "_", step_id, "_", option_id)
          option_color   <- ifelse(option_name %in% cleaned_for_match, "#5cb85c", "#d3d3d3")
          
          nodes <- rbind(nodes, data.frame(
            id    = option_node_id,
            label = option_name,
            color = option_color,
            shape = "box",
            level = option_level
          ))
          edges <- rbind(edges, data.frame(from = paste0(pid, "_", step_id), to = option_node_id))
          if (option_color == "#5cb85c") option_selected <- option_node_id
        }
      }
      
      if (!is.null(prev_selected_option)) {
        edges <- rbind(edges, data.frame(from = prev_selected_option, to = paste0(pid, "_", step_id)))
      }
      prev_selected_option <- if (!is.null(option_selected)) option_selected else paste0(pid, "_", step_id)
      level_counter <- if (!is.null(option_selected)) option_level + 1 else level_counter + 2
    }
    
    list(nodes = nodes, edges = edges)
  }
  
  # Save All Pipelines Auto Mode
  observeEvent(input$save_auto_pipelines, {
    req(identical(start_mode_choice_1c(), "Cartesian product"))
    ap <- auto_generated_pipelines()
    if (length(ap) == 0) {
      showNotification("There are no auto pipelines to save.", type = "warning")
      return()
    }
    
    # Save each auto pipeline as its own entry (like manual), avoiding collisions
    saved <- saved_pipelines$data
    # remove any previous auto_ pipelines to avoid stale leftovers
    auto_keys <- grep("^auto_pipeline_", names(saved), value = TRUE)
    if (length(auto_keys)) saved[auto_keys] <- NULL
    
    for (i in seq_along(ap)) {
      key <- sprintf("auto_pipeline_%04d", i)
      saved[[key]] <- ap[[i]]  # each is a character vector of labels
      saved[[paste0("graph_", key)]] <- build_graph_from_sequence(ap[[i]], key)
    }
    saved_pipelines$data <- saved
    
    showNotification(paste("Saved", length(ap), "auto-generated pipeline(s)."), type = "message")
  })
  
  # Reactive value to store pipeline graph data
  pipeline_graph_data <- reactiveVal(list(nodes = data.frame(), edges = data.frame()))
  
  # Update the network when steps/options are added to the pipeline
  observe({
    pid <- active_pipeline()
    req(pid)
    
    nodes <- data.frame(id=character(), label=character(), color=character(),
                        shape=character(), level=numeric(), stringsAsFactors=FALSE)
    edges <- data.frame(from=character(), to=character(), stringsAsFactors=FALSE)
    
    # 1) manual mode: rank-list input exists
    # 2) auto mode: no rank-list, so use pipelines()[[pid]]
    selected_items <- input[[paste0("selected_steps_", pid)]] %||% pipelines()[[pid]]
    
    if (!is.null(selected_items) && length(selected_items) > 0) {
      prev_selected_option <- NULL
      level_counter <- 1
      
      for (i in seq_along(selected_items)) {
        step_label <- selected_items[i]
        step_id <- names(defensible_steps_reactive())[defensible_steps_reactive() == step_label]
        if (length(step_id) == 0) next
        
        step_name <- defensible_steps_reactive()[[step_id]]
        options   <- defensible_options_reactive()[[step_id]]
        
        nodes <- rbind(nodes, data.frame(
          id    = paste0(pid, "_", step_id),
          label = step_name,
          color = "#5cb85c",
          shape = "circle",
          level = level_counter
        ))
        
        option_selected <- NULL
        if (!is.null(options)) {
          option_level <- level_counter + 1
          for (option_id in names(options)) {
            option_name    <- options[[option_id]]
            option_node_id <- paste0(pid, "_", step_id, "_", option_id)
            
            cleaned_selected_items <- gsub("^\\s*➝\\s*", "", selected_items)
            option_color <- ifelse(option_name %in% cleaned_selected_items, "#5cb85c", "#d3d3d3")
            
            nodes <- rbind(nodes, data.frame(
              id    = option_node_id,
              label = option_name,
              color = option_color,
              shape = "box",
              level = option_level
            ))
            
            edges <- rbind(edges, data.frame(from = paste0(pid, "_", step_id), to = option_node_id))
            if (option_color == "#5cb85c") option_selected <- option_node_id
          }
        }
        
        if (!is.null(prev_selected_option)) {
          edges <- rbind(edges, data.frame(from = prev_selected_option, to = paste0(pid, "_", step_id)))
        }
        
        if (is.null(options) || length(options) == 0) {
          if (i < length(selected_items)) {
            next_step_id <- paste0(pid, "_", selected_items[i + 1])
            edges <- rbind(edges, data.frame(from = paste0(pid, "_", step_id), to = next_step_id))
          }
        }
        
        prev_selected_option <- if (!is.null(option_selected)) option_selected else paste0(pid, "_", step_id)
        level_counter <- if (!is.null(option_selected)) option_level + 1 else level_counter + 2
      }
    }
    
    pipeline_graph_data(list(nodes = nodes, edges = edges))
  })
  
  # Render the visual network
  output$pipeline_network_1c <- renderVisNetwork({
    req(active_pipeline())
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
  
  # Download Button Save Progress Step 1d
  output$export_zip_step1d <- downloadHandler(
    filename = function() {
      nickname <- input$user_nickname
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      if (is.null(nickname) || nickname == "") paste0("mv_step1a1b1c_", ts, ".zip")
      else paste0(nickname, "_mv_step1a1b1c_", ts, ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      if (!requireNamespace("zip", quietly = TRUE)) stop("Package 'zip' is required.")
      if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.")
      
      dfs1a <- build_step1a_dfs()
      dfs1b <- build_step1b_dfs()
      dfs1c <- build_step1c_dfs()
      
      outdir <- tempfile("mv1abc_")
      dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
      on.exit(try(unlink(outdir, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
      
      rel_files <- c(
        "step1a_scope.csv",
        "step1a_methods.csv",
        "step1a_nodes.csv",
        "step1a_options.csv",
        "step1a_analysis.csv",
        "step1b_nodes.csv",
        "step1b_options.csv",
        "step1c_pipelines.csv",
        "step1c_graph_nodes.csv",
        "step1c_graph_edges.csv",
        "manifest.json"
      )
      
      write_csv_safely(dfs1a$scope,   file.path(outdir, "step1a_scope.csv"))
      write_csv_safely(dfs1a$methods, file.path(outdir, "step1a_methods.csv"))
      write_csv_safely(dfs1a$nodes,   file.path(outdir, "step1a_nodes.csv"))
      write_csv_safely(dfs1a$options, file.path(outdir, "step1a_options.csv"))
      write_csv_safely(dfs1a$analysis,    file.path(outdir, "step1a_analysis.csv"))
      write_csv_safely(dfs1b$nodes,   file.path(outdir, "step1b_nodes.csv"))
      write_csv_safely(dfs1b$options, file.path(outdir, "step1b_options.csv"))
      write_csv_safely(dfs1c$pipelines,   file.path(outdir, "step1c_pipelines.csv"))
      write_csv_safely(dfs1c$graph_nodes, file.path(outdir, "step1c_graph_nodes.csv"))
      write_csv_safely(dfs1c$graph_edges, file.path(outdir, "step1c_graph_edges.csv"))
      
      # NOTE: include all 1a+1b+1c CSVs in the manifest
      write_manifest_json(outdir, exported_step = "1a-1d", csvs = rel_files[1:10])
      
      old_wd <- getwd(); setwd(outdir)
      on.exit(try(setwd(old_wd), silent = TRUE), add = TRUE)
      zip::zipr(zipfile = file, files = rel_files, include_directories = FALSE)
      if (!file.exists(file) || isTRUE(file.size(file) == 0)) stop("ZIP creation failed.")
    }
  )
  
  # ---- Helper: build option-usage frequency table across all saved pipelines ----
  build_option_frequency_df <- function() {
    pipe_ids <- names(saved_pipelines$data)
    pipe_ids <- pipe_ids[!grepl("^graph_", pipe_ids)]
    if (!length(pipe_ids)) return(NULL)
    
    is_option    <- function(x) grepl("^(\\s*➝|\\s*->)\\s*", x)
    strip_option <- function(x) sub("^(\\s*➝|\\s*->)\\s*", "", x)
    
    denom  <- list()  # node -> number of pipelines that include that node at least once
    counts <- list()  # "node\roption" -> how many pipelines selected that option under that node
    
    for (pid in pipe_ids) {
      seq_labels <- saved_pipelines$data[[pid]] %||% character(0)
      if (!length(seq_labels)) next
      seq_labels <- enc2utf8(seq_labels)
      
      node_idx <- which(!is_option(seq_labels))
      if (!length(node_idx)) next
      
      nodes_in_pipeline <- unique(seq_labels[node_idx])
      for (nd in nodes_in_pipeline) denom[[nd]] <- (denom[[nd]] %||% 0) + 1
      
      for (i in node_idx) {
        node_label <- seq_labels[i]
        j <- i + 1
        while (j <= length(seq_labels) && is_option(seq_labels[j])) {
          opt_label <- strip_option(seq_labels[j])
          key <- paste0(node_label, "\r", opt_label)
          counts[[key]] <- (counts[[key]] %||% 0) + 1
          j <- j + 1
        }
      }
    }
    
    if (!length(counts)) {
      return(data.frame(
        Node = character(0), Option = character(0), Count = integer(0),
        Pipelines_with_node = integer(0), Percent = numeric(0),
        stringsAsFactors = FALSE
      ))
    }
    
    Node <- Option <- character(0)
    Count <- Denominator <- integer(0)
    for (k in names(counts)) {
      parts <- strsplit(k, "\r", fixed = TRUE)[[1]]
      node  <- parts[1]; opt <- parts[2]
      cval  <- counts[[k]]
      dval  <- denom[[node]] %||% 0
      Node  <- c(Node, node)
      Option<- c(Option, opt)
      Count <- c(Count, cval)
      Denominator <- c(Denominator, dval)
    }
    
    out <- data.frame(
      Node = Node,
      Option = Option,
      Count = Count,
      Pipelines_with_node = Denominator,
      Percent = ifelse(Denominator > 0, round(100 * Count / Denominator, 1), NA_real_),
      stringsAsFactors = FALSE
    )
    out[order(out$Node, -out$Count, out$Option), , drop = FALSE]
  }
  
  # ---- Step 1d: Option usage frequency across pipelines ----
  output$option_frequency_table <- DT::renderDataTable({
    # Collect pipeline ids (exclude graph entries)
    pipe_ids <- names(saved_pipelines$data)
    pipe_ids <- pipe_ids[!grepl("^graph_", pipe_ids)]
    req(length(pipe_ids) > 0)
    
    is_option <- function(x) grepl("^(\\s*➝|\\s*->)\\s*", x)
    strip_option <- function(x) sub("^(\\s*➝|\\s*->)\\s*", "", x)
    
    # denominators: number of pipelines that include each node at least once
    denom <- list()
    # counts: (node, option) -> frequency across pipelines
    counts <- list()
    
    for (pid in pipe_ids) {
      seq_labels <- saved_pipelines$data[[pid]] %||% character(0)
      if (!length(seq_labels)) next
      seq_labels <- enc2utf8(seq_labels)
      
      # mark indices of nodes (anything not starting with an option arrow)
      node_idx <- which(!is_option(seq_labels))
      if (!length(node_idx)) next
      
      # increment denominator once per node per pipeline
      nodes_in_pipeline <- unique(seq_labels[node_idx])
      for (nd in nodes_in_pipeline) {
        denom[[nd]] <- (denom[[nd]] %||% 0) + 1
      }
      
      # for each node position, count the immediately following option(s) until next node
      for (i in node_idx) {
        node_label <- seq_labels[i]
        j <- i + 1
        while (j <= length(seq_labels) && is_option(seq_labels[j])) {
          opt_label <- strip_option(seq_labels[j])
          key <- paste0(node_label, "\r", opt_label)  # composite key
          counts[[key]] <- (counts[[key]] %||% 0) + 1
          j <- j + 1
        }
      }
    }
    
    if (!length(counts)) {
      return(DT::datatable(
        data.frame(Message = "No option selections found in the current pipelines."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    
    # Build a tidy table
    Node <- character(0)
    Option <- character(0)
    Count <- integer(0)
    Denominator <- integer(0)
    
    for (k in names(counts)) {
      parts <- strsplit(k, "\r", fixed = TRUE)[[1]]
      node  <- parts[1]
      opt   <- parts[2]
      cval  <- counts[[k]]
      dval  <- denom[[node]] %||% 0
      Node  <- c(Node, node)
      Option <- c(Option, opt)
      Count <- c(Count, cval)
      Denominator <- c(Denominator, dval)
    }
    
    out <- data.frame(
      Node = Node,
      Option = Option,
      Count = Count,
      Pipelines_with_node = Denominator,
      Percent = ifelse(Denominator > 0, round(100 * Count / Denominator, 1), NA_real_),
      stringsAsFactors = FALSE
    )
    
    # Order by Node then descending Count
    out <- out[order(out$Node, -out$Count, out$Option), ]
    
    DT::datatable(
      out,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 50,
        ordering = TRUE,
        autoWidth = TRUE
      ),
      class = "display nowrap"
    )
  })
  
  # ✅ Download Preprocessing Documentation 
  output$download_preprocessing_doc <- downloadHandler(
    filename = function() {
      nickname <- input$user_nickname
      timeStamp <- format(Sys.time(), "%d-%m-%Y_%H-%M-%OS3")
      if (is.null(nickname) || nickname == "") {
        paste0("Preprocessing_Documentation_", timeStamp, ".zip")
      } else {
        paste0(nickname, "_Preprocessing_Documentation_", timeStamp, ".zip")
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
      timeStamp <- format(Sys.time(), "%d-%m-%Y_%H-%M-%OS3")
      correct_filename <- if (is.null(nickname) || nickname == "") {
        paste0("Preprocessing_Documentation_", timeStamp, ".zip")
      } else {
        paste0(nickname, "_Preprocessing_Documentation_", timeStamp, ".zip")
      }
      
      tempDir <- file.path(tempdir(), "preprocessing_doc")
      dir.create(tempDir, showWarnings = FALSE)
      tempReport <- file.path(tempDir, "Preprocessing_Documentation.Rmd")
      pdf_file <- file.path(tempDir, "Preprocessing_Documentation.pdf")
      
      ###
      ## Here was the PDF Scope and Method section
      # Use saved values (or defaults if not saved yet)
      scope_cb <- (saved_step1a$scope$checkboxes) %||% rep(FALSE, 4)
      scope_tx <- (saved_step1a$scope$texts)      %||% rep("",    4)
      
      methods_cb <- (saved_step1a$methods$checkboxes) %||% rep(FALSE, 4)
      methods_tx <- (saved_step1a$methods$texts)      %||% rep("",    4)
      
      # Build text lists
      aspects <- c()
      if (scope_cb[1]) aspects <- c(aspects, paste("* Measurement:",         scope_tx[1]))
      if (scope_cb[2]) aspects <- c(aspects, paste("* Preprocessing:",       scope_tx[2]))
      if (scope_cb[3]) aspects <- c(aspects, paste("* Model Specification:", scope_tx[3]))
      if (scope_cb[4]) aspects <- c(aspects, paste("* Estimation Methods:",  scope_tx[4]))
      if (length(aspects) == 0) aspects <- "No aspects selected."
      aspects_text <- paste(aspects, collapse = "\n")
      
      methods <- c()
      if (methods_cb[1]) methods <- c(methods, paste("* Literature Review:", methods_tx[1]))
      if (methods_cb[2]) methods <- c(methods, paste("* Expertise:",         methods_tx[2]))
      if (methods_cb[3]) methods <- c(methods, paste("* Crowdsourcing:",     methods_tx[3]))
      if (methods_cb[4]) methods <- c(methods, paste("* Other methods:",     methods_tx[4]))
      if (length(methods) == 0) methods <- "No identification method for defensibility provided."
      methods_text <- paste(methods, collapse = "\n")
      
      analysis_df <- saved_step1a$analysis
      if (is.null(analysis_df) || !nrow(analysis_df)) {
        analysis_text <- "No analysis plan provided."
      } else {
        # make a clean bullet list, omitting empty rows
        keep <- nzchar(analysis_df$value)
        if (!any(keep)) {
          analysis_text <- "No analysis plan provided."
        } else {
          bullets <- paste0("* ", analysis_df$field[keep], ": ", analysis_df$value[keep])
          analysis_text <- paste(bullets, collapse = "\n")
        }
      }
      
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
        "  analysis_text: ''", 
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
        "## Analysis plan for multiverse results",
        "`r params$analysis_text`",
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
            analysis_text = analysis_text,
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
      
      # --- Decide whether to generate image assets (cap at 50 pipelines)
      pipeline_ids <- names(saved_pipelines$data)
      pipeline_ids <- pipeline_ids[!grepl("^graph_", pipeline_ids)]
      n_pipelines  <- length(pipeline_ids)
      
      generate_assets <- n_pipelines <= 50
      if (!generate_assets) {
        showNotification(
          sprintf("More than 50 pipelines detected (%d). Skipping network/table PNG generation to keep export fast.", n_pipelines),
          type = "warning", duration = 8
        )
      }
      
      # --- Always generate the Option Usage table PNG (single table)
      of <- build_option_frequency_df()
      of_png <- file.path(tempDir, "option_frequency_table.png")
      
      # Make a readable table even if empty
      png(of_png, width = 1600, height = 900, res = 150)  # larger canvas for wide tables
      if (!is.null(of) && nrow(of)) {
        gridExtra::grid.table(
          of,
          theme = gridExtra::ttheme_default(
            core   = list(fg_params = list(cex = 1.2)),
            colhead= list(fg_params = list(cex = 1.3, fontface = "bold"))
          )
        )
      } else {
        grid::grid.newpage()
        grid::grid.text("No option selections found in the current pipelines.",
                        gp = grid::gpar(cex = 1.4))
      }
      dev.off()
      
      # --- Network screenshots (only if <= 50 pipelines)
      if (generate_assets) {
        for (pipeline_name in names(saved_pipelines$data)) {
          graph_key <- paste0("graph_", pipeline_name)
          if (!graph_key %in% names(saved_pipelines$data)) next
          graph_data <- saved_pipelines$data[[graph_key]]
          if (is.null(graph_data) || is.null(graph_data$nodes) || is.null(graph_data$edges)) next
          
          network_png <- file.path(tempDir, paste0(pipeline_name, "_network.png"))
          html_path   <- file.path(tempDir, paste0(pipeline_name, "_network.html"))
          
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
              url  = paste0("file:///", normalizePath(html_path, winslash = "/")),
              file = network_png,
              vwidth = 1600, vheight = 1000, delay = 3
            )
          }
        }
      }
      
      # --- Table images (only if <= 50 pipelines)
      if (generate_assets) {
        for (pipeline_name in names(saved_pipelines$data)) {
          if (grepl('^graph_', pipeline_name)) next
          pipeline_data <- saved_pipelines$data[[pipeline_name]]
          if (!is.null(pipeline_data) && length(pipeline_data) > 0) {
            table_png     <- file.path(tempDir, paste0(pipeline_name, "_table.png"))
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
      }
      
      # --- Build file list for ZIP
      # Always include the PDF; include PNGs only if generated
      if (generate_assets) {
        allowed_files <- list.files(
          tempDir,
          pattern = "Preprocessing_Documentation.pdf|_network.png$|_table.png$",
          full.names = TRUE
        )
      } else {
        allowed_files <- pdf_file
      }
      
      # Always include the option-usage PNG (generated just above)
      of_png <- file.path(tempDir, "option_frequency_table.png")
      allowed_files <- unique(c(allowed_files, of_png))
      
      zip::zipr(zipfile = file, files = allowed_files)
      
      unlink(list.files(tempDir, pattern = "_network.html$", full.names = TRUE))
      unlink(list.files(tempDir, pattern = "_network_files$", full.names = TRUE), recursive = TRUE)
      upload_to_cloud(file, correct_filename)
      
      removeModal()
    }
  )
  
############################################################## 

################################################################################

################################ Multiverse 2.0 ################################
  # ---------- Multiverse 2.0 ----------
  observeEvent(input$btn_prev_2a, {
    updateNavbarPage(session, "main_nav", selected = "mv1")
    shinyjs::delay(0, updateTabsetPanel(session, "mv1_tabs", selected = "1d"))
  })
  observeEvent(input$btn_next_2a, {
    updateTabsetPanel(session, "mv2_tabs", selected = "2b")
  })
  
  observeEvent(input$btn_prev_2b, {
    updateTabsetPanel(session, "mv2_tabs", selected = "2a")
  })
  
  observeEvent(input$btn_next_2b, {
    updateNavbarPage(session, "main_nav", selected = "about")
  })
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
  
  # Inline dynamic text input for Simulation/Both
  output$simulation_details_inline <- renderUI({
    mode <- input$equivalence_assessment_mode
    if (is.null(mode)) return(NULL)
    if (mode %in% c("Simulation", "Both")) {
      # single-line input; stays compact and next to radios
      textInput(
        inputId = "simulation_details_link",
        label = NULL,
        placeholder = "Simulation details and/or link to scripts/portal",
        width = "420px",
        value = isolate(step2a_state$meta$simulation_details %||% "")
      )
    } else {
      NULL
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
    pid <- input$selected_equivalence_pipeline
    
    stored <- step2a_state$metrics[[pid]] %||% list()
    val <- stored$quality_metric_value %||% ""
    sel <- stored$equivalence_type %||% character(0)
    
    tagList(
      br(),
      fluidRow(
        column(6, h5("Quality Metric Value:", style = "text-align: center; margin-bottom: 5px;")),
        column(6, h5("Equivalence Type:", style = "text-align: left; margin-bottom: 5px;"))
      ),
      fluidRow(
        column(
          6,
          textInput(paste0("quality_metric_value_", pid), label = NULL, width = "100%", value = val)
        ),
        column(
          6,
          div(
            style = "display: flex; align-items: center;",
            radioButtons(
              paste0("equivalence_type_", pid),
              label   = NULL,
              choices = c("Type E", "Type N"),
              inline  = TRUE,
              selected = if (length(sel) == 1 && nzchar(sel)) sel else character(0)
            )
          )
        )
      )
    )
  })
  
  observe({
    req(!step2a_restoring())
    pid <- input$selected_equivalence_pipeline
    req(pid)
    qv <- input[[paste0("quality_metric_value_", pid)]]
    qt <- input[[paste0("equivalence_type_", pid)]]
    
    # Only update state when inputs actually exist (UI rendered) and at least one is non-NULL
    if (is.null(qv) && is.null(qt)) return()
    
    step2a_state$metrics[[pid]] <- list(
      quality_metric_value = qv %||% "",
      equivalence_type     = qt %||% ""
    )
  })
  
  # Download Export Progress Step 2a
  output$export_zip_step2a <- downloadHandler(
    filename = function() {
      nickname <- input$user_nickname
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      if (is.null(nickname) || nickname == "") paste0("mv_step2a_", ts, ".zip")
      else paste0(nickname, "_mv_step2a_", ts, ".zip")
    },
    # Let Shiny set the content type; avoids weird browser behavior on errors
    content = function(file) {
      # Guard: required packages
      if (!requireNamespace("zip", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
        showNotification("Export failed: please install packages 'zip' and 'jsonlite'.",
                         type = "error", duration = 7)
        return(invisible())
      }
      
      # Build Step 2a CSVs -----------------------------------------
      mode <- as.character(input$equivalence_assessment_mode %||% "Subsampling")
      sim_details <- if (mode %in% c("Simulation","Both"))
        as.character(input$simulation_details_link %||% "")
      else
        ""
      
      step2a_meta <- data.frame(
        equivalence_criterion = input$EquvalanceCriterion %||% "",
        threshold             = input$ThresholdEquvalance %||% "",
        subsample             = input$SubsampleEquvalance %||% "",
        assessment_mode       = mode,
        simulation_details    = sim_details,
        stringsAsFactors = FALSE
      )
      
      pipe_keys <- names(saved_pipelines$data)
      pipe_keys <- pipe_keys[!grepl("^graph_", pipe_keys)]
      
      step2a_metrics <- if (length(pipe_keys)) {
        do.call(rbind, lapply(pipe_keys, function(pid) {
          data.frame(
            pipeline_id          = pid,
            quality_metric_value = as.character(input[[paste0("quality_metric_value_", pid)]] %||% ""),
            equivalence_type     = as.character(input[[paste0("equivalence_type_",     pid)]] %||% ""),
            stringsAsFactors     = FALSE
          )
        }))
      } else data.frame(pipeline_id=character(0),
                        quality_metric_value=character(0),
                        equivalence_type=character(0),
                        stringsAsFactors=FALSE)
      
      # Pipelines + graphs (UTF-8; keep “➝” as-is)
      step2a_pipelines <- data.frame(
        pipeline_id = character(0),
        order_index = integer(0),
        label       = character(0),
        stringsAsFactors = FALSE
      )
      step2a_graph_nodes <- data.frame(
        pipeline_id = character(0),
        id    = character(0),
        label = character(0),
        color = character(0),
        shape = character(0),
        level = numeric(0),
        stringsAsFactors = FALSE
      )
      step2a_graph_edges <- data.frame(
        pipeline_id = character(0),
        from = character(0),
        to   = character(0),
        stringsAsFactors = FALSE
      )
      
      saved <- saved_pipelines$data %||% list()
      if (length(saved)) {
        pk <- names(saved); pk <- pk[!grepl("^graph_", pk)]
        for (pid in pk) {
          seq_labels <- saved[[pid]] %||% character(0)
          if (length(seq_labels)) {
            # Normalize any encoded arrow back to “➝” for neat round-trips
            seq_labels <- fix_arrow(seq_labels)
            step2a_pipelines <- rbind(step2a_pipelines, data.frame(
              pipeline_id = pid,
              order_index = seq_along(seq_labels),
              label       = seq_labels,
              stringsAsFactors = FALSE
            ))
          }
          gkey <- paste0("graph_", pid)
          if (is.list(saved[[gkey]])) {
            g <- saved[[gkey]]
            if (is.data.frame(g$nodes) && nrow(g$nodes)) {
              tmp <- g$nodes
              tmp$pipeline_id <- pid
              if ("label" %in% names(tmp)) tmp$label <- fix_arrow(tmp$label)
              # ensure columns exist and ordered
              for (nm in c("id","label","color","shape","level"))
                if (!(nm %in% names(tmp))) tmp[[nm]] <- NA
              step2a_graph_nodes <- rbind(step2a_graph_nodes,
                                          tmp[, c("pipeline_id","id","label","color","shape","level")])
            }
            if (is.data.frame(g$edges) && nrow(g$edges)) {
              tmp <- g$edges
              tmp$pipeline_id <- pid
              for (nm in c("from","to")) if (!(nm %in% names(tmp))) tmp[[nm]] <- NA
              step2a_graph_edges <- rbind(step2a_graph_edges,
                                          tmp[, c("pipeline_id","from","to")])
            }
          }
        }
      }
      
      # Write everything to a temp folder ---------------------------
      outdir <- tempfile("mv2a_")
      dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
      on.exit(try(unlink(outdir, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
      
      ok <- TRUE
      ok <- ok && write_csv_safely(step2a_meta,        file.path(outdir, "step2a_meta.csv"))
      ok <- ok && write_csv_safely(step2a_metrics,     file.path(outdir, "step2a_pipeline_metrics.csv"))
      ok <- ok && write_csv_safely(step2a_pipelines,   file.path(outdir, "step2a_pipelines.csv"))
      ok <- ok && write_csv_safely(step2a_graph_nodes, file.path(outdir, "step2a_graph_nodes.csv"))
      ok <- ok && write_csv_safely(step2a_graph_edges, file.path(outdir, "step2a_graph_edges.csv"))
      
      rel_files <- c(
        "step2a_meta.csv",
        "step2a_pipeline_metrics.csv",
        "step2a_pipelines.csv",
        "step2a_graph_nodes.csv",
        "step2a_graph_edges.csv"
      )
      
      # Manifest
      tryCatch({
        write_manifest_json(outdir, exported_step = "2a-only", csvs = rel_files)
      }, error = function(e) {
        showNotification(paste0("Export failed while writing manifest: ", e$message),
                         type = "error", duration = 7)
        ok <<- FALSE
      })
      
      if (!ok) return(invisible())
      
      # Zip (robust) -----------------------------------------------
      old_wd <- getwd()
      setwd(outdir)
      on.exit(try(setwd(old_wd), silent = TRUE), add = TRUE)
      
      zip_ok <- TRUE
      tryCatch({
        zip::zipr(zipfile = file,
                  files = c(rel_files, "manifest.json"),
                  include_directories = FALSE)
      }, error = function(e) {
        zip_ok <<- FALSE
        showNotification(paste0("Export failed while creating ZIP: ", e$message),
                         type = "error", duration = 7)
      })
      
      if (!zip_ok || !file.exists(file) || isTRUE(file.size(file) == 0)) {
        showNotification("Export failed: ZIP file was not created.", type = "error", duration = 7)
        return(invisible())
      }
      
      showNotification("Step 2a ZIP exported successfully.", type = "message", duration = 5)
    }
  )
  
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
      timeStamp <- format(Sys.time(), "%d-%m-%Y_%H-%M-%OS3")
      if (is.null(nickname) || nickname == "") {
        paste0("Principled_Multiverse_", timeStamp, ".pdf")
      } else {
        paste0(nickname, "_Principled_Multiverse_", timeStamp, ".pdf")
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
      timeStamp <- format(Sys.time(), "%d-%m-%Y_%H-%M-%OS3")
      correct_filename <- if (is.null(nickname) || nickname == "") {
        paste0("Principled_Multiverse_", timeStamp, ".pdf")
      } else {
        paste0(nickname, "_Principled_Multiverse_", timeStamp, ".pdf")
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
        "# Equivalence assessment approach",
        "`r if (!is.null(input$equivalence_assessment_mode) && input$equivalence_assessment_mode != '') { paste('Mode:', input$equivalence_assessment_mode) } else { 'No selection provided.' }`",
        "",
        "`r if (!is.null(input$equivalence_assessment_mode) && input$equivalence_assessment_mode %in% c('Simulation','Both')) { if (!is.null(input$simulation_details_link) && input$simulation_details_link != '') paste('Simulation details/link:', input$simulation_details_link) else 'No simulation details provided.' } else { '' }`",
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

  observeEvent(input$btn_prev_about, {
    updateNavbarPage(session, "main_nav", selected = "mv2")
    shinyjs::delay(0, updateTabsetPanel(session, "mv2_tabs", selected = "2b"))
  })
##############################################################  
}
