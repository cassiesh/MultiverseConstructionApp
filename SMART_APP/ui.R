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

source("Source_function.R", local = TRUE)

nav_buttons <- function(prev_id = NULL, next_id = NULL, prev_label = "Previous", next_label = "Next") {
  tags$div(
    style = "display:flex; justify-content:space-between; gap:10px; margin-top:20px;",
    if (!is.null(prev_id)) actionButton(prev_id, prev_label, class = "btn-secondary") else div(),
    if (!is.null(next_id)) actionButton(next_id, next_label, class = "btn-primary") else div()
  )
}

ui <- navbarPage(
  title = "Systematic Multiverse Analysis Registration Tool",
  id = "main_nav",
  theme = shinytheme("flatly"),
  useShinyjs(),

################################## Welcome Tab #################################
tabPanel(
    "Welcome", value = "welcome",
    navset_pill(
      id = "welcome_tabs",
      
      # --- Information subtab ---
      tabPanel(
        "Information", value = "info",
        fluidPage(
          wellPanel(
            style = "background-color: #f8f9fa; padding: 50px; border-radius: 10px; 
                   box-shadow: 10px 10px 10px rgba(0, 0, 0, 0.1);",
            
            h1(strong("Welcome to the Systematic Multiverse Analysis Registration Tool!")),
            p("This app is designed to guide researchers through the transparent and systematic 
            definition of pipelines to be included in multiverse analyses. By using this tool, 
            you can ensure your research meets recently proposed guidelines (Short et al., 2025) 
            of transparency and rigor.",
              strong("Before using it for the first time, we recommend watching our ",
              a(href = 'https://www.youtube.com/watch?v=f3_SiHl9tOA', 
                'tutorial video', target = "_blank"), "that demonstrates the use of app."),
              style="text-align: justify; font-size:20px"),
            
            h3(strong("Why Multiverse Analysis?")),
            p("Multiverse analysis addresses critical issues in research replicability 
            by reporting the uncertainty that arises in the reported outcome due to 
            the multiplicity of defensible data processing and analysis decisions. 
            By reporting a distribution of outcomes across defensible variations in 
            data processing and analysis, researchers can:", style="text-align: justify; font-size:20px"),
            tags$ul(
              tags$li("Communicate uncertainty: Illustrate how different analytical choices 
                    influence research findings.", style="text-align: justify; font-size:20px"),
              tags$li("Foster robust scientific findings: Build confidence in results 
                    by demonstrating their consistency across defensible analytic 
                    variations.", style="text-align: justify; font-size:20px"),
              tags$li("Enhance transparency: Clearly document and communicate the decision-making 
                    process.", style="text-align: justify; font-size:20px")
            ),
            
            p("In order to achieve these benefits, the multiverse should be principled 
            (Del Giudice & Gangestad, 2021), including only defensible pipelines that 
            are arbitrary in terms of a specific criterion, such as validity, testing 
            for the same effect, and estimating the effect with comparable precision. 
            This ensures that the included pipelines align with the research goals 
            without introducing irrelevant noise.", style="text-align: justify; font-size:20px"),
            
            p("Implementing this principled approach requires transparency and systematicity 
            throughout the decision-making process. This is where the Multiverse Analysis 
            Preregistration App helps, providing researchers with a structured, user-friendly 
            platform to define, evaluate, and refine their multiverse analyses. By 
            facilitating these critical steps, the app ensures that researchers can 
            achieve transparency, rigor, and replicability in their multiverse studies.", style="text-align: justify; font-size:20px"),
            
            h3(strong("Purpose and Use of the App")),
            p("The Multiverse Analysis Preregistration App is designed as a practical 
            solution for researchers across disciplines, providing a user-friendly 
            interface for defining, evaluating, and refining analytic pipelines in a 
            systematic and transparent way.", style="text-align: justify; font-size:20px"),
            
            p("The app serves several key purposes:", style="text-align: justify; font-size:20px"),
            tags$ul(
              tags$li("Supporting rigorous decision-making: Help users identify defensible 
                    pipelines and refine them based on clear criteria, reducing the 
                    risk of bias.", style="text-align: justify; font-size:20px"),
              tags$li("Efficient and systematic implementation: Save time and effort by 
                    using a structured workflow.", style="text-align: justify; font-size:20px"),
              tags$li("Facilitating transparency: Generate detailed documentation of the 
                    decision-making process that can be shared as a multiverse analysis 
                    preregistration of pipelines and as supplementary information with 
                    the reported results, to make your multiverse analysis traceable 
                    and reproducible.", style="text-align: justify; font-size:20px"),
              tags$li("Encouraging interdisciplinary innovation: Bridge gaps between fields 
                    by providing a standardized platform for multiverse analysis that 
                    incorporates best practices and fosters collaboration.", style="text-align: justify; font-size:20px")
            ),
            
            p("The app is intended for use by researchers in fields that can benefit 
            from multiverse analysis, such as Sociology, Psychology, Neurocognitive 
            Science, Education, Economics, Epidemiology and beyond, helping them to 
            define and refine their multiverse analyses with greater ease. Whether you 
            are conducting a single study or collaborating on a large-scale interdisciplinary 
            project, this tool is designed to make multiverse analysis more accessible 
            and impactful.", style="text-align: justify; font-size:20px"),
            br(),
            p(strong("Important Note:"), 
              " This app is designed to support the documentation and systematic procedure of a multiverse analysis. 
              It will guide you through recording and organizing your decisions, 
              but it does not provide recommendations or determine which decisions are defensible for your specific research context.",
              style = "text-align: justify; font-size:20px"),
            
            h3(strong("App Features")),
            p("This app walks you through two key stages:", style="text-align: justify; font-size:20px"),
            tags$ul(
              tags$li("Multiverse 1.0: Define the defensible pipelines for your analysis.", style="text-align: justify; font-size:20px"),
              tags$li("Multiverse 2.0: Refine these pipelines into a principled multiverse 
                    (Del Giudice and Gangestad, 2021).", style="text-align: justify; font-size:20px")
            ),
            
            p("Each step is designed to be intuitive and exportable. Overall this app 
            is here to guide you through a transparent, systematic, and replicable 
            research process.", style="text-align: justify; font-size:20px"),
            
            p("You can watch the instruction ", 
              a(href = 'https://www.youtube.com/watch?v=f3_SiHl9tOA', 
                'video', target = "_blank"), 
              "that demonstrates the use of app.", 
              style = "text-align: justify; font-size:20px"),
            
            h3(strong("References")),
            p("Del Giudice, M., & Gangestad, S. W. (2021). A traveler’s guide to the 
            multiverse: Promises, pitfalls, and a framework for the evaluation of 
            analytic decisions. Advances in Methods and Practices in Psychological 
            Science, 4(1).", a(href = "https://doi.org/10.1177/2515245920954925", 
            "https://doi.org/10.1177/2515245920954925", target = "_blank"), style="text-align: justify; font-size:20px"),
            p("Gollwitzer, M. (2020). DFG Priority Program SPP 2317 Proposal: A meta-scientific 
            program to analyze and optimize replicability in the behavioral, social, 
            and cognitive sciences (META-REP). PsychArchives.",
            a(href = "https://doi.org/10.23668/PSYCHARCHIVES.3010", "https://doi.org/10.23668/PSYCHARCHIVES.3010", target = "_blank"),
            style="text-align: justify; font-size:20px"),
            p("Short, C. A., Breznau, N., Bruntsch, M., Burkhardt, M., Busch, N., 
            Cesnaite, E., Frank, M., Gießing, C., Krähmer, D., Kristanto, D., 
            Lonsdorf, T., Neuendorf, C., Nguyen, H. H. V., Rausch, M., Schmalz, X., 
            Schneck, A., Tabakci, C., Hildebrandt, A. (2025a). Multi-curious: 
            A multi-disciplinary guide to a multiverse analysis. MetaArXiV.",
            a(href = "https://doi.org/10.31222/osf.io/4yzeh_v1", "https://doi.org/10.31222/osf.io/4yzeh_v1", target = "_blank"),
            style="text-align: justify; font-size:20px")
          ),
          nav_buttons(
            prev_id = NULL,
            next_id = "btn_next_info",
            next_label = "Continue →"
          )
        )
      ),
      
      # --- Start subtab ---
      tabPanel(
        "Start", value = "start",
        fluidPage(
          wellPanel(
            style = "background-color: #f8f9fa; padding: 50px; border-radius: 10px; 
                   box-shadow: 10px 10px 10px rgba(0, 0, 0, 0.1);",
            
            h2(strong("Get Started")),
            p("Please enter a username in the text box below that will be used as a 
              unique identifier for your multiverse in our repository. 
              The username is automatically saved as soon as you type it. 
              Please use only letters, numbers, underscores (_) or hyphens (-). 
              Spaces and special characters are not allowed.",
              style="text-align: justify; font-size:20px"),
            
            textInput("user_nickname", "Enter Your Username:", value = "", placeholder = "Type your username here"),
            actionButton("save_username", "Save Username", class = "btn-primary"),
            
            p("All data (username and pipeline decisions) are processed and stored in 
            compliance with GDPR regulations. The application is hosted by the Shiny 
            App server of the META-REP project (Gollwitzer, 2020).", 
              style="text-align: justify; font-size:20px"),
            
            h3(strong("Continue Your Multiverse Analysis")),
            p("If you have previously completed Multiverse 1.0 and saved your progress, 
            you can upload your saved Construction Documentation zip folder to continue 
            from where you left off. Please do not change the file structure within 
            the zip folder. You will be able to upload this zip folder later to 
            retrieve your progress.", style="text-align: justify; font-size:20px"),
            
            fileInput("progress_file", "Upload Progress (CSV or ZIP)", accept = c(".zip", ".csv")),
            actionButton("load_progress", "Load Progress", class = "btn-primary"),
            
            p("Once uploaded, the app will automatically restore your previous selections 
            and inputs, allowing you to continue directly with Multiverse 2.0.", 
              style="text-align: justify; font-size:20px")
          ),
          nav_buttons(                      
            prev_id = "btn_prev_start",
            next_id = "btn_next_start",
            prev_label = "← Back",
            next_label = "Continue →"
          )
        )
      )
    )
),
################################################################################
  
################################# Multiverse 1.0 ###############################
tabPanel(
  "Multiverse 1.0", value = "mv1",
  navset_pill(
    id = "mv1_tabs",
################# Step 1a: Define Multiverse Space #################
tabPanel(
  "Step 1a: Define Multiverse Space", value = "1a",
  
  fluidPage(
    
    # ---- Single wellPanel containing instructions, scope, and methods ----
    wellPanel(
      style = "background-color:#f8f9fa; padding:15px; border-radius:5px;",
      
      # --- Instructions ---
      div(style="text-align:justify; font-size:20px",
          p("Select the sections of the research procedure that will be forked as part of the multiverse analysis."),
          p("For each selected section:"),
          tags$ol(
            tags$li(
              "If all the decision nodes that fall within that specific section of the research procedure (e.g., data processing) are to be forked in the multiverse analysis, type ‘all’ in the description box.",
              tags$br(), strong("Example:"), " You plan to fork all preprocessing decisions, from raw data to modelling. Write: ‘All’."
            ),
            tags$li(
              "If only a subset of the decision nodes at that stage of the procedure will be forked in the multiverse analysis, enter the specific decision nodes in the box.",
              tags$br(), strong("Example:"), " You will only fork outlier detection in the data processing section because you wish to focus on the robustness of an effect to this decision node specifically. Write: ‘Outlier detection’."
            ),
            tags$li(
              "If you will not fork any decision nodes at that stage of the procedure, leave the box empty.",
              tags$br(), strong("Example:"), " If you will focus on robustness to data preprocessing but not to measurement, model specification, or estimation methods, leave the boxes underneath ‘measurement’, ‘model specification’, and ‘Estimation methods’ empty."
            )
          ),
          p(strong("Note:"), " At this stage, you are not creating pipeline sequences. Simply list all decision nodes and options — whether defensible or not — that are within scope. Pipeline sequences will be constructed in a later step."),
      ),
      
      hr(),
      h3(strong("Plan Your Analysis of Multiverse Results")),
      
      # 1) Descriptive analysis
      div(
        style = "margin-top: 6px;",
        div(
          style="display:flex; align-items:center; position:relative;",
          p("If you will descriptively analyse the multiverse of results, please enter in the text box below how you will do this:",
            style = "margin:0;"),
          tags$span(
            id="tooltip_step1a_desc",
            class="tooltip-icon",
            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative;",
            "?"
          )
        ),
        bsTooltip(
          "tooltip_step1a_desc",
          "Describe how you plan to summarise and visualise your multiverse results. Examples include specification curve plots, reporting the percentage of statistically non-significant results, or summarising distributions of effect sizes. e.g., specification curve plots; % non-significant; effect size distributions",
          placement = "right", trigger = "hover"
        ),
        textInput(
          "step1a_descriptive_analysis",
          label = NULL,
          placeholder = "e.g., specification curve plots; % non-significant; effect size distributions",
          width = "70%"
        )
      ),
      
      # 2) Inferential analysis
      div(
        style = "margin-top: 12px;",
        div(
          style="display:flex; align-items:center; position:relative;",
          p("If you will inferentially analyse the multiverse of results, please enter in the text box below how you will do this:",
            style = "margin:0;"),
          tags$span(
            id="tooltip_step1a_infer",
            class="tooltip-icon",
            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative;",
            "?"
          )
        ),
        bsTooltip(
          "tooltip_step1a_infer",
          "Specify the planned inferential method(s) you will use to formally test hypotheses across the multiverse (e.g., the PIMA framework, Girardi et al., 2024. Girardi, P., Vesely, A., Lakens, D., Altoè, G., Pastore, M., Calcagnì, A., & Finos, L. (2024). Post-selection inference in multiverse analysis (PIMA): An inferential framework based on the sign flipping score test. Psychometrika, 89(2), 542–568.). e.g., PIMA framework; sign-flipping score test; permutation-based PSI",
          placement = "right", trigger = "hover"
        ),
        textInput(
          "step1a_inferential_analysis",
          label = NULL,
          placeholder = "e.g., PIMA framework; sign-flipping score test; permutation-based PSI",
          width = "70%"
        )
      ),
      
      # 3) Multiple comparisons correction
      div(
        style = "margin-top: 12px;",
        div(
          style="display:flex; align-items:center; position:relative;",
          p("If you will apply a correction for multiple comparisons, how will you do this:",
            style = "margin:0;"),
          tags$span(
            id="tooltip_step1a_multy",
            class="tooltip-icon",
            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative;",
            "?"
          )
        ),
        bsTooltip(
          "tooltip_step1a_multy",
          "Indicate whether you will adjust for multiple testing in your inferential analysis, and if so, specify the correction method. If the method you are using has an inbuilt correction, note that here. e.g., Bonferroni; Holm; BH/FDR; inbuilt correction of chosen method",
          placement = "right", trigger = "hover"
        ),
        textInput(
          "step1a_multiple_correction",
          label = NULL,
          placeholder = "e.g., Bonferroni; Holm; BH/FDR; inbuilt correction of chosen method",
          width = "70%"
        )
      ),
      
      div(style = "text-align:right; margin-top:8px;",
          actionButton("save_analysis_plan_1a", "Save Analysis Plan", class = "btn-success")
      ),

      # ---- Scope (left) and Methods (right) side-by-side ----
      fluidRow(
        # --- Scope ---
        column(
          6,
          h3(strong("Specify Your Multiverse Scope")),
          p("Define the aspects of your research that will be included in your multiverse analysis. You may choose multiple specific categories, or specify in the accompanying text box any subset of one category that will be the focus.",
            style = "text-align: justify; font-size:20px"),
          
          fluidRow(
            column(
              6,
              div(style="display:flex; align-items:center; position:relative;",
                  checkboxInput("aspect1a_1", "Measurement", value = FALSE),
                  tags$span(id="tooltip_aspect1a_1", class="tooltip-icon",
                            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative; top:-15px;",
                            "?")
              ),
              bsTooltip("tooltip_aspect1a_1",
                        "Operationalization, measurement, and/or collection of input and output variables.",
                        placement = "right", trigger = "hover"),
              textInput("aspect1a_text_1", NULL, placeholder = "Enter description")
            ),
            column(
              6,
              div(style="display:flex; align-items:center; position:relative;",
                  checkboxInput("aspect1a_2", "Preprocessing", value = FALSE),
                  tags$span(id="tooltip_aspect1a_2", class="tooltip-icon",
                            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative; top:-15px;",
                            "?")
              ),
              bsTooltip("tooltip_aspect1a_2",
                        "Preparing data for analysis. E.g., selection, cleaning, transformation.",
                        placement = "right", trigger = "hover"),
              textInput("aspect1a_text_2", NULL, placeholder = "Enter description")
            )
          ),
          
          fluidRow(
            column(
              6,
              div(style="display:flex; align-items:center; position:relative;",
                  checkboxInput("aspect1a_3", "Model Specification", value = FALSE),
                  tags$span(id="tooltip_aspect1a_3", class="tooltip-icon",
                            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative; top:-15px;",
                            "?")
              ),
              bsTooltip("tooltip_aspect1a_3",
                        "Specification of the statistical model.",
                        placement = "right", trigger = "hover"),
              textInput("aspect1a_text_3", NULL, placeholder = "Enter description")
            ),
            column(
              6,
              div(style="display:flex; align-items:center; position:relative;",
                  checkboxInput("aspect1a_4", "Estimation methods", value = FALSE),
                  tags$span(id="tooltip_aspect1a_4", class="tooltip-icon",
                            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative; top:-15px;",
                            "?")
              ),
              bsTooltip("tooltip_aspect1a_4",
                        "Method to estimate parameters.",
                        placement = "right", trigger = "hover"),
              textInput("aspect1a_text_4", NULL, placeholder = "Enter description")
            )
          ),
          
          div(style="text-align:right; margin-top:8px;",
              actionButton("save_scope", "Save", class = "btn-success"))
        ),
        
        # --- Methods ---
        column(
          6,
          div(
            style = "display:flex; align-items:center; gap:8px; margin-top:20px;",
            h3(style = "margin:0;", strong("Identify Defensible Pipelines")),
            actionLink(
              "open_methods_help", 
              label = NULL, 
              icon = icon("question-circle"),
              style = "font-size:20px; color:#0d6efd; text-decoration:none;",
              title = "What goes here?"
            )
          ),
          br(),
          p("Select the method for identifying defensible pipelines, and please provide any additional details in the accompanying text box below.",
            style = "text-align: justify; font-size:20px"),
          br(),
          fluidRow(
            column(
              6,
              div(style="display:flex; align-items:center; position:relative;",
                  checkboxInput("defens1a_1", "Literature review", value = FALSE),
                  tags$span(id="tooltip_defens1a_1", class="tooltip-icon",
                            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative; top:-15px;",
                            "?")
              ),
              bsTooltip("tooltip_defens1a_1",
                        "Alternative options or pipelines are defined by their use in relevant previous literature, as identified through a systematic literature review.",
                        placement = "right", trigger = "hover"),
              textInput("defens1a_text_1", NULL, placeholder = "Enter description")
            ),
            column(
              6,
              div(style="display:flex; align-items:center; position:relative;",
                  checkboxInput("defens1a_2", "Expertise", value = FALSE),
                  tags$span(id="tooltip_defens1a_2", class="tooltip-icon",
                            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative; top:-15px;",
                            "?")
              ),
              bsTooltip("tooltip_defens1a_2",
                        "Alternative options or pipelines are identified based on expertise. This may be the expertise of an individual researcher working independently, or the combined judgment of a team working collaboratively.",
                        placement = "right", trigger = "hover"),
              textInput("defens1a_text_2", NULL, placeholder = "Enter description")
            )
          ),
          
          fluidRow(
            column(
              6,
              div(style="display:flex; align-items:center; position:relative;",
                  checkboxInput("defens1a_3", "Crowdsourcing", value = FALSE),
                  tags$span(id="tooltip_defens1a_3", class="tooltip-icon",
                            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative; top:-15px;",
                            "?")
              ),
              bsTooltip("tooltip_defens1a_3",
                        "Multiple experts contribute one defensible option or pipeline each.",
                        placement = "right", trigger = "hover"),
              textInput("defens1a_text_3", NULL, placeholder = "Enter description")
            ),
            column(
              6,
              div(style="display:flex; align-items:center; position:relative;",
                  checkboxInput("defens1a_4", "Other", value = FALSE),
                  tags$span(id="tooltip_defens1a_4", class="tooltip-icon",
                            style="margin-left:10px; font-size:14px; color:#007BFF; cursor:pointer; position:relative; top:-15px;",
                            "?")
              ),
              bsTooltip("tooltip_defens1a_4",
                        "Please describe the alternative approach in the text box below.",
                        placement = "right", trigger = "hover"),
              textInput("defens1a_text_4", NULL, placeholder = "Enter description")
            )
          ),
          div(style="text-align:right; margin-top:8px;",
              actionButton("save_methods", "Save", class = "btn-success"))
        )
      )
    ),
    
    hr(),
    
    # ---- Rest stays in sidebar/main layout ----
    sidebarLayout(
      sidebarPanel(
        width = 5,
        h3(strong("List Decision Nodes and Options to be considered"),
          actionLink("info_nodes", label = NULL, icon = icon("question-circle"), style = "color: #007BFF; margin-left: 8px;")),
        p("Using one text box for one decision node, list all decision 
           nodes that will be considered in your multiverse analysis, 
           regardless of defensibility. Please use the smaller text 
           boxes underneath each decision node to list all options 
           within that decision node, regardless of their defensibility. 
           The full multiverse for defensibility consideration will be presented 
           to you in the next step of the procedure.",
          style="text-align: justify; font-size:20px"),
        p("Click on the node title (e.g., 'Node 1') to expand and see the list of options.", style="text-align: justify; font-size:20px"),
        div(id = "accordion_container_step1a"),
        actionButton("add_step", "Add Node", class = "btn-primary", style = "margin-top: 10px;"),
        actionButton("delete_step", "Delete Node", class = "btn-danger", style = "margin-top: 10px;"),
        div(style = "text-align:right; margin-top:10px;",
            actionButton("save_nodes_options", "Save All Nodes & Options", class = "btn-success")
        ),
        div(style="margin-top:10px; text-align:right;",
            downloadButton("export_zip_step1a", "Export Progress", class = "btn-primary")
        )
      ),
      mainPanel(
        width = 7,
        visNetworkOutput("pipeline_network_1a", height = "calc(100vh - 150px)")
      )
    ),
    
    nav_buttons(
      prev_id = "btn_prev_1a",
      next_id = "btn_next_1a",
      prev_label = "← Back",
      next_label = "Continue →"
    )
  )
),
##############################################################
    
############### Step 1b: Define Defensibility ################
    tabPanel(
      "Step 1b: Define Defensibility", value = "1b",
      wellPanel(style = "background-color:#f8f9fa; padding:15px; border-radius:5px;",
                p("In this step of the procedure, you should review your listed decision 
                nodes and options and categorise them as defensible (even if the 
                defensibility is conditional on other options, or on its place 
                along the pipeline), or indefensible (regardless of other options 
                along the pipeline, or where this would be placed along the pipeline 
                sequence, this option at this decision node will never be defensible 
                for your research question or dataset). You do not need to consider 
                equivalence yet – only the defensibility of the decision nodes 
                and options.", style="text-align: justify;", style = "font-size:20px"),
                p("Using the radio buttons, please label each decision node and option 
                as defensible or indefensible.", style="text-align: justify;", style = "font-size:20px"),
                p("Using the text boxes below, write the justifications that will 
                be used for your defensibility decisions. Then assign the relevant justifications to each defensible 
                or indefensible label.", style="text-align: justify;", style = "font-size:20px"),
                p("This process ensures that only valid and justified elements are 
                included in your analysis, promoting rigor and transparency.", 
                  style="text-align: justify;", style = "font-size:20px"),
      ),
      
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            width = 5,
            div(id = "accordion_container_step1b"),
            br(),
            div(style = "text-align:right; margin-top:10px;",
            actionButton("save_step1b_all", "Save", class = "btn btn-success")),
            div(style = "text-align:right; margin-top:10px;",
                downloadButton("export_zip_step1b", "Export Progress", class = "btn btn-primary")
            )
          ),
          mainPanel(
            width = 7,
            br(),
            visNetworkOutput("pipeline_network_step1b", height = "calc(100vh - 150px)")
          )
        ),
        nav_buttons(
          prev_id = "btn_prev_1b",
          next_id = "btn_next_1b",
          prev_label = "← Back",
          next_label = "Continue →"
        )
      )
    ),
##############################################################


############ Step 1c: Create Defensible Pipelines ############
    tabPanel(
      "Step 1c: Create Defensible Pipelines", value = "1c",
      mainPanel(
        width = 12,
        br(),
        
        # ===== Mode selector (shown first) =====
        p("Here you can see all of the decision nodes and options 
      that were categorized as defensible in the previous step.", 
          style = "text-align: justify;", style = "font-size:20px"),
        p("Below you can build defensible pipeline sequences in two ways:",
          style = "text-align: justify; font-size:20px"),
        tags$ul(
          tags$li(tags$b("Manual (Empty slate):"), " Start from an empty canvas and drag defensible nodes and their options to build pipelines by hand.", style = "text-align: justify; font-size:20px"),
          tags$li(tags$b("Automatic (Cartesian product):"), " Let the app create all combinations of defensible options across your defensible nodes, then you prune/remove pipelines you don’t want.", style = "text-align: justify; font-size:20px")
        ),
        p("Tip: The automatic mode can be very large if you have many nodes/options. Use it when you want a complete starting set and then remove.",
          style = "text-align: justify; font-size:20px"),
        div(
              style = "display:flex; gap:16px; justify-content:center; margin-top: 10px;",
              actionButton("start_manual_1c",  "Start Manual (Empty slate)",  class = "btn-primary btn-lg"),
              actionButton("start_auto_1c",    "Start Automatic (Cartesian)", class = "btn-warning btn-lg")
        ),
        hr(),
        
        # ===== Work area (hidden until a mode is chosen) =====
        shinyjs::hidden(
          div(
            id = "work_area_1c",
            
            # Optional: a small header row once mode is chosen
            div(
              id = "work_header_1c",
              style = "display:flex; align-items:center; justify-content:space-between; margin-bottom: 10px;",
              p("Create pipelines below. You can add multiple pipelines and edit each independently.",
                style = "margin: 0; font-size: 18px;"),
            ),
            tags$hr(),
            
            # ===== AUTO MODE CONTROLS (hidden until user clicks Start Automatic) =====
            # Two-column layout (Pipeline Creation + Visualization)
            div(
              id = "work_two_col",
              style = "display: flex; gap: 20px; align-items: flex-start;",  # << key line
              # LEFT column
              div(
                style = "width: 40%;",
                # manual panel
                div(
                  id = "manual_left",
                  div(
                    id = "pipeline_creation_container",
                    div(id = "main_pipeline_accordion", class = "panel-group")
                  ),
                  actionButton("new_pipeline", "New Pipeline", class = "btn-primary", style = "margin-top: 8px;")
                ),
                
                shinyjs::hidden(
                  div(
                    id = "auto_mode_controls",
                    style = "margin-bottom: 10px;",
                    p("By default, SMART generates the Cartesian product of all defensible options across all nodes in the fixed order that the nodes were entered in Step 1b. Alternatively, selecting \"Advanced: include all node permutations\" will additionally generate all pipelines for all the Cartesian product of all options across all nodes for all possible combinations of node order.",
                      style = "text-align: justify; font-size: 20px;"),
                    p("Regardless of whether the Cartesian product is generated for the fixed sequence of nodes or for all possible orders of nodes, Once generated, individual pipelines or groups of pipelines can be deleted (e.g., all pipelines that contain a specific two-node order, or all pipelines that begin with a specific node).",
                      style = "text-align: justify; font-size: 20px;"),
                    checkboxInput("permute_nodes_1c", "Advanced: include all node permutations", value = FALSE),
                    actionButton("generate_pipelines_1c", "Generate Pipelines", class = "btn-success"),
                    shinyjs::hidden(
                      div(
                        id = "auto_pipeline_selector_container",
                        style = "margin-top: 10px;",
                        shinyWidgets::pickerInput(
                          inputId = "auto_pipeline_picker",
                          label   = "Select pipeline(s) to view / prune:",
                          choices = list(),           # server fills
                          multiple = TRUE,
                          options  = shinyWidgets::pickerOptions(
                            liveSearch   = TRUE,
                            actionsBox   = TRUE,      # Select All / Deselect All
                            size         = 10,        # dropdown body height
                            virtualScroll = 2000,
                            dropupAuto   = FALSE
                          )
                        )
                      )
                    )
                  )
                ),
                
                # auto panel (hidden until a pipeline is selected)
                shinyjs::hidden(
                  div(
                    id = "auto_left",
                    div(
                      id = "auto_actions_row",
                      style = "display:flex; gap:10px; align-items:center; margin-bottom:8px;",
                      actionButton("delete_auto_pipeline", "Delete Pipeline", class = "btn-danger"),
                      actionButton("save_auto_pipelines", "Save All Pipelines", class = "btn-success")
                    ),
                    h4(textOutput("auto_pipeline_title"), style = "margin:0 0 6px 0;"),  # trim top space
                    tableOutput("auto_pipeline_table")
                  )
                )
              ),
              # RIGHT column
              div(
                style = "width: 60%;",
                visNetworkOutput("pipeline_network_1c", height = "calc(100vh - 50px)")
              )
            ),
            
            nav_buttons(
              prev_id = "btn_prev_1c",
              next_id = "btn_next_1c",
              prev_label = "← Back",
              next_label = "Continue →"
            )
          )
        )
      ),
      tags$script(HTML("
          $(document).on('click', '[id^=selected_steps_] .rank-list-item', function() {
              var selectedText = $(this).text().trim();
              console.log('Clicked:', selectedText); // ✅ Debugging
          Shiny.setInputValue('clicked_pipeline_item', selectedText, {priority: 'event'});
          });
      ")),
      tags$script(HTML("
        (function(){
          // Intercept keyboard refresh (F5 / Ctrl+R / Cmd+R)
          document.addEventListener('keydown', function(e){
            var isF5    = (e.key === 'F5');
            var isCtrlR = (e.ctrlKey && (e.key === 'r' || e.key === 'R'));
            var isCmdR  = (e.metaKey && (e.key === 'r' || e.key === 'R')); // macOS

            if (isF5 || isCtrlR || isCmdR) {
              e.preventDefault();
              e.stopPropagation();
              // Tell server a refresh is being attempted; it will show a Shiny modal
              Shiny.setInputValue('refresh_attempt', Date.now(), {priority: 'event'});
            }
          });

          // Fallback for toolbar refresh / tab close: native beforeunload dialog.
          // NOTE: Browsers ignore custom strings here.
          var warnOnLeave = true; // keep always-on for this step; server can toggle later if you want
          window.addEventListener('beforeunload', function(e){
            if (!warnOnLeave) return;
            e.preventDefault();
            e.returnValue = '';
            return ''; // triggers native prompt
          });

          // Server tells us to actually reload (after user clicks Continue in the modal)
          Shiny.addCustomMessageHandler('doHardRefresh', function(){
            // remove the beforeunload guard briefly so we don't double prompt
            var listener = function(ev){};
            window.removeEventListener('beforeunload', listener);
            window.location.reload(true);
          });
        })();
      "))
    ),
##############################################################


################ Step 1d: Your Multiverse 1.0 ################
    tabPanel(
      "Step 1d: Your Multiverse 1.0", value = "1d",
      mainPanel(
        width = 12,
        br(),
        p("Below you can see a table of each saved pipeline and a path diagram of the defensible 
          multiverse that you have created. You can export this by clicking the ‘Export’ button 
          underneath each. Please continue the procedure to refine the Multiverse 1.0 into a 
          principled multiverse based on equivalence.",
          style = "text-align: justify; font-size:20px"),
        
        fluidRow(
          column(
            width = 6,
            # --- Controls row: dropdown + export button side-by-side ---
            div(
              style = "display:flex; align-items:flex-end; gap:12px; margin-bottom:8px;",
              div(
                style = "flex:1;",
                selectInput("selected_pipeline", "Choose a Pipeline:", choices = NULL, selected = NULL)
              ),
              div(
                downloadButton("export_zip_step1d", "Export Construction ZIP (1a–1c)", class = "btn-primary")
              )
            ),
            # Table below the controls row
            DT::dataTableOutput("pipeline_table"),
            hr(),
            h4("Option usage across pipelines", align = "center"),
            p(
              "Counts how often each option is selected within its decision node across all saved pipelines.",
              style = "text-align: justify; font-size:14px; margin-bottom:8px;"
            ),
            div(
              style = "font-size:12px; max-height:250px; overflow-y:auto; border:1px solid #ddd; padding:4px; border-radius:4px;",
              DT::dataTableOutput("option_frequency_table")
            )
          ),
          column(
            width = 6,
            visNetworkOutput("selected_pipeline_network", height = "calc(100vh - 150px)") 
          ),
          br(),
          div(
            style = "text-align: center; margin-top: 20px;",
            downloadButton("download_preprocessing_doc", "Download Preprocessing Documentation", class = "btn-primary")
          )
        ),
        nav_buttons(
          prev_id = "btn_prev_1d",
          next_id = "btn_next_1d",
          prev_label = "← Back",
          next_label = "Continue →"
        )
      )
    )
##############################################################
  )
),
################################################################################

################################# Multiverse 2.0 ###############################
tabPanel(
  "Multiverse 2.0", value = "mv2",
  navset_pill(
    id = "mv2_tabs",
############# Step 2a: Criterion for Equivalance #############
    tabPanel(
      "Step 2a: Criterion for Equivalance", value = "2a",
      mainPanel(width = 12,
                br(),
                p("To transition to a principled multiverse in a data-driven 
                        manner, please compute your multiverse 1.0 on a subset of 
                        your data (e.g., 10% of your sample of participants), 
                        plotting a quality metric and not an effect size of interest. 
                        Pipelines that fall within an interval threshold, which 
                        you will define à priori below, will be considered as equivalent 
                        and will be included in your principled multiverse.", 
                  style = "text-align: justify;", style = "font-size:20px"),
                p("In the text box below, please enter the criterion for 
                        equivalence (e.g., signal-to-noise ratio, standardised 
                        measurement error, intraclass correlation coefficient).", 
                  style = "text-align: justify;", style = "font-size:20px"),
                textAreaInput("EquvalanceCriterion", "", width = 10000),
                p("In the text box below, please set a threshold for equivalence 
                        (e.g., the interval values of proportion that will be used 
                        as a threshold for declaring equivalence in the aforementioned 
                        criterion).", style = "text-align: justify;", style = "font-size:20px"),
                textAreaInput("ThresholdEquvalance", "", width = 10000),
                
                div(
                  style = "display:flex; align-items:center; gap:16px; flex-wrap:wrap; margin-bottom:10px;",
                  tags$div(
                    style = "display:flex; align-items:center; gap:8px;",
                    tags$label("Equivalence assessment via:"),
                    radioButtons(
                      inputId = "equivalence_assessment_mode",
                      label   = NULL,
                      choices = c("Subsampling", "Simulation", "Both"),
                      inline  = TRUE,
                      selected = "Subsampling"
                    )
                  ),
                  # dynamic area for the extra text input (appears when Simulation or Both is chosen)
                  uiOutput("simulation_details_inline")
                ),
                
                p("In the text box below, specify the subsample for equivalence 
                        testing (e.g., a percentage of the final sample of participants).", 
                  style = "text-align: justify;", style = "font-size:20px"),
                textAreaInput("SubsampleEquvalance", "", width = 10000),
                h3(HTML("<u>Perform Equivalence Analysis</u>")),
                p("Please compute the multiverse 1.0 on the subset of data 
                        specified above, plotting the quality metric specified 
                        above.", style = "text-align: justify;", style = "font-size:20px"),
                p("Below you see the list of pipelines in your Multiverse 
                        1.0. For each pipeline, please enter the value of the quality 
                        metric, and please select either ‘Type E’ and ‘Type N’ 
                        next to each pipeline to report whether the pipeline did 
                        or did not meet the threshold for equivalence based on your 
                        criteria.", style = "text-align: justify;", style = "font-size:20px"),
                p("When you have done this, you can proceed to the final 
                        tab to view and export your principled multiverse analysis.", 
                  style = "text-align: justify;", style = "font-size:20px"),
                br(),
                selectInput("selected_equivalence_pipeline", "Choose a Pipeline:", 
                            choices = NULL, selected = NULL),
                fluidRow(
                  column(
                    6,
                    h3("Pipeline Structure", align = "center"),
                    DT::dataTableOutput("equivalence_pipeline_table"),
                    uiOutput("equivalence_inputs_ui"),
                    div(
                      style = "margin-top: 10px; margin-bottom: 10px;",
                      fileInput(
                        "load_step2a_zip",
                        label = "Load Step 2a Progress (.zip)",
                        accept = ".zip",
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"
                      ),
                      downloadButton("export_zip_step2a", "Export Step 2a Progress", class = "btn-primary")
                    )
                  ),
                  column(
                    6,
                    h3("Visual Network", align = "center"),
                    visNetworkOutput("equivalence_pipeline_network", height = "calc(100vh - 150px)")
                  )
                ),
                nav_buttons(
                  prev_id = "btn_prev_2a",
                  next_id = "btn_next_2a",
                  prev_label = "← Back",
                  next_label = "Continue →"
                )
      )
    ),
##############################################################

############ Step 2b: Your Principled Multiverse #############
    tabPanel(
      "Step 2b: Your Principled Multiverse", value = "2b",
      mainPanel(width = 12,
                p("Below you can find a table and visual representation of 
                  the principled multiverse. This can now be computed on 
                  your full sample to observe the distribution of the effect 
                  of interest across these equivalence variations.", 
                  style = "text-align: justify;", style = "font-size:20px"),
                
                h3(HTML("<u>Export Your Work</u>")),
                p("Download the complete documentation, including multiverse 
                  1.0, multiverse 2.0, and equivalence thresholds, in a .pdf 
                  format for preregistration or publication.", 
                  style = "text-align: justify;", style = "font-size:20px"),
                
                br(),
                
                selectInput("selected_principled_pipeline", "Choose a Type E Pipeline:", 
                            choices = NULL, selected = NULL),
                
                fluidRow(
                  column(
                    6,
                    h3("Principled Pipeline Structure", align = "center"),
                    DT::dataTableOutput("principled_pipeline_table")
                  ),
                  column(
                    6,
                    h3("Visual Network", align = "center"),
                    visNetworkOutput("principled_pipeline_network", height = "calc(100vh - 150px)")
                  )
                ),
                br(),
                downloadButton("download_principled_multiverse", "Download Your 
                               Principled Multiverse PDF", class = "btn-primary"),
                nav_buttons(
                  prev_id = "btn_prev_2b",
                  next_id = "btn_next_2b",
                  prev_label = "← Back",
                  next_label = "Continue →"
                )
      )
    )
##############################################################

  ),
),
################################################################################

###################################### About ###################################
tags$head(tags$style(HTML("
  /* Scope to About tab only */
  .about-tab .team-photo {
    max-width: 200px;   /* same visual width as before */
    width: 100%;
    height: auto;       /* <-- keeps aspect ratio */
    display: block;
    margin: 0 auto;
  }
  .about-tab .panel-body {
    /* remove any fixed sizes; let content size naturally */
    width: auto !important;
    height: auto !important;
  }
  /* Optional: make all cards the same visual height without squashing the photo */
  .about-tab .panel {
    min-height: 340px;  /* tweak if you want equal card heights */
  }
"))),

tabPanel(
  "About", value = "about",
  div(class = "about-tab",
      column(1),
      column(10,
             shiny::HTML("<h3><center>Thank you for using the Systematic Multiverse 
                   Analysis Registration Tool</center></h3><br>
                   <h2><center>Our team</center></h2><br>")
      ),
      
      # spacer
      fluidRow(style = "height: 20px;"),
      
      # team row
      fluidRow(
        column(2),  # left spacer
        
        # Andrea
        column(2,
               div(class="panel panel-default",
                   div(class="panel-body", align="center",
                       tags$img(src="andrea2.jpg", class="team-photo"),
                       tags$h5("Hildebrandt, Andrea, Prof. Dr. rer. nat."),
                       div("Professor for Psychological Methods and Statistics, Carl von Ossietzky Universität Oldenburg.")
                   )
               )
        ),
        
        # Cassie
        column(2,
               div(class="panel panel-default",
                   div(class="panel-body", align="center",
                       tags$img(src="cassie.jpg", class="team-photo"),
                       tags$h5("Short, Cassie, PhD."),
                       div("Carl von Ossietzky Universität Oldenburg.")
                   )
               )
        ),
        
        # Max
        column(2,
               div(class="panel panel-default",
                   div(class="panel-body", align="center",
                       tags$img(src="max.jpg", class="team-photo"),
                       tags$h5("Frank, Maximilian, MsC."),
                       div("Ludwig Maximilian Universität München.")
                   )
               )
        ),
        
        # Cosku
        column(2,
               div(class="panel panel-default",
                   div(class="panel-body", align="center",
                       tags$img(src="cosku.jpg", class="team-photo"),
                       tags$h5("Inceler, Yusuf Cosku, BsC."),
                       div("Carl von Ossietzky Universität Oldenburg.")
                   )
               )
        ),
        
        column(2)  # right spacer
      ),
      
      # contact row (put in its own row so it doesn't overflow the team row)
      fluidRow(
        column(1),
        column(10, shiny::HTML('
        <h3><center>For questions and suggestions, please 
        contact us at: cassie.ann.short@uni-oldenburg.de</center></h3>
        <h4><center>The code for this app is open for public access through 
        <a href="https://github.com/cassiesh/MultiverseConstructionApp" target="_blank">Github</a></center></h4>
      ')),
        column(1)
      ),
      
      fluidRow(style = "height: 50px;"),
      
      nav_buttons(
        prev_id = "btn_prev_about",
        next_id = NULL,
        prev_label = "← Back"
      )
  )
)

################################################################################
)
