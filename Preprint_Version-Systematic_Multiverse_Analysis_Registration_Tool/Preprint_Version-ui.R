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

ui <- navbarPage(
  title = "Systematic Multiverse Analysis Registration Tool",
  theme = shinytheme("flatly"),
  useShinyjs(),
  
################################## Welcome Tab #################################
  tabPanel(
    "Welcome",
    fluidPage(
      wellPanel(
        style = "background-color: #f8f9fa; padding: 50px; border-radius: 10px; box-shadow: 10px 10px 10px rgba(0, 0, 0, 0.1);",
        h1(strong("Welcome to the Systematic Multiverse Analysis Registration Tool!")),
        p("This app is designed to guide researchers through the transparent and systematic 
          definition of pipelines to be included in multiverse analyses. By using this tool, 
          you can ensure your research meets recently proposed guidelines (Short et al., 2025) 
          of transparency and rigor.", style="text-align: justify;", style = "font-size:20px"),
        h3(strong("Why Multiverse Analysis?")),
        p("Multiverse analysis addresses critical issues in research replicability 
          by reporting the uncertainty that arises in the reported outcome due to 
          the multiplicity of defensible data processing and analysis decisions. 
          By reporting a distribution of outcomes across defensible variations in 
          data processing and analysis, researchers can:", style="text-align: justify;", style = "font-size:20px"),
        tags$ul(
          tags$li("Communicate uncertainty: Illustrate how different analytical choices 
                  influence research findings.", style="text-align: justify;", style = "font-size:20px"),
          tags$li("Foster robust scientific findings: Build confidence in results 
                  by demonstrating their consistency across defensible analytic 
                  variations.", style="text-align: justify;", style = "font-size:20px"),
          tags$li("Enhance transparency: Clearly document and communicate the decision-making 
                  process.", style="text-align: justify;", style = "font-size:20px")
        ),
        p("In order to achieve these benefits, the multiverse should be principled 
          (Del Giudice & Gangestad, 2021), including only defensible pipelines that 
          are arbitrary in terms of a specific criterion, such as validity, testing 
          for the same effect, and estimating the effect with comparable precision. 
          This ensures that the included pipelines align with the research goals 
          without introducing irrelevant noise.", style="text-align: justify;", style = "font-size:20px"),
        p("Implementing this principled approach requires transparency and systematicity 
          throughout the decision-making process. This is where the Multiverse Analysis 
          Preregistration App helps, providing researchers with a structured, user-friendly 
          platform to define, evaluate, and refine their multiverse analyses. By 
          facilitating these critical steps, the app ensures that researchers can 
          achieve transparency, rigor, and replicability in their multiverse studies.", style="text-align: justify;", style = "font-size:20px"),
        h3(strong("Purpose and Use of the App")),
        p("The Multiverse Analysis Preregistration App is designed as a practical 
          solution for researchers across disciplines, providing a user-friendly 
          interface for defining, evaluating, and refining analytic pipelines in a 
          systematic and transparent way.", style="text-align: justify;", style = "font-size:20px"),
        p("The app serves several key purposes:", style="text-align: justify;", style = "font-size:20px"),
        tags$ul(
          tags$li("Supporting rigorous decision-making: Help users identify defensible 
                  pipelines and refine them based on clear criteria, reducing the 
                  risk of bias.", style="text-align: justify;", style = "font-size:20px"),
          tags$li("Efficient and systematic implementation: Save time and effort by 
                  using a structured workflow.", style="text-align: justify;", style = "font-size:20px"),
          tags$li("Facilitating transparency: Generate detailed documentation of the 
                  decision-making process that can be shared as a multiverse analysis 
                  preregistration of pipelines and as supplementary information with 
                  the reported results, to make your multiverse analysis traceable 
                  and reproducible.", style="text-align: justify;", style = "font-size:20px"),
          tags$li("Encouraging interdisciplinary innovation: Bridge gaps between fields 
                  by providing a standardized platform for multiverse analysis that 
                  incorporates best practices and fosters collaboration.", style="text-align: justify;", style = "font-size:20px")
        ),
        p("The app is intended for use by researchers in fields that can benefit 
          from multiverse analysis, such as Sociology, Psychology, Neurocognitive 
          Science, Education, Economics, Epidemiology and beyond, helping them to 
          define and refine their multiverse analyses with greater ease. Whether you 
          are conducting a single study or collaborating on a large-scale interdisciplinary 
          project, this tool is designed to make multiverse analysis more accessible 
          and impactful.", style="text-align: justify;", style = "font-size:20px"),
        h3(strong("App Features")),
        p("This app walks you through two key stages:", style="text-align: justify;", style = "font-size:20px"),
        tags$ul(
          tags$li("Multiverse 1.0: Define the defensible pipelines for your 
                  analysis.", style="text-align: justify;", style = "font-size:20px"),
          tags$li("Multiverse 2.0: Refine these pipelines into a principled multiverse 
                  (Del Giudice and Gangestad, 2021).", style="text-align: justify;", style = "font-size:20px")
        ),
        p("Each step is designed to be intuitive and exportable. Overall this app 
          is here to guide you through a transparent, systematic, and replicable 
          research process.", style="text-align: justify;", style = "font-size:20px"),
        p("You can watch the instruction ", a(href = 'https://www.youtube.com/watch?v=8qKZw3FecqA', 'video'), 
          "that demonstrates the use of app", style="text-align: justify;", style = "font-size:20px"),
        p("Please enter a username in the text box below that will be used as a 
          unique identifier for your multiverse in our repository.", 
          style="text-align: justify;", style = "font-size:20px"),
        textInput("user_nickname", "Enter Your Username:", value = "", placeholder = "Type your username here"),
        p("All data (username and pipeline decisions) are processed and stored in 
          compliance with GDPR regulations. The application is hosted by the Shiny 
          App server of the META-REP project (Gollwitzer, 2020).", 
          style="text-align: justify;", style = "font-size:20px"),
        h3(strong("Continue Your Multiverse Analysis")),
        p("If you have previously completed Multiverse 1.0 and saved your progress, 
          you can upload your saved Construction Documentation zip folder to continue 
          from where you left off. Please do not change the file structure within 
          the zip folder. You will be able to upload this zip folder later to 
          retrieve your progress.", style="text-align: justify;", style = "font-size:20px"),
        fileInput("upload_construction_doc", 
                  label = "Upload Construction Documentation", 
                  multiple = FALSE, 
                  accept = ".zip", 
                  buttonLabel = "Browse...", 
                  placeholder = "No file selected"),
        p("Once uploaded, the app will automatically restore your previous selections 
          and inputs, allowing you to continue directly with Multiverse 2.0.", 
          style="text-align: justify;", style = "font-size:20px"),
        h3(strong("References")),
        p("Del Giudice, M., & Gangestad, S. W. (2021). A traveler’s guide to the 
        multiverse: Promises, pitfalls, and a framework for the evaluation of 
        analytic decisions. Advances in Methods and Practices in Psychological 
        Science, 4(1), 2515245920954925. https://doi.org/10.1177/2515245920954925", 
          style="text-align: justify;", style = "font-size:20px"),
        p("Gollwitzer, M. (2020). DFG Priority Program SPP 2317 Proposal: A meta-scientific 
          program to analyze and optimize replicability in the behavioral, social, 
          and cognitive sciences (META-REP). PsychArchives. 
          https://doi.org/10.23668/PSYCHARCHIVES.3010", style="text-align: justify;", style = "font-size:20px"),
        p("Short, C. A., Breznau, N., Bruntsch, M., Burkhardt, M., Busch, N., 
          Cesnaite, E., Frank, M., Gießing, C., Krähmer, D., Kristanto, D., 
          Lonsdorf, T., Neuendorf, C., Nguyen, H. H. V., Rausch, M., Schmalz, X., 
          Schneck, A., Tabakci, C., Hildebrandt, A. (2025a). Multi-curious: 
          A multi-disciplinary guide to a multiverse analysis. MetaArXiV. 
          https://doi.org/10.31222/osf.io/4yzeh_v1", style="text-align: justify;", style = "font-size:20px")
      )
    )
  ),

################################################################################

################################# Multiverse 1.0 ###############################
tabPanel(
  "Multiverse 1.0",
  navset_pill(
    
################# Step 1a: Specify the Scope #################
    tabPanel(
      "Step 1a: Specify the Scope",
      fluidPage(
        sidebarLayout(
          sidebarPanel(width = 5,
                       h3(strong("Specify Your Multiverse Scope")),
                       p("Define the aspects of your research that will be included 
                        in your multiverse analysis:", style="text-align: justify;", style = "font-size:20px"),
                       column(6,
                              div(style="display: flex; align-items: center; position: relative;",
                                  checkboxInput("aspect1a_1", "Measurement", value = FALSE),
                                  tags$span(
                                    id = "tooltip_aspect1a_1",
                                    class = "tooltip-icon",
                                    style = "margin-left: 10px; font-size: 14px; color: #007BFF; cursor: pointer; position: relative; top: -15px;",
                                    "?"
                                  )
                              ),
                              bsTooltip("tooltip_aspect1a_1", "Operationalization, measurement, and/or collection of input and output variables.", 
                                        placement = "right", trigger = "hover"),
                              textInput("aspect1a_text_1", NULL, placeholder = "Enter details..."),
                       ),
                       column(6,
                              div(style="display: flex; align-items: center; position: relative;",
                                  checkboxInput("aspect1a_2", "Preprocessing", value = FALSE),
                                  tags$span(
                                    id = "tooltip_aspect1a_2",
                                    class = "tooltip-icon",
                                    style = "margin-left: 10px; font-size: 14px; color: #007BFF; cursor: pointer; position: relative; top: -15px;",
                                    "?"
                                  )
                              ),
                              bsTooltip("tooltip_aspect1a_2", "Preparing data for analysis. E.g., selection, cleaning, transformation.", 
                                        placement = "right", trigger = "hover"),
                              textInput("aspect1a_text_2", NULL, placeholder = "Enter details..."),
                       ),
                       column(6,
                              div(style="display: flex; align-items: center; position: relative;",
                                  checkboxInput("aspect1a_3", "Model Specification", value = FALSE),
                                  tags$span(
                                    id = "tooltip_aspect1a_3",
                                    class = "tooltip-icon",
                                    style = "margin-left: 10px; font-size: 14px; color: #007BFF; cursor: pointer; position: relative; top: -15px;",
                                    "?"
                                  )
                              ),
                              bsTooltip("tooltip_aspect1a_3", "Specification of the statistical model.", 
                                        placement = "right", trigger = "hover"),
                              textInput("aspect1a_text_3", NULL, placeholder = "Enter details..."),
                       ),
                       column(6,
                              div(style="display: flex; align-items: center; position: relative;",
                                  checkboxInput("aspect1a_4", "Estimation methods", value = FALSE),
                                  tags$span(
                                    id = "tooltip_aspect1a_4",
                                    class = "tooltip-icon",
                                    style = "margin-left: 10px; font-size: 14px; color: #007BFF; cursor: pointer; position: relative; top: -15px;",
                                    "?"
                                  )
                              ),
                              bsTooltip("tooltip_aspect1a_4", "Method to estimate parameters.", 
                                        placement = "right", trigger = "hover"),
                              textInput("aspect1a_text_4", NULL, placeholder = "Enter details..."),
                       ),
                       p("You may choose multiple specific categories, or specify 
                        in the accompanying text box any subset of one category 
                        that will be the focus.", style="text-align: justify;", style = "font-size:20px"),
                       h3(strong("Identify Defensible Pipelines")),
                       p("Select the method for identifying defensible pipelines:", style="text-align: justify;", style = "font-size:20px"),
                       column(6,
                              div(style="display: flex; align-items: center; position: relative;",
                                  checkboxInput("defens1a_1", "Literature review", value = FALSE),
                                  tags$span(
                                    id = "tooltip_defens1a_1",
                                    class = "tooltip-icon",
                                    style = "margin-left: 10px; font-size: 14px; color: #007BFF; cursor: pointer; position: relative; top: -15px;",
                                    "?"
                                  )
                              ),
                              bsTooltip("tooltip_defens1a_1", "Alternative options or pipelines are defined by their use in relevant previous literature, as identified through a systematic literature review.", 
                                        placement = "right", trigger = "hover"),
                              textInput("defens1a_text_1", NULL, placeholder = "Enter details..."),
                       ),
                       column(6,
                              div(style="display: flex; align-items: center; position: relative;",
                                  checkboxInput("defens1a_2", "Expertise", value = FALSE),
                                  tags$span(
                                    id = "tooltip_defens1a_2",
                                    class = "tooltip-icon",
                                    style = "margin-left: 10px; font-size: 14px; color: #007BFF; cursor: pointer; position: relative; top: -15px;",
                                    "?"
                                  )
                              ),
                              bsTooltip("tooltip_defens1a_2", "The individual researcher, or the collaboration of researchers identify alternative options or pipelines based on their expertise.", 
                                        placement = "right", trigger = "hover"),
                              textInput("defens1a_text_2", NULL, placeholder = "Enter details..."),
                       ),
                       column(6,
                              div(style="display: flex; align-items: center; position: relative;",
                                  checkboxInput("defens1a_3", "Crowdsourcing", value = FALSE),
                                  tags$span(
                                    id = "tooltip_defens1a_3",
                                    class = "tooltip-icon",
                                    style = "margin-left: 10px; font-size: 14px; color: #007BFF; cursor: pointer; position: relative; top: -15px;",
                                    "?"
                                  )
                              ),
                              bsTooltip("tooltip_defens1a_3", "Multiple experts contribute one defensible option or pipeline each.", 
                                        placement = "right", trigger = "hover"),
                              textInput("defens1a_text_3", NULL, placeholder = "Enter details..."),
                       ),
                       column(6,
                              div(style="display: flex; align-items: center; position: relative;",
                                  checkboxInput("defens1a_4", "Other", value = FALSE),
                                  tags$span(
                                    id = "tooltip_defens1a_4",
                                    class = "tooltip-icon",
                                    style = "margin-left: 10px; font-size: 14px; color: #007BFF; cursor: pointer; position: relative; top: -15px;",
                                    "?"
                                  )
                              ),
                              bsTooltip("tooltip_defens1a_4", "Please describe the alternative approach in the text box below.", 
                                        placement = "right", trigger = "hover"),
                              textInput("defens1a_text_4", NULL, placeholder = "Enter details..."),
                       ),
                       p("Please provide any additional details in the accompanying 
                        text box.", style="text-align: justify;", style = "font-size:20px"),
                       h3(strong("List Decision Nodes and Options to be considered")),
                       p("Using on text box for one decision node, list all decision 
                        nodes that will be considered in your multiverse analysis, 
                        regardless of defensibility. Please use the smaller text 
                        boxes underneath each decision node to list all options 
                        within that decision node, regardless of their defensibility. 
                        The full multiverse for defensibility consideration will be presented 
                        to you in the next step of the procedure.", style="text-align: justify;", style = "font-size:20px"),
                       div(id = "accordion_container_step1a"),
                       actionButton("add_step", "Add Node", class = "btn-primary", style = "margin-top: 10px;"),
                       actionButton("delete_step", "Delete Node", class = "btn-danger", style = "margin-top: 10px;")
          ),
          mainPanel(width = 7,
                    visNetworkOutput("pipeline_network_1a", height = "calc(100vh - 150px)")
          )
        )
      )
    ),
##############################################################
    
############### Step 1b: Define Defensibility ################
    tabPanel(
      "Step 1b: Define Defensibility",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            width = 5,
            div(id = "accordion_container_step1b")
          ),
          mainPanel(
            width = 7,
            br(),
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
            visNetworkOutput("pipeline_network_step1b", height = "calc(100vh - 150px)")
          )
        )
      )
    ),
##############################################################


############ Step 1c: Create Defensible Pipelines ############
    tabPanel(
      "Step 1c: Create Defensible Pipelines",
      mainPanel(
        width = 12,  # Take full width
        br(),
        p("Here you can see all of the decision nodes and options 
      that were categorized as defensible in the previous step.", 
          style = "text-align: justify;", style = "font-size:20px"),
        p("By dragging and dropping the defensible decision nodes 
      and options, please create sequences of defensible pipelines. 
      Please construct all plausible combinations. You do not 
      need to consider equivalence yet – only defensible combinations.", 
          style = "text-align: justify;", style = "font-size:20px"),
        p("To start a new pipeline, click 'New Pipeline' below.", 
          style = "text-align: justify;", style = "font-size:20px"),
        
        # Two-column layout (Pipeline Creation + Visualization)
        div(
          style = "display: flex; gap: 20px;",
          
          # COLUMN 1 - Pipeline Creation & Drag-and-Drop
          div(
            style = "width: 50%;",
            div(id = "pipeline_creation_container",
                div(id = "main_pipeline_accordion", class = "panel-group")  # ✅ Add this
            ),
            actionButton("new_pipeline", "New Pipeline", class = "btn-primary")
          ),
          
          # COLUMN 2 - Pipeline Visualization
          div(
            style = "width: 50%;",
            visNetworkOutput("pipeline_network_1c", height = "calc(100vh - 150px)")
          )
        )
      ),
      tags$script(HTML("
          $(document).on('click', '[id^=selected_steps_] .rank-list-item', function() {
              var selectedText = $(this).text().trim();
              console.log('Clicked:', selectedText); // ✅ Debugging
          Shiny.setInputValue('clicked_pipeline_item', selectedText, {priority: 'event'});
          });
      "))
    ),
##############################################################


################ Step 1d: Your Multiverse 1.0 ################
    tabPanel(
      "Step 1d: Your Multiverse 1.0",
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
            selectInput("selected_pipeline", "Choose a Pipeline:", choices = NULL, selected = NULL), 
            DT::dataTableOutput("pipeline_table")
          ),
          column(
            width = 6,
            visNetworkOutput("selected_pipeline_network", height = "calc(100vh - 150px)") 
          ),
          br(),
          div(
            style = "text-align: center; margin-top: 20px;",
            downloadButton("download_preprocessing_doc", "Download Preprocessing Documentation", class = "btn-primary"),
            downloadButton("download_construction_doc", "Download Construction", class = "btn-success")
          )
        )
      )
    )
##############################################################
  )
),
################################################################################

################################# Multiverse 2.0 ###############################
tabPanel(
  "Multiverse 2.0",
  navset_pill(
############# Step 2a: Criterion for Equivalance #############
    tabPanel(
      "Step 2a: Criterion for Equivalance",
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
                    uiOutput("equivalence_inputs_ui")
                  ),
                  column(
                    6,
                    h3("Visual Network", align = "center"),
                    visNetworkOutput("equivalence_pipeline_network", height = "calc(100vh - 150px)")
                  )
                ),
                 
      )
    ),
##############################################################

############ Step 2b: Your Principled Multiverse #############
    tabPanel(
      "Step 2b: Your Principled Multiverse",
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
                               Principled Multiverse PDF", class = "btn-primary")
      )
    )
##############################################################

  ),
),
################################################################################

###################################### About ###################################
tabPanel("About",
         column(1),
         column(10,
                shiny::HTML("<h3><center>Thank you for using the Systematic Multiverse 
                Analysis Registration Tool</center></h3><br><h2><center>Our team</center></h2><br>")
         ),
         
         # TEAM BIO
         fluidRow(
           
           style = "height:50px;"),
         
         fluidRow(
           column(2),
           
           # Andrea
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px", height = "800px",
                          align = "center",
                          div(
                            tags$img(src = "andrea2.jpg",
                                     width = "200px", height = "250px")
                          ),
                          div(
                            tags$h5("Hildebrandt, Andrea, Prof. Dr. rer. nat."),
                          ),
                          div(
                            "Professor for Psychological Methods and Statistics, Carl von Ossietzky Universität Oldenburg."
                          )
                      )
                  )
           ),
           # Cassie
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px", height = "800px",
                          align = "center",
                          div(
                            tags$img(src = "cassie.jpg",
                                     width = "200px", height = "250px")),
                          div(
                            tags$h5("Short, Cassie, PhD."),
                          ),
                          div(
                            "Carl von Ossietzky Universität Oldenburg."
                          )
                      )
                  )
           ),
           # Max
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px", height = "800px",
                          align = "center",
                          div(
                            tags$img(src = "max.jpg",
                                     width = "200px", height = "250px")),
                          div(
                            tags$h5("Frank, Maximilian, MsC."),
                          ),
                          div(
                            "Ludwig Maximilian Universität München."
                          )
                      )
                  )
           ),
           # Cosku
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px", height = "800px",
                          align = "center",
                          div(
                            tags$img(src = "cosku.jpg",
                                     width = "200px", height = "250px")),
                          div(
                            tags$h5("Inceler, Yusuf Cosku, BsC."),
                          ),
                          div(
                            "Carl von Ossietzky Universität Oldenburg."
                          )
                      )
                  )
           ),
           column(2),
           column(10,
                  shiny::HTML('<h3><center>For questions and suggestions, please 
                              contact us at: cassie.ann.short@uni-oldenburg.de</center></h3>
                              <h4><center>The code for this app is open for public access through 
                              <a href="https://github.com/cassiesh/MultiverseConstructionApp" target="_blank">Github</a></center></h4>')
                  
           ),
         ),
         fluidRow(style = "height:150px;")
)
################################################################################
)
