##### Load required packages ###################################################

library(magrittr)
library(multiarm)
library(rmarkdown)
library(shiny)
library(shinyalert)
library(shinycssloaders)
library(shinydashboard)
library(shinyFeedback)
library(shinyhelper)
library(shinyjs)
library(shinyWidgets)

##### UI #######################################################################
ui <- dashboardPage(
  ##### Dashboard: Header ######################################################
  dashboardHeader(title      = "multiarm",
                  titleWidth = 175),
  ##### Dashboard: Sidebar #####################################################
  dashboardSidebar(
    width = 175,
    sidebarMenu(
      menuItem(text    = "Home",
               tabName = "home",
               icon    = icon("home")),
      menuItem(text       = "Design",
               tabName    = "design",
               icon       = icon(name = "list-alt",
                                 lib  = "glyphicon"),
               badgeLabel = "new",
               badgeColor = "green"),
      menuItem(text    = "About",
               tabName = "about",
               icon    = icon(name = "question")),
      menuItem(text    = "Source code",
               icon    = icon(name = "file-code-o"),
               href    = "https://github.com/mjg211/multiarm/")
    )
  ),
  ##### Dashboard: Body ########################################################
  dashboardBody(
    tabItems(
      ##### Tab: Home ##########################################################
      tabItem(tabName = "home"),
      ##### Tab: Design ########################################################
      tabItem(tabName = "design",
              ### Row 1: Design parameters & Design summary ###
              fluidRow(
                box(useShinyjs(),
                    useShinyalert(),
                    useShinyFeedback(),
                    id          = "box_design_parameters",
                    title       = "Design parameters",
                    width       = 4,
                    solidHeader = T,
                    status      = "primary",
                    tags$style(type = "text/css",
                               ".irs-grid-pol.small {height: 0px;}"),
                    sliderInput(inputId = "design_K",
                                label   = paste0("Number of experimental ",
                                                 "treatment arms (\U1D43E):"),
                                min     = 2,
                                max     = 5,
                                value   = 2,
                                step    = 1) %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_K",
                             size    = "m",
                             colour  = "black"),
                    selectInput(inputId  = "design_correction",
                                label    = "Multiple comparison correction:",
                                choices  =
                                  list("Single-step" =
                                         list("Bonferroni"      = "bonferroni",
                                              "Dunnett"         = "dunnett",
                                              "None"            = "none",
                                              "\U0160id\U00E1k" = "sidak"),
                                       "Step-wise"   =
                                         list("Benjamini-Hochberg" =
                                                "benjamini_hochberg",
                                              "Hochberg"           = "hochberg",
                                              "Holm"               = "holm",
                                              "Step-down Dunnett"  =
                                                "step_down_dunnett")),
                                selected = "dunnett") %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_correction",
                             size    = "m",
                             colour  = "black"),
                    numericInput(inputId = "design_alpha",
                                 label   = "Significance level (\U1D6FC):",
                                 value   = 0.05,
                                 min     = 0,
                                 max     = 1,
                                 step    = 0.01) %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_alpha",
                             size    = "m",
                             colour  = "black"),
                    selectInput(inputId = "design_power",
                                label   = "Type of power to control:",
                                choices = c("Conjunctive" = "conjunctive",
                                            "Disjunctive" = "disjunctive",
                                            "Marginal"    = "marginal"),
                                selected = "marginal")  %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_power",
                             size    = "m",
                             colour  = "black"),
                    numericInput(inputId = "design_beta",
                                 label   = "Desired power (1 \U2212 \U1D6FD):",
                                 value   = 0.8,
                                 min     = 0,
                                 max     = 1,
                                 step    = 0.025)  %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_beta",
                             size    = "m",
                             colour  = "black"),
                    numericInput(inputId = "design_delta1",
                                 label   = paste0("Interesting treatment ",
                                                  "effect (\U1D6FF\U2081):"),
                                 value   = 0.5,
                                 min     = 0,
                                 max     = NA,
                                 step    = 0.1) %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_delta1",
                             size    = "m",
                             colour  = "black"),
                    uiOutput("outputNumericInput_design_delta0"),
                    selectInput(inputId = "design_sigma_type",
                                label   = "Standard deviations:",
                                choices = c("Equal across all arms"          =
                                              "equal_all",
                                            "Equal across experimental arms" =
                                              "equal_exp",
                                            "Unequal across all arms"        =
                                              "unequal"),
                                selected = "equal_all")  %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_sigma_type",
                             size    = "m",
                             colour  = "black"),
                    uiOutput("outputNumericInput_design_sigma"),
                    selectInput(
                      inputId = "design_ratio_type",
                      label   = "Allocation ratios:",
                      choices =
                        list("Explicit" =
                               list("Equal across all arms"          =
                                      "equal_all",
                                    "Equal across experimental arms" =
                                      "equal_exp",
                                    "Unequal across all arms"        =
                                      "unequal",
                                    "\U221A\U1D43E-rule"             =
                                      "root_K"),
                             "Implicit" = list("\U1D434-optimal" = "A",
                                               "\U1D437-optimal" = "D",
                                               "\U1D438-optimal" = "E")),
                      selected = "equal_all")  %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_ratio_type",
                             size    = "m",
                             colour  = "black"),
                    uiOutput("outputNumericInput_design_ratio"),
                    prettySwitch(inputId = "design_integer",
                                 label   = "Require integer sample sizes",
                                 status  = "info",
                                 value   = F,
                                 slim    = T)  %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_integer",
                             size    = "m",
                             colour  = "black"),
                    prettySwitch(inputId = "design_plots",
                                 label   = "Plot power curves",
                                 status  = "info",
                                 value   = T,
                                 slim    = T) %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_plots",
                             size    = "m",
                             colour  = "black"),
                    uiOutput("outputSelectInput_design_density"),
                    uiOutput("outputNumericInput_design_delta"),
                    hr(),
                    actionButton(inputId = "design_reset",
                                 label   = "\U2000 Reset inputs \U2000",
                                 icon    = icon(name = "eraser"),
                                 width   = "100%"),
                    hr(),
                    uiOutput("outputWarning_design"),
                    actionButton(inputId = "design_update",
                                 label   = "\U2000 Update outputs \U2000",
                                 icon    = icon(name = "check-square-o"),
                                 width   = "100%"),
                    hr(),
                    textInput(inputId = "design_filename",
                              label   = "Report filename:",
                              value   = "multiarm_design")  %>%
                      helper(type    = "markdown",
                             title   = "",
                             content = "design_filename",
                             size    = "m",
                             colour  = "black"),
                    tags$head(tags$style(".full_width{width:100%;}")),
                    downloadButton(outputId = "design_report_html",
                                   label    =
                                     "\U2000 Download report (.html) \U2000",
                                   class    = "full_width"),
                    p(),
                    downloadButton(outputId = "design_report_pdf",
                                   label    =
                                     "\U2000 Download report (.pdf) \U2000",
                                   class    = "full_width")
                ),
                box(title       = "Design summary",
                    width       = 8,
                    solidHeader = T,
                    status      = "primary",
                    withSpinner(uiOutput("design_summary"),
                                type  = 6,
                                color = "#3C8DBC",
                                size  = 1/3)
                )
              ),
              fluidRow(
                valueBoxOutput("design_ss_box"),
                valueBoxOutput("design_fwer_box"),
                valueBoxOutput("design_power_box")
              ),
              fluidRow(
                box(title       = "Operating characteristics summary",
                    width       = 12,
                    solidHeader = T,
                    collapsible = T,
                    status      = "primary",
                    column(width = 12,
                           align = "center",
                           withSpinner(tableOutput('summary'),
                                       type  = 6,
                                       color = "#3C8DBC",
                                       size  = 1/3))
                )
              ),
              fluidRow(
                box(title       = "Equal treatment effects",
                    width       = 6,
                    solidHeader = T,
                    collapsible = T,
                    status      = "primary",
                    withSpinner(plotOutput("plot_global",
                                           dblclick = "plot_global_dblclick",
                                           brush    =
                                             brushOpts(
                                               id         = "plot_global_brush",
                                               resetOnNew = T
                                             )
                    ),
                    type  = 6,
                    color = "#3C8DBC",
                    size  = 1/3)
                ),
                box(title       = "Least favourable configurations",
                    width       = 6,
                    solidHeader = T,
                    collapsible = T,
                    status      = "primary",
                    withSpinner(plotOutput("plot_LFC",
                                           dblclick = "plot_LFC_dblclick",
                                           brush    = brushOpts(
                                             id = "plot_LFC_brush",
                                             resetOnNew = TRUE
                                           )
                    ),
                    type  = 6,
                    color = "#3C8DBC",
                    size  = 1/3)
                )
              ),
              fluidRow(
                box(title       = "Session Information",
                    status      = "primary",
                    solidHeader = T,
                    width       = 12,
                    collapsible = T,
                    collapsed   = T,
                    verbatimTextOutput("design_debug")
                )
              )
      ),
      tabItem(tabName = "about")
    )
  ),
  title = "multiarm",
  skin  = "blue"
)

server <- function(input, output, session) {

  observe_helpers(withMathJax = T)
  ranges_global <- reactiveValues(x = NULL, y = NULL)
  ranges_LFC    <- reactiveValues(x = NULL, y = NULL)

  ##### Reactive warning messages ##############################################

  observeEvent(input$design_alpha, {
    feedbackDanger(inputId   = "design_alpha",
                   condition = any(input$design_alpha <= 0,
                                   input$design_alpha >= 1),
                   text      = "\U1D6FC must be strictly between 0 and 1"
    )
  })

  observeEvent(input$design_beta, {
    feedbackDanger(inputId   = "design_beta",
                   condition = any(input$design_beta <= 0,
                                   input$design_beta >= 1),
                   text      = "Desired power must be strictly between 0 and 1"
    )
  })

  observeEvent(input$design_delta1, {
    feedbackDanger(inputId   = "design_delta1",
                   condition = (input$design_delta1 <= 0),
                   text      = "\U1D6FF\U2081 must be strictly positive"
    )
  })

  observeEvent(input$design_delta0, {
    feedbackDanger(inputId   = "design_delta0",
                   condition = (input$design_delta0 >= input$design_delta1),
                   text      = paste0("\U1D6FF\U2080 must be strictly smaller ",
                                      " than \U1D6FF\U2081")
    )
  })

  observeEvent(input$design_sigma, {
    feedbackDanger(inputId   = "design_sigma",
                   condition = (input$design_sigma <= 0),
                   text      = "Standard deviations must be strictly positive"
    )
  })

  observeEvent(input$design_sigma_0, {
    feedbackDanger(inputId   = "design_sigma_0",
                   condition = (input$design_sigma_0 <= 0),
                   text      = "Standard deviations must be strictly positive"
    )
  })

  observeEvent(c(input$design_sigma_1, input$design_sigma_2,
                 input$design_sigma_3,
                 input$design_sigma_4,
                 input$design_sigma_5), {
                   vals <- c(input$design_sigma_1, input$design_sigma_2, input$design_sigma_3,
                             input$design_sigma_4, input$design_sigma_5)
                   for (i in 1:5) {
                     feedbackDanger(inputId   = paste0("design_sigma_", i),
                                    condition = (vals[i] <= 0),
                                    text      = "Standard deviations must be strictly positive"
                     )
                   }
                 })

  observeEvent(c(input$design_ratio_1, input$design_ratio_2,
                 input$design_ratio_3, input$design_ratio_4,
                 input$design_ratio_5), {
                   vals <- c(input$design_ratio_1, input$design_ratio_2, input$design_ratio_3,
                             input$design_ratio_4, input$design_ratio_5)
                   for (i in 1:5) {
                     feedbackDanger(inputId   = paste0("design_ratio_", i),
                                    condition = (vals[i] <= 0),
                                    text      = "Allocation ratios must be strictly positive"
                     )
                   }
                 })

  observeEvent(input$design_filename, {
    feedbackWarning(
      inputId   = "design_filename",
      condition = any(strsplit(input$design_filename, split = "")[[1]] %in%
                        c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text      = paste0('It is generally inadvisable to use the characters /',
                         ', \\, ?, %, *, :, |, ", <, and > in a filename')
    )
  })

  observeEvent(input$design_delta, {
    feedbackDanger(inputId   = "design_delta",
                   condition = (input$design_delta <= 0),
                   text      = "\U1D6FF must be strictly positive"
    )
  })

  output$outputNumericInput_design_delta0 <- renderUI({
    if (input$design_K > 1) {
      numericInput(inputId = "design_delta0",
                   label   = "Uninteresting treatment effect (\U1D6FF\U2080):",
                   value   = 0,
                   min     = NA,
                   max     = input$design_delta1,
                   step    = 0.1)  %>%
        helper(type    = "markdown",
               title   = "",
               content = "design_delta0",
               size    = "m",
               colour  = "black")
    }
  })

  output$outputNumericInput_design_sigma <- renderUI({
    subscripts             <- c("\U2081", "\U2082", "\U2083", "\U2084",
                                "\U2085")
    if (input$design_sigma_type == "equal_all") {
      if (input$design_K > 1) {
        label_design_sigma <-
          paste0("Standard deviation of the responses (\U1D70E\U2080 = \U22EF ",
                 "= \U1D70E", subscripts[input$design_K], "):")
      } else {
        label_design_sigma <-
          "Standard deviation of the responses (\U1D70E\U2080 = \U1D70E\U2081):"
      }
      numericInput(inputId = "design_sigma",
                   label   = label_design_sigma,
                   value   = 1,
                   min     = 0,
                   max     = NA,
                   step    = 0.1)
    } else if (input$design_sigma_type == "equal_exp") {
      if (input$design_K == 1) {
        label_sigma_1      <-
          "Standard deviation of the experimental responses (\U1D70E\U2081):"
      } else if (input$design_K == 2) {
        label_sigma_1      <-
          paste0("Standard deviation of the experimental responses (\U1D70E",
                 "\U2081 = \U1D70E\U2082):")
      } else {
        label_sigma_1      <-
          paste0("Standard deviation of the experimental responses (\U1D70E",
                 "\U2081 = \U22EF = \U1D70E", subscripts[input$design_K], "):")
      }
      tagList(numericInput(inputId = "design_sigma_0",
                           label   = paste0("Standard deviation of the control",
                                            " responses (\U1D70E\U2080):"),
                           value   = 1,
                           min     = 0,
                           max     = NA,
                           step    = 0.1),
              numericInput(inputId = "design_sigma_1",
                           label   = label_sigma_1,
                           value   = 1,
                           min     = 0,
                           max     = NA,
                           step    = 0.1))
    } else {
      inputTagList         <-
        tagList(numericInput(inputId = "design_sigma_0",
                             label   =
                               paste0("Standard deviation of the control ",
                                      "responses (\U1D70E\U2080):"),
                             value   = 1,
                             min     = 0,
                             max     = NA,
                             step    = 0.1))
      lapply(1:input$design_K, function(i) {
        newInput           <-
          numericInput(inputId = paste0("design_sigma_", i),
                       label   = paste0("Standard deviation of experimental ",
                                        "arm ", i, " responses (\U1D70E",
                                        subscripts[i], "):"),
                       value   = 1,
                       min     = 0,
                       max     = NA,
                       step    = 0.1)
        inputTagList       <<- tagAppendChild(inputTagList, newInput)
      })
      inputTagList
    }
  })

  output$outputNumericInput_design_ratio <- renderUI({
    subscripts             <- c("\U2081", "\U2082", "\U2083", "\U2084",
                                "\U2085")
    if (input$design_ratio_type == "equal_exp") {
      if (input$design_K == 1) {
        label_ratio_1      <-
          "Allocation ratio for the experimental arm (\U1D45F\U2081):"
      } else if (input$design_K == 2) {
        label_ratio_1      <-
          paste0("Allocation ratio for the experimental arms (\U1D45F",
                 "\U2081 = \U1D45F\U2082):")
      } else {
        label_ratio_1      <-
          paste0("Allocation ratio for the experimental arms (\U1D45F",
                 "\U2081 = \U22EF = \U1D45F", subscripts[input$design_K], "):")
      }
      numericInput(inputId = "design_ratio_1",
                   label   = label_ratio_1,
                   value   = 1,
                   min     = 0,
                   max     = NA,
                   step    = 0.25)
    } else if (input$design_ratio_type == "unequal") {
      inputTagList         <-
        tagList(numericInput(inputId = "design_ratio_1",
                             label   =
                               paste0("Allocation ratio for experimental arm 1 (\U1D45F\U2081):"),
                             value   = 1,
                             min     = 0,
                             max     = NA,
                             step    = 0.25))
      if (input$design_K > 1) {
        lapply(2:input$design_K, function(i) {
          newInput           <-
            numericInput(inputId = paste0("design_ratio_", i),
                         label   = paste0("Allocation ratio for experimental ",
                                          "arm ", i, " (\U1D45F",
                                          subscripts[i], "):"),
                         value   = 1,
                         min     = 0,
                         max     = NA,
                         step    = 0.25)
          inputTagList       <<- tagAppendChild(inputTagList, newInput)
        })
      }
      inputTagList
    }
  })

  output$outputWarning_design <- renderUI({
    if (any(all(input$design_K %in% c(4, 5),
                input$design_correction %in% c("benjamini_hochberg", "hochberg",
                                               "holm", "step_down_dunnett")),
            all(input$design_K == 5, input$design_plots))) {
      p(strong("WARNING:"), " Execution time may be long for chosen input ",
        "parameters.")
    }
  })

  output$outputSelectInput_design_density <- renderUI({
    if (input$design_plots) {
      selectInput(inputId = "design_density",
                  label   = "Plot quality:",
                  choices = c("Very low" = 33, "Low" = 66,
                              "Medium" = 100, "High" = 150, "Very high" = 200),
                  selected = 100)  %>%
        helper(type    = "markdown",
               title   = "",
               content = "design_density",
               size    = "m",
               colour  = "black")
    }
  })

  output$outputNumericInput_design_delta <- renderUI({
    if (input$design_plots) {
      numericInput(inputId = "design_delta",
                   label   = "Plot treatment effect shift (\U1D6FF):",
                   value   = input$design_delta1 - input$design_delta0,
                   min     = 0,
                   max     = NA,
                   step    = 0.1)  %>%
        helper(type    = "markdown",
               title   = "",
               content = "design_delta",
               size    = "m",
               colour  = "black")
    }
  })

  observeEvent(input$plot_global_dblclick, {
    brush <- input$plot_global_brush
    if (!is.null(brush)) {
      ranges_global$x <- c(brush$xmin, brush$xmax)
      ranges_global$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges_global$x <- NULL
      ranges_global$y <- NULL
    }
  })

  observeEvent(input$plot_LFC_dblclick, {
    brush <- input$plot_LFC_brush
    if (!is.null(brush)) {
      ranges_LFC$x <- c(brush$xmin, brush$xmax)
      ranges_LFC$y <- c(brush$ymin, brush$ymax)

    } else {
      ranges_LFC$x <- NULL
      ranges_LFC$y <- NULL
    }
  })

  des <- eventReactive(input$design_update, {

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs",
                 value   = 0)

    if (input$design_sigma_type == "equal_all") {
      sigma <- rep(input$design_sigma, input$design_K + 1)
    } else if (input$design_sigma_type == "equal_exp") {
      sigma <- c(input$design_sigma_0,
                 rep(input$design_sigma_1, input$design_K))
    } else if (input$design_sigma_type == "unequal") {
      sigma   <- NULL
      for (i in 0:input$design_K) {
        sigma <- c(sigma, input[[paste0("design_sigma_", i)]])
      }
    }
    if (input$design_ratio_type == "equal_all") {
      ratio   <- rep(1, input$design_K)
    } else if (input$design_ratio_type == "equal_exp") {
      ratio   <- rep(input$design_ratio_1, input$design_K)
    } else if (input$design_ratio_type == "unequal") {
      ratio   <- NULL
      for (i in 1:input$design_K) {
        ratio <- c(ratio, input[[paste0("design_ratio_", i)]])
      }
    } else if (input$design_ratio_type == "root_K") {
      ratio   <- rep(1/sqrt(input$design_K), input$design_K)
    } else {
      ratio   <- input$design_ratio_type
    }
    print(input$design_correction)
    design  <- multiarm::des_ma(K          = input$design_K,
                                alpha      = input$design_alpha,
                                beta       = 1 - input$design_beta,
                                delta1     = input$design_delta1,
                                delta0     = input$design_delta0,
                                sigma      = sigma,
                                ratio      = ratio,
                                correction = input$design_correction,
                                power      = input$design_power,
                                integer    = input$design_integer)

    progress$inc(amount  = 0.25 + as.numeric(!input$design_plots),
                 message = "Rendering design summary")

    rmarkdown::render("design_summary.Rmd",
                      output_file = "design_summary.html",
                      params = list(K          = design$K,
                                    alpha      = design$alpha,
                                    beta       = design$beta,
                                    delta1     = design$delta1,
                                    delta0     = design$delta0,
                                    sigma      = design$sigma,
                                    ratio_type = input$design_ratio_type,
                                    ratio_init = c(input$design_ratio_1,
                                                   input$design_ratio_2,
                                                   input$design_ratio_3,
                                                   input$design_ratio_4,
                                                   input$design_ratio_5),
                                    ratio      = design$ratio,
                                    correction = design$correction,
                                    power      = design$power,
                                    integer    = design$integer,
                                    large_N    = design$N,
                                    small_n    = design$n,
                                    opchar     = design$opchar,
                                    pi        = design$pi,
                                    piO        = design$piO)
    )

    design$data_og <- design$opchar
    design$data <- data.frame(design$opchar,
                              row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                                            paste0("<i>LFC<sub>", 1:input$design_K,
                                                   "</sub></i>")))
    colnames(design$data) <-
      c(paste0("<i>&tau;</i><sub>", 1:input$design_K, "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", 1:input$design_K),
               "</sub>"),
        "<i>FWER</i>", "<i>FDR</i>")


    if (input$design_plots) {
      print(class(design))
      progress$inc(amount  = 0.25,
                   message = "Rendering plots")
      tau                <- matrix(0, as.numeric(input$design_density),
                                   design$K)
      tau[, 1]           <- c(seq(-design$delta1, -1e-6,
                                length.out =
                                  0.5*as.numeric(input$design_density)),
                              seq(1e-6, 2*design$delta1,
                                  length.out =
                                    0.5*as.numeric(input$design_density)))
      for (k in 2:design$K) {
        tau[, k]         <- tau[, 1]
      }
      opchar_global      <- opchar_ma(design, tau)$opchar
      opchar_global      <- tidyr::gather(opchar_global, "type", "P",
                                          `Pdis`:`FWER`)
      opchar_global$type <- factor(opchar_global$type,
                                   levels = c(paste("P", 1:design$K, sep = ""),
                                              "Pcon", "Pdis", "FWER"))
      labels             <- numeric(design$K + 3L)
      labels[1]          <- parse(text = "italic(FWER)/italic(FDR)")
      for (i in 2:(design$K + 1L)) {
        labels[i]        <- parse(text = paste0("italic(P)[", i - 1L, "]"))
      }
      labels[(design$K + 2L):(design$K + 3L)] <- c(parse(text = "italic(P)[con]"),
                                                   parse(text = "italic(P)[dis]"))
      alpha              <- design$alpha
      beta               <- design$beta
      delta0             <- design$delta0
      delta1             <- design$delta1
      design$plot_global <- ggplot2::ggplot() +
        ggplot2::geom_line(data = dplyr::filter(opchar_global,
                                                !(type %in% c("FWER"))),
                           ggplot2::aes(x   = tau1,
                                        y   = P,
                                        col = type)) +
        ggplot2::geom_line(data = dplyr::filter(opchar_global,
                                                (type %in% c("FWER")) &
                                                  tau1 <= 0),
                           ggplot2::aes(x   = tau1,
                                        y   = P,
                                        col = type)) +
        ggplot2::geom_line(data = dplyr::filter(opchar_global,
                                                (type %in% c("FWER")) &
                                                  tau1 > 0),
                           ggplot2::aes(x   = tau1,
                                        y   = P,
                                        col = type)) +
        ggthemes::scale_color_ptol(labels = labels) +
        ggplot2::xlab(expression(paste(tau[1], " = \u00B7\u00B7\u00B7 = ",
                                       tau[K], sep = ""))) +
        ggplot2::ylab("Probability/Rate") +
        ggplot2::theme(legend.position  = "bottom",
                       legend.title     = ggplot2::element_blank(),
                       legend.spacing.x = grid::unit(0.2, "cm"),
                       axis.text        = ggplot2::element_text(size = 11),
                       legend.text      = ggplot2::element_text(size = 12),
                       axis.title       = ggplot2::element_text(size = 14),
                       plot.margin      = grid::unit(rep(0.25, 4), "cm")) +
        ggplot2::geom_hline(yintercept = alpha,
                            linetype   = 2) +
        ggplot2::geom_hline(yintercept = 1 - beta,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = delta0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = delta1,
                            linetype   = 2)

      progress$inc(amount  = 0.25,
                   message = "Rendering plots")

      opchar_matrix    <- NULL
      for (k in 1:design$K) {
        tau                <- matrix(0, as.numeric(input$design_density),
                                     design$K)
        tau[, k]           <- seq(-design$delta1, 2*design$delta1,
                                  length.out =
                                    as.numeric(input$design_density))
        for (l in (1:design$K)[-k]) {
          tau[, l]     <- tau[, k] - input$design_delta
        }
        opchar_k       <- opchar_ma(design, tau)$opchar
        opchar_matrix  <- rbind(opchar_matrix,
                                as.matrix(opchar_k[, c(k, input$design_K + 2 + k)]))
        progress$inc(amount  = 0.25/(design$K + 1),
                     message = "Rendering plots")
      }
      opchar_LFC       <- tibble::as_tibble(opchar_matrix)
      colnames(opchar_LFC) <- c("tauk", "P")
      opchar_LFC       <-
        dplyr::mutate(opchar_LFC, type = factor(rep(paste("P", 1:design$K, sep = ""),
                                                    each = as.numeric(input$design_density))))
      labels <- numeric(design$K)
      for (i in 1:design$K) {
        labels[i] <- parse(text = paste0("italic(P)[", i, "]"))
      }
      design$plot_LFC <- ggplot2::ggplot() +
        ggplot2::geom_line(data = opchar_LFC,
                           ggplot2::aes(x   = .data$tauk,
                                        y   = P,
                                        col = type)) +
        ggthemes::scale_color_ptol(labels = labels) +
        ggplot2::xlab(bquote(paste(tau[1], " + ", delta,
                                   " = \u00B7\u00B7\u00B7 = ", tau[k - 1],
                                   " + ", delta, " = ", tau[k], " = ",
                                   tau[k + 1], " + ", delta,
                                   " = \u00B7\u00B7\u00B7 = ", tau[K], " + ",
                                   delta, ", ", delta, " = ", .(delta),
                                   sep = ""))) +
        ggplot2::ylab("Probability") +
        ggplot2::theme(legend.position  = "bottom",
                       legend.title     = ggplot2::element_blank(),
                       legend.spacing.x = grid::unit(0.2, "cm"),
                       axis.text        = ggplot2::element_text(size = 11),
                       legend.text      = ggplot2::element_text(size = 12),
                       axis.title       = ggplot2::element_text(size = 14),
                       plot.margin      = grid::unit(rep(0.25, 4), "cm")) +
        ggplot2::geom_hline(yintercept = alpha,
                            linetype   = 2) +
        ggplot2::geom_hline(yintercept = 1 - beta,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = delta0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = delta1,
                            linetype   = 2)
    } else {
      design$plot_global <- design$plot_LFC <- NULL
    }
    progress$inc(amount  = 0.25 + as.numeric(!input$design_plots),
                 message = "Outputting results")
    design
  })

  output$design_ss_box <- renderValueBox({
    input$design_update
    valueBox(value    = round(des()$N, 1),
             subtitle = "Total required sample size",
             icon     = icon(name = "users"),
             color    = "light-blue"
    )
  })

  output$design_fwer_box <- renderValueBox({
    input$design_update
    correction      <- isolate(input$design_correction)
    if (!(correction %in% c("benjamini_hochberg", "none"))) {
      if (des()$opchar$FWER[1] <= isolate(input$design_alpha) + 1e-4) {
        icon_choice <- "thumbs-up"
      } else {
        icon_choice <- "thumbs-down"
      }
    } else {
      icon_choice   <- ""
    }
    valueBox(value    = round(des()$opchar$FWER[1], 3),
             subtitle = "Maximum FWER",
             icon     = icon(name = icon_choice),
             color    = "light-blue"
    )
  })

  output$design_power_box <- renderValueBox({
    input$design_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    = "Minimum marginal power")[isolate(input$design_power)]
    K                 <- isolate(input$design_K)
    if (input$design_power == "conjunctive") {
      value_power_box <- des()$opchar[2, 4]
    } else if (input$design_power == "disjunctive") {
      value_power_box <- des()$opchar[2, 3]
    } else {
      value_power_box <-
        min(diag(as.matrix(des()$opchar[-(1:2), (K + 3):(2*K + 2)])))
    }
    if (value_power_box >= isolate(input$design_beta) - 1e-3) {
      icon_choice     <- "thumbs-up"
    } else {
      icon_choice     <- "thumbs-down"
    }
    valueBox(value    = round(value_power_box, 3),
             subtitle = subtitle,
             icon     = icon(name = icon_choice),
             color    = "light-blue"
    )
  })

  output$design_summary <- renderText({
    input$design_update
    N <- des()$N
    includeHTML("design_summary.html")
  })


  output$summary <- renderTable({
    input$design_update
    des()$data
  }, rownames = T, sanitize.text.function = function(x) x, digits = 4,
  options = list(searching  = F,
                 scrollX    = T,
                 autoWidth  = T,
                 columnDefs = list(list(width = '50px')),
                 paging = F))

  output$plot_global <- renderPlot({
    input$design_update
    if (isolate(input$design_plots)) {
      des()$plot_global + ggplot2::coord_cartesian(xlim = ranges_global$x,
                                                   ylim = ranges_global$y,
                                                   expand = F)
    }
  })

  output$plot_LFC <- renderPlot({
    input$design_update
    if (isolate(input$design_plots)) {
      des()$plot_LFC + ggplot2::coord_cartesian(xlim = ranges_LFC$x,
                                                ylim = ranges_LFC$y,
                                                expand = F)
    }
  })

  observeEvent(input$design_reset, {
    shinyjs::reset("box_design_parameters")
  })

  output$design_report_html <- downloadHandler(
    filename = paste0(input$design_filename, ".html"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "design_report_html.Rmd")
      file.copy("design_report_html.Rmd", tempReport, overwrite = T)
      params <- list(K          = des()$K,
                     alpha      = des()$alpha,
                     beta       = des()$beta,
                     delta1     = des()$delta1,
                     delta0     = des()$delta0,
                     sigma      = des()$sigma,
                     ratio_type = input$design_ratio_type,
                     ratio_init = c(input$design_ratio_1,
                                    input$design_ratio_2,
                                    input$design_ratio_3,
                                    input$design_ratio_4,
                                    input$design_ratio_5),
                     ratio      = des()$ratio,
                     correction = des()$correction,
                     power      = des()$power,
                     integer    = des()$integer,
                     large_N    = des()$N,
                     small_n    = des()$n,
                     opchar     = des()$opchar,
                     pi        = des()$pi,
                     piO        = des()$piO,
                     plots       = input$design_plots,
                     plot_global = des()$plot_global,
                     plot_LFC = des()$plot_LFC,
                     data = des()$data_og)
       rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$design_report_pdf <- downloadHandler(
    filename = paste0(input$design_filename, ".pdf"),
    content = function(file) {
      tempReport <- file.path(tempdir(), "design_report_pdf.Rmd")
      file.copy("design_report_pdf.Rmd", tempReport, overwrite = T)
      params <- list(K          = des()$K,
                     alpha      = des()$alpha,
                     beta       = des()$beta,
                     delta1     = des()$delta1,
                     delta0     = des()$delta0,
                     sigma      = des()$sigma,
                     ratio_type = input$design_ratio_type,
                     ratio_init = c(input$design_ratio_1,
                                    input$design_ratio_2,
                                    input$design_ratio_3,
                                    input$design_ratio_4,
                                    input$design_ratio_5),
                     ratio      = des()$ratio,
                     correction = des()$correction,
                     power      = des()$power,
                     integer    = des()$integer,
                     large_N    = des()$N,
                     small_n    = des()$n,
                     opchar     = des()$opchar,
                     pi        = des()$pi,
                     piO        = des()$piO,
                     plots       = input$design_plots,
                     plot_global = des()$plot_global,
                     plot_LFC = des()$plot_LFC,
                     data = des()$data_og)
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$design_debug <- renderPrint({
    sessionInfo()
  })

  session$onSessionEnded(stopApp)

}

shinyApp(ui, server)
