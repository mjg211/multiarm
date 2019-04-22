##### Load required packages ###################################################

library(multiarm)
options(shiny.sanitize.errors = T)

##### UI #######################################################################
ui <- shinydashboard::dashboardPage(
  ##### Dashboard: Header ######################################################
  shinydashboard::dashboardHeader(
    title      = "multiarm",
    titleWidth = 175),
  ##### Dashboard: Sidebar #####################################################
  shinydashboard::dashboardSidebar(
    width = 175,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        text    = "Home",
        tabName = "home",
        icon    = shiny::icon(name = "home")),
      shinydashboard::menuItem(
        text       = "Design",
        tabName    = "design",
        icon       = shiny::icon(name = "list-alt",
                                 lib  = "glyphicon"),
        badgeLabel = "new",
        badgeColor = "green"),
      shinydashboard::menuItem(
        text    = "About",
        tabName = "about",
        icon    = shiny::icon(name = "question")),
      shinydashboard::menuItem(
        text    = "Source code",
        icon    = shiny::icon(name = "file-code-o"),
        href    = "https://github.com/mjg211/multiarm/")
    )
  ),
  ##### Dashboard: Body ########################################################
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      ##### Tab: Home ##########################################################
      shinydashboard::tabItem(
        tabName = "home",
        h1(strong("multiarm:"),
           "Design and analysis of fixed-sample multi-arm clinical trials"),
        p("Welcome to the R Shiny graphical user interface (GUI) to the R ",
          "package multiarm, which is currently available from:"),
        a(href = "https://github.com/mjg211/multiarm",
          "https://github.com/mjg211/multiarm"),
        p(""),
        p("Within R, multiarm provides functionality to assist with the design",
          "and analysis of fixed-sample multi-arm clinical trial utilising one",
          "of several supported multiple comparison corrections. Available",
          "functions allow for sample size determination (including for A-,",
          "D-, and E-optimal designs), trial simulation, analytical operating",
          "characteristic calculation (including the conjunctive power,",
          "disjunctive power, family-wise error-rate, and false discovery",
          "rate), and the production of several plots."),
        p("At present, this GUI supports execution of the commands for design",
          " determination and plot production. Additional functionality will",
          "be added over time."),
        p("See the 'Design' tab on the sidebar for code execution, or the",
          "'About' tab for further information on the GUI.")
      ),
      ##### Tab: Design ########################################################
      shinydashboard::tabItem(
        tabName = "design",
        ##### Row 1: Design parameters & Design summary ########################
        shiny::fluidRow(
          shinydashboard::box(
            shinyjs::useShinyjs(),
            shinyalert::useShinyalert(),
            shinyFeedback::useShinyFeedback(),
            shiny::withMathJax(),
            id          = "box_design_parameters",
            title       = "Design parameters",
            width       = 4,
            solidHeader = T,
            status      = "primary",
            tags$style(type = "text/css",
                       ".irs-grid-pol.small {height: 0px;}"),
            shiny::sliderInput(
              inputId = "design_K",
              label   = "Number of experimental treatment arms:",
              min     = 2,
              max     = 5,
              value   = 2,
              step    = 1) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_K",
                size    = "m",
                colour  = "black"),
            shiny::selectInput(
              inputId  = "design_correction",
              label    = "Multiple comparison correction:",
              choices  =
                list("Single-step" =
                       list("Bonferroni"      = "bonferroni",
                            "Dunnett"         = "dunnett",
                            "None"            = "none",
                            "Sidak"           = "sidak"),
                     "Step-wise"   =
                       list("Benjamini-Hochberg" = "benjamini_hochberg",
                            "Hochberg"           = "hochberg",
                            "Holm"               = "holm",
                            "Step-down Dunnett"  = "step_down_dunnett")),
              selected = "dunnett") %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_correction",
                size    = "m",
                colour  = "black"),
            shiny::numericInput(
              inputId = "design_alpha",
              label   = "Significance level:",
              value   = 0.05,
              min     = 0,
              max     = 1,
              step    = 0.01) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_alpha",
                size    = "m",
                colour  = "black"),
            shiny::selectInput(
              inputId = "design_power",
              label   = "Type of power to control:",
              choices = c("Conjunctive" = "conjunctive",
                          "Disjunctive" = "disjunctive",
                          "Marginal"    = "marginal"),
              selected = "marginal") %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_power",
                size    = "m",
                colour  = "black"),
            shiny::numericInput(
              inputId = "design_beta",
              label   = "Desired power:",
              value   = 0.8,
              min     = 0,
              max     = 1,
              step    = 0.025) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_beta",
                size    = "m",
                colour  = "black"),
            shiny::numericInput(
              inputId = "design_delta1",
              label   = "Interesting treatment effect:",
              value   = 0.5,
              min     = 0,
              max     = NA,
              step    = 0.1) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_delta1",
                size    = "m",
                colour  = "black"),
            shiny::uiOutput("design_delta0"),
            shiny::selectInput(
              inputId = "design_sigma_type",
              label   = "Standard deviations:",
              choices = c("Equal across all arms"          = "equal_all",
                          "Equal across experimental arms" = "equal_exp",
                          "Unequal across all arms"        = "unequal"),
              selected = "equal_all") %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_sigma_type",
                size    = "m",
                colour  = "black"),
            shiny::uiOutput("design_sigma"),
            shiny::selectInput(
              inputId = "design_ratio_type",
              label   = "Allocation ratios:",
              choices =
                list("Explicit" =
                       list("Equal across all arms"          = "equal_all",
                            "Equal across experimental arms" = "equal_exp",
                            "Unequal across all arms"        = "unequal",
                            "root-K rule"                    = "root_K"),
                     "Implicit" = list("A-optimal" = "A",
                                       "D-optimal" = "D",
                                       "D-optimal" = "E")),
              selected = "equal_all") %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_ratio_type",
                size    = "m",
                colour  = "black"),
            shiny::uiOutput("design_ratio"),
            shinyWidgets::prettySwitch(
              inputId = "design_integer",
              label   = "Require integer sample sizes",
              status  = "info",
              value   = F,
              slim    = T) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_integer",
                size    = "m",
                colour  = "black"),
            shinyWidgets::prettySwitch(
              inputId = "design_plots",
              label   = "Plot power curves",
              status  = "info",
              value   = T,
              slim    = T) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_plots",
                size    = "m",
                colour  = "black"),
            shiny::uiOutput("design_density"),
            shiny::hr(),
            shiny::actionButton(
              inputId = "design_reset",
              label   = " Reset inputs  ",
              icon    = shiny::icon(name = "eraser"),
              width   = "100%"),
            shiny::hr(),
            shiny::uiOutput("design_warning"),
            shiny::actionButton(
              inputId = "design_update",
              label   = "  Update outputs  ",
              icon    = shiny::icon(name = "check-square-o"),
              width   = "100%"),
            shiny::hr(),
            shiny::textInput(
              inputId = "design_filename",
              label   = "Report filename:",
              value   = "multiarm_design") %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_filename",
                size    = "m",
                colour  = "black"),
            tags$head(tags$style(".full_width{width:100%;}")),
            shiny::radioButtons(
              inputId = "design_format",
              label   = "Download format",
              choices = c("PDF"  = "pdf",
                          "HTML" = "html",
                          "Word" = "word"),
              selected = "pdf",
              inline   = T),
            shiny::downloadButton(
              outputId = "design_report",
              label    = "  Download report  ",
              class    = "full_width")
          ),
          shinydashboard::box(
            title       = "Design summary",
            width       = 8,
            solidHeader = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::withMathJax(shiny::htmlOutput("design_summary")),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        ##### Row 2: Value box outputs #########################################
        shiny::fluidRow(
          shinydashboard::valueBoxOutput("design_ss_box"),
          shinydashboard::valueBoxOutput("design_fwer_box"),
          shinydashboard::valueBoxOutput("design_power_box")
        ),
        ##### Row 3: Operating characteristics summary #########################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Operating characteristics summary",
            width       = 12,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shiny::column(
              width = 12,
              align = "center",
              shinycssloaders::withSpinner(
                shiny::tableOutput("design_tab"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          )
        ),
        ##### Row 4: Plots #####################################################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Equal treatment effects",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                "equal",
                dblclick = "equal_dblclick",
                brush    = shiny::brushOpts(id         = "equal_brush",
                                            resetOnNew = T)
              ),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          ),
          shinydashboard::box(
            title       = "Shifted treatment effects",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                "shifted",
                dblclick = "shifted_dblclick",
                brush    = shiny::brushOpts(id         = "shifted_brush",
                                            resetOnNew = T)
              ),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Session Information",
            status      = "primary",
            solidHeader = T,
            width       = 12,
            collapsible = T,
            collapsed   = T,
            shiny::verbatimTextOutput("design_debug")
          )
        )
      ),
      ##### Tab: About #########################################################
      shinydashboard::tabItem(
        tabName = "about",
        h1("About"),
        p("This graphical user interface (GUI) is built upon (and in to)",
          " v.0.9.1 of the R package multiarm, written by Michael Grayling",
          "(Newcastle University)."),
        p("The first-line response to a possible bug should be to submit it as",
          " a 'New issue' at:"),
        a(href = "https://github.com/mjg211/multiarm/issues",
          "https://github.com/mjg211/multiarm/issues"),
        p(),
        p("If the issue is more complex, or a patch is not provided in",
          "reasonable time, please contact Michael Grayling at",
          "michael.grayling@newcastle.ac.uk. Similarly, please feel free to",
          "contact with suggestions for new features, or for further support",
          "with using the package or GUI."),
        p("If you use multiarm, please cite it with:"),
        p("Grayling MJ (2019) multiarm: Design and analysis of fixed-sample ",
          "multi-arm clinical trials.",
          "URL: http://www.github.com/mjg211/multiarm/.")
      )
    )
  ),
  title = "multiarm",
  skin  = "blue"
)

##### Server ###################################################################
server <- function(input, output, session) {

  ##### Initial set-up #########################################################

  shinyhelper::observe_helpers(withMathJax = T)
  ranges_equal   <- shiny::reactiveValues(x = NULL, y = NULL)
  ranges_shifted <- shiny::reactiveValues(x = NULL, y = NULL)

  ##### Design: shinyFeedback warning messages #################################

  shiny::observeEvent(input$design_alpha, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_alpha",
      condition = any(input$design_alpha <= 0, input$design_alpha >= 1),
      text      = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_beta, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_beta",
      condition = any(input$design_beta <= 0, input$design_beta >= 1),
      text      = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_delta1, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_delta1",
      condition = (input$design_delta1 <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_delta0, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_delta0",
      condition = (input$design_delta0 >= input$design_delta1),
      text      =
        "Must be strictly smaller than the interesting treatment effect")
  })

  shiny::observeEvent(input$design_sigma, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_sigma",
      condition = (input$design_sigma <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_sigma_0, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_sigma_0",
      condition = (input$design_sigma_0 <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(c(input$design_sigma_1, input$design_sigma_2,
                        input$design_sigma_3, input$design_sigma_4,
                        input$design_sigma_5), {
                          vals <- c(input$design_sigma_1, input$design_sigma_2, input$design_sigma_3,
                                    input$design_sigma_4, input$design_sigma_5)
                          for (i in 1:5) {
                            shinyFeedback::feedbackDanger(
                              inputId   = paste0("design_sigma_", i),
                              condition = (vals[i] <= 0),
                              text      = "Must be strictly positive")
                          }
                        })

  shiny::observeEvent(c(input$design_ratio_1, input$design_ratio_2,
                        input$design_ratio_3, input$design_ratio_4,
                        input$design_ratio_5), {
                          vals <- c(input$design_ratio_1, input$design_ratio_2, input$design_ratio_3,
                                    input$design_ratio_4, input$design_ratio_5)
                          for (i in 1:5) {
                            shinyFeedback::feedbackDanger(
                              inputId   = paste0("design_ratio_", i),
                              condition = (vals[i] <= 0),
                              text      = "Must be strictly positive")
                          }
                        })

  shiny::observeEvent(input$design_filename, {
    shinyFeedback::feedbackWarning(
      inputId   = "design_filename",
      condition = any(strsplit(input$design_filename, split = "")[[1]] %in%
                        c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text      = paste0('It is generally inadvisable to use the characters /',
                         ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Design: Dynamic UI elements ############################################

  output$design_delta0 <- renderUI({
    shiny::numericInput(
      inputId = "design_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = NA,
      max     = input$design_delta1,
      step    = 0.1) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black")
  })

  output$design_sigma <- renderUI({
    if (input$design_sigma_type == "equal_all") {
      shiny::numericInput(
        inputId = "design_sigma",
        label   = paste0("Standard deviation of the responses (arms 0, ..., ",
                         input$design_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.1)
    } else if (input$design_sigma_type == "equal_exp") {
      shiny::tagList(
        shiny::numericInput(
          inputId = "design_sigma_0",
          label   = "Standard deviation of the control arm responses (arm 0):",
          value   = 1,
          min     = 0,
          max     = NA,
          step    = 0.1),
        shiny::numericInput(
          inputId = "design_sigma_1",
          label   = paste0("Standard deviation of the experimental arm ",
                           "responses (arms 1, ..., ", input$design_K, ")"),
          value   = 1,
          min     = 0,
          max     = NA,
          step    = 0.1)
      )
    } else {
      inputTagList   <-
        shiny::tagList(
          shiny::numericInput(
            inputId = "design_sigma_0",
            label   =
              "Standard deviation of the control arm responses (arm 0):",
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.1)
        )
      lapply(1:input$design_K, function(i) {
        newInput     <-
          shiny::numericInput(
            inputId = paste0("design_sigma_", i),
            label   = paste0("Standard deviation of experimental arm ", i,
                             " responses:"),
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.1)
        inputTagList <<- tagAppendChild(inputTagList, newInput)
      })
      inputTagList
    }
  })

  output$design_ratio <- renderUI({
    if (input$design_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_ratio_1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25)
    } else if (input$design_ratio_type == "unequal") {
      inputTagList     <-
        shiny::tagList(
          shiny::numericInput(
            inputId = "design_ratio_1",
            label   = "Allocation ratio for experimental arm 1:",
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25)
        )
      lapply(2:input$design_K, function(i) {
        newInput     <-
          shiny::numericInput(
            inputId = paste0("design_ratio_", i),
            label   = paste0("Allocation ratio for experimental arm ", i, ":"),
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25)
        inputTagList <<- tagAppendChild(inputTagList, newInput)
      })
      inputTagList
    }
  })

  output$design_warning <- renderUI({
    if (any(all(input$design_K %in% c(4, 5),
                input$design_correction %in% c("benjamini_hochberg", "hochberg",
                                               "holm", "step_down_dunnett")),
            all(input$design_K == 5, input$design_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_density <- renderUI({
    if (input$design_plots) {
      shiny::selectInput(
        inputId = "design_density",
        label   = "Plot quality:",
        choices = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                    "Very high" = 200),
        selected = 100) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black")
    }
  })

  shiny::observeEvent(input$design_reset, {
    shinyjs::reset("box_design_parameters")
  })

  ##### Design: Plot zoom set-up ###############################################

  shiny::observeEvent(input$equal_dblclick, {
    brush            <- input$equal_brush
    if (!is.null(brush)) {
      ranges_equal$x <- c(brush$xmin, brush$xmax)
      ranges_equal$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_equal$x  <- ranges_equal$y <- NULL
    }
  })

  shiny::observeEvent(input$shifted_dblclick, {
    brush              <- input$shifted_brush
    if (!is.null(brush)) {
      ranges_shifted$x <- c(brush$xmin, brush$xmax)
      ranges_shifted$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges_shifted$x <- ranges_shifted$y <- NULL
    }
  })

  ##### Design: des() ##########################################################

  des <- shiny::eventReactive(input$design_update, {
    K                          <- input$design_K
    progress                   <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs",
                 value   = 0)
    if (input$design_sigma_type == "equal_all") {
      sigma                    <- rep(input$design_sigma, K + 1)
    } else if (input$design_sigma_type == "equal_exp") {
      sigma                    <- c(input$design_sigma_0,
                                    rep(input$design_sigma_1, K))
    } else if (input$design_sigma_type == "unequal") {
      sigma                    <- numeric(K + 1)
      for (i in 0:K) {
        sigma[i + 1]           <- input[[paste0("design_sigma_", i)]]
      }
    }
    if (input$design_ratio_type == "equal_all") {
      ratio                    <- rep(1, K)
    } else if (input$design_ratio_type == "equal_exp") {
      ratio                    <- rep(input$design_ratio_1, K)
    } else if (input$design_ratio_type == "unequal") {
      ratio                    <- numeric(K)
      for (i in 1:K) {
        ratio[i]               <- input[[paste0("design_ratio_", i)]]
      }
    } else if (input$design_ratio_type == "root_K") {
      ratio                    <- rep(1/sqrt(K), K)
    } else {
      ratio                    <- input$design_ratio_type
    }
    design                     <-
      multiarm::des_ma(K          = input$design_K,
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
    rmarkdown::render(
      input         = "design_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_summary.html"),
      params        = list(K          = design$K,
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
                           pi         = design$pi,
                           piO        = design$piO,
                           plots      = input$design_plots)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_summary_modified.html")
    )
    design$data_og             <- design$opchar
    design$data                <-
      data.frame(design$opchar,
                 row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                               paste0("<i>LFC<sub>", 1:input$design_K,
                                      "</sub></i>")))
    colnames(design$data)      <-
      c(paste0("<i>&tau;</i><sub>", 1:input$design_K, "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", 1:input$design_K), "</sub>"),
        "<i>FWER</i>", "<i>FDR</i>")
    if (input$design_plots) {
      alpha                    <- design$alpha
      beta                     <- design$beta
      delta0                   <- design$delta0
      delta1                   <- design$delta1
      delta                    <- delta1 - delta0
      density                  <- as.numeric(input$design_density)
      progress$inc(amount  = 0.25,
                   message = "Rendering plots")
      length_out               <- ceiling(0.5*as.numeric(input$design_density))
      tau                      <- matrix(c(seq(-design$delta1, -1e-6,
                                               length.out = length_out),
                                           seq(1e-6, 2*design$delta1,
                                               length.out = length_out)),
                                         2*length_out, K)
      opchar_equal             <- multiarm::opchar_ma(design, tau)$opchar
      opchar_equal             <- tidyr::gather(opchar_equal, "type", "P",
                                                `Pdis`:`FWER`)
      opchar_equal$type        <- factor(opchar_equal$type,
                                         levels = c(paste0("P", 1:K), "Pcon",
                                                    "Pdis", "FWER"))
      labels                   <- numeric(K + 3)
      labels[1]                <- parse(text = "italic(FWER)/italic(FDR)")
      for (i in 2:(K + 1)) {
        labels[i]              <- parse(text = paste0("italic(P)[", i - 1, "]"))
      }
      labels[(K + 2):(K + 3)]  <- c(parse(text = "italic(P)[con]"),
                                    parse(text = "italic(P)[dis]"))
      colours                  <- ggthemes::ptol_pal()(3 + K)
      design$equal             <- ggplot2::ggplot() +
        ggplot2::geom_line(data = dplyr::filter(opchar_equal,
                                                !(type %in% "FWER")),
                           ggplot2::aes(x   = tau1,
                                        y   = P,
                                        col = type)) +
        ggplot2::geom_line(data = dplyr::filter(opchar_equal,
                                                (type %in% "FWER") &
                                                  (tau1 <= 0)),
                           ggplot2::aes(x   = tau1,
                                        y   = P,
                                        col = type)) +
        ggplot2::geom_line(data = dplyr::filter(opchar_equal,
                                                (type %in% "FWER") &
                                                  (tau1 > 0)),
                           ggplot2::aes(x   = tau1,
                                        y   = P,
                                        col = type)) +
        ggplot2::scale_colour_manual(values = colours, labels = labels) +
        ggplot2::ylab("Probability/Rate") +
        ggplot2::theme_bw() +
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
      if (K == 2) {
        design$equal           <- design$equal +
          ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], sep = "")))
      } else if (K == 3) {
        design$equal           <- design$equal +
          ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], " = ", tau[3],
                                         sep = "")))
      } else {
        design$equal           <- design$equal +
          ggplot2::xlab(bquote(paste(tau[1], " = ... = ", tau[.(K)], sep = "")))
      }
      progress$inc(amount  = 0.25,
                   message = "Rendering plots")
      opchar_matrix            <- NULL
      tau_init                 <- matrix(seq(-delta1, 2*delta1,
                                             length.out = density) - delta,
                                         density, K)
      for (k in 1:K) {
        tau                    <- tau_init
        tau[, k]               <- tau[, k] + delta
        opchar_k               <- multiarm::opchar_ma(design, tau)$opchar
        opchar_matrix          <- rbind(opchar_matrix,
                                        as.matrix(opchar_k[, c(k, K + 2 + k)]))
        progress$inc(amount  = 0.25/(K + 1),
                     message = "Rendering plots")
      }
      opchar_shifted           <- tibble::as_tibble(opchar_matrix)
      colnames(opchar_shifted) <- c("tauk", "P")
      opchar_shifted           <-
        dplyr::mutate(opchar_shifted,
                      type = factor(rep(paste("P", 1:K, sep = ""),
                                        each = density)))
      labels                   <- numeric(K)
      for (i in 1:K) {
        labels[i]              <- parse(text = paste0("italic(P)[", i, "]"))
      }
      design$shifted           <- ggplot2::ggplot() +
        ggplot2::geom_line(data = opchar_shifted,
                           ggplot2::aes(x   = tauk,
                                        y   = P,
                                        col = type)) +
        ggplot2::scale_colour_manual(values = colours[2:(K + 1)],
                                     labels = labels) +
        ggplot2::xlab(bquote(paste("... = ", tau[italic(k) - 1], " + ",
                                   .(delta), " = ", tau[italic(k)], " = ",
                                   tau[italic(k) + 1], " + ", .(delta),
                                   " = ... ", sep = ""))) +
        ggplot2::ylab("Probability") +
        ggplot2::theme_bw() +
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
      design$equal             <- design$shifted <- design$delta <- NULL
    }
    progress$inc(amount  = 0.25 + as.numeric(!input$design_plots),
                 message = "Outputting results")
    design
  })

  ##### Design: Value boxes ####################################################

  output$design_ss_box <- shinydashboard::renderValueBox({
    input$design_update
    shinydashboard::valueBox(
      value    = round(des()$N, 1),
      subtitle = "Total required sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_fwer_box <- shinydashboard::renderValueBox({
    input$design_update
    correction      <- shiny::isolate(input$design_correction)
    if (!(correction %in% c("benjamini_hochberg", "none"))) {
      if (des()$opchar$FWER[1] <= shiny::isolate(input$design_alpha) + 1e-4) {
        icon_choice <- "thumbs-up"
      } else {
        icon_choice <- "thumbs-down"
      }
    } else {
      icon_choice   <- ""
    }
    shinydashboard::valueBox(
      value    = round(des()$opchar$FWER[1], 3),
      subtitle = "Maximum FWER",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_power_box <- shinydashboard::renderValueBox({
    input$design_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(input$design_power)]
    K                 <- isolate(input$design_K)
    if (input$design_power == "conjunctive") {
      value_power_box <- des()$opchar$Pcon[2]
    } else if (input$design_power == "disjunctive") {
      value_power_box <- des()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(des()$opchar[-(1:2), (K + 3):(2*K + 2)])))
    }
    if (value_power_box >= shiny::isolate(input$design_beta) - 1e-3) {
      icon_choice     <- "thumbs-up"
    } else {
      icon_choice     <- "thumbs-down"
    }
    shinydashboard::valueBox(
      value    = round(value_power_box, 3),
      subtitle = subtitle,
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  ##### Design: Summary ########################################################

  output$design_summary <- shiny::renderUI({
    input$design_update
    N <- des()$N
    shiny::withMathJax(
      shiny::includeHTML(path = file.path(tempdir(),
                                          "/design_summary_modified.html")
      )
    )
  })

  ##### Design: Table ##########################################################

  output$design_tab <- shiny::renderTable({
    input$design_update
    des()$data
  }, rownames            = T,
  sanitize.text.function = function(x) x,
  digits                 = 4,
  options                = list(searching  = F,
                                scrollX    = T,
                                autoWidth  = T,
                                columnDefs = list(list(width = '50px')),
                                paging     = F)
  )

  ##### Design: Plots ##########################################################

  output$equal <- shiny::renderPlot({
    input$design_update
    if (shiny::isolate(input$design_plots)) {
      des()$equal + ggplot2::coord_cartesian(xlim   = ranges_equal$x,
                                             ylim   = ranges_equal$y,
                                             expand = F)
    }
  })

  output$shifted <- shiny::renderPlot({
    input$design_update
    if (shiny::isolate(input$design_plots)) {
      des()$shifted + ggplot2::coord_cartesian(xlim   = ranges_shifted$x,
                                               ylim   = ranges_shifted$y,
                                               expand = F)
    }
  })

  ##### Design: Report #########################################################

  output$design_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_filename, sep = '.',
            switch(input$design_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_report.Rmd")
      file.copy("design_report.Rmd", tempReport, overwrite = T)
      params <- list(K            = des()$K,
                     alpha        = des()$alpha,
                     beta         = des()$beta,
                     delta1       = des()$delta1,
                     delta0       = des()$delta0,
                     sigma        = des()$sigma,
                     ratio_type   = input$design_ratio_type,
                     ratio_init   = c(input$design_ratio_1,
                                      input$design_ratio_2,
                                      input$design_ratio_3,
                                      input$design_ratio_4,
                                      input$design_ratio_5),
                     ratio        = des()$ratio,
                     correction   = des()$correction,
                     power        = des()$power,
                     integer      = des()$integer,
                     large_N      = des()$N,
                     small_n      = des()$n,
                     opchar       = des()$opchar,
                     pi           = des()$pi,
                     piO          = des()$piO,
                     plots        = input$design_plots,
                     equal        = des()$equal,
                     shifted      = des()$shifted,
                     data         = des()$data_og)
      if (input$design_format == "pdf") {
        format <- "pdf_document"
      } else if (input$design_format == "html") {
        format <- "html_document"
      } else {
        format <- "word_document"
      }
      rmarkdown::render(tempReport, output_format = format,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  ##### Design: Session Info ###################################################

  output$design_debug <- shiny::renderPrint({
    utils::sessionInfo()
  })

  ##### Close set-up ###########################################################

  session$onSessionEnded(stopApp)

}

shiny::shinyApp(ui, server)
