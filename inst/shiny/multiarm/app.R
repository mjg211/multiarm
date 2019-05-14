##### Load required packages ###################################################

library(multiarm)
options(shiny.sanitize.errors = T)

##### UI #######################################################################
ui <- shinydashboard::dashboardPage(
  ##### Dashboard: Header ######################################################
  shinydashboard::dashboardHeader(
    title      = "multiarm",
    titleWidth = 175
  ),
  ##### Dashboard: Sidebar #####################################################
  shinydashboard::dashboardSidebar(
    width = 175,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        text    = "Home",
        tabName = "home",
        icon    = shiny::icon(name = "home")
      ),
      shinydashboard::menuItem(
        text    = "Design",
        tabName = "design",
        icon    = shiny::icon(name = "list-alt",
                              lib  = "glyphicon"),
        shinydashboard::menuSubItem(
          text    = "Normal",
          tabName = "design_normal"
        ),
        shinydashboard::menuSubItem(
          text    = "Bernoulli",
          tabName = "design_bernoulli"
        )
      ),
      shinydashboard::menuItem(
        text    = "About",
        tabName = "about",
        icon    = shiny::icon(name = "question")
      ),
      shinydashboard::menuItem(
        text    = "Source code",
        icon    = shiny::icon(name = "file-code-o"),
        href    = "https://github.com/mjg211/multiarm/"
      )
    )
  ),
  ##### Dashboard: Body ########################################################
  shinydashboard::dashboardBody(
    #tags$head(includeScript("google-analytics.js")),
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
          "of several supported multiple comparison corrections, when the",
          "outcome data is assumed to be either normally or Bernoulli",
          "distributed. Available functions allow for sample size ",
          "determination (including for A-, D-, and E-optimal designs), trial",
          "simulation, analytical operating characteristic calculation",
          "(including the conjunctive power, disjunctive power, family-wise",
          "error-rate, and false discovery rate), and the production of ",
          "several plots."),
        p("At present, this GUI supports execution of the commands for design",
          "determination and plot production. Additional functionality will",
          "be added over time."),
        p("See the 'Design' tab on the sidebar for code execution, or the",
          "'About' tab for further information on the GUI.")
      ),
      ##### Tab: Design (Normal) ###############################################
      shinydashboard::tabItem(
        tabName = "design_normal",
        ##### Row 1: Design parameters & Design summary ########################
        shiny::fluidRow(
          shinydashboard::box(
            shiny::withMathJax(),
            shinyalert::useShinyalert(),
            shinyFeedback::useShinyFeedback(),
            shinyjs::useShinyjs(),
            id          = "design_normal_parameters",
            title       = "Design parameters",
            width       = 4,
            solidHeader = T,
            status      = "primary",
            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
            shiny::sliderInput(
              inputId = "design_normal_K",
              label   = "Number of experimental treatment arms:",
              min     = 2,
              max     = 5,
              value   = 2,
              step    = 1
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_K",
                size    = "m",
                colour  = "black"
              ),
            shiny::selectInput(
              inputId  = "design_normal_correction",
              label    = "Multiple comparison correction:",
              choices  =
                list("Per-hypothesis type-I error-rate control" =
                       list("No multiple comparison correction" = "none"),
                     "Familywise error-rate control: Single-step" =
                       list("Bonferroni"      = "bonferroni",
                            "Dunnett"         = "dunnett",
                            "Sidak"           = "sidak"),
                     "Familywise error-rate control: Step-wise" =
                       list("Hochberg"           = "hochberg",
                            "Holm-Bonferroni"    = "holm_bonferroni",
                            "Holm-Sidak"         = "holm_sidak",
                            "Step-down Dunnett"  = "step_down_dunnett"),
                     "False discovery rate control" =
                       list("Benjamini-Hochberg"  = "benjamini_hochberg",
                            "Benjamini-Yekutieli" = "benjamini_yekutieli")),
              selected = "dunnett"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_correction",
                size    = "m",
                colour  = "black"
              ),
            shiny::numericInput(
              inputId = "design_normal_alpha",
              label   = "Significance level:",
              value   = 0.05,
              min     = 0,
              max     = 1,
              step    = 0.01
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_alpha",
                size    = "m",
                colour  = "black"),
            shiny::selectInput(
              inputId = "design_normal_power",
              label   = "Type of power to control:",
              choices = c("Conjunctive" = "conjunctive",
                          "Disjunctive" = "disjunctive",
                          "Marginal"    = "marginal"),
              selected = "marginal"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_power",
                size    = "m",
                colour  = "black"),
            shiny::numericInput(
              inputId = "design_normal_beta",
              label   = "Desired power:",
              value   = 0.8,
              min     = 0,
              max     = 1,
              step    = 0.025
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_beta",
                size    = "m",
                colour  = "black"),
            shiny::numericInput(
              inputId = "design_normal_delta1",
              label   = "Interesting treatment effect:",
              value   = 0.5,
              min     = 0,
              max     = NA,
              step    = 0.1
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_delta1",
                size    = "m",
                colour  = "black"
              ),
            shiny::uiOutput("design_normal_delta0"),
            shiny::selectInput(
              inputId = "design_normal_sigma_type",
              label   = "Standard deviations:",
              choices = c("Equal across all arms"          = "equal_all",
                          "Equal across experimental arms" = "equal_exp",
                          "Unequal across all arms"        = "unequal"),
              selected = "equal_all"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_sigma_type",
                size    = "m",
                colour  = "black"
              ),
            shiny::uiOutput("design_normal_sigma"),
            shiny::selectInput(
              inputId = "design_normal_ratio_type",
              label   = "Allocation ratios:",
              choices =
                list("Explicit" =
                       list("Equal across all arms"          = "equal_all",
                            "Equal across experimental arms" = "equal_exp",
                            "Unequal across all arms"        = "unequal",
                            "root-K rule"                    = "root_K"),
                     "Implicit" = list("A-optimal" = "A",
                                       "D-optimal" = "D",
                                       "E-optimal" = "E")),
              selected = "equal_all"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_ratio_type",
                size    = "m",
                colour  = "black"
              ),
            shiny::uiOutput("design_normal_ratio"),
            shinyWidgets::prettySwitch(
              inputId = "design_normal_integer",
              label   = "Require integer sample sizes",
              status  = "info",
              value   = F,
              slim    = T
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_integer",
                size    = "m",
                colour  = "black"
              ),
            shinyWidgets::prettySwitch(
              inputId = "design_normal_plots",
              label   = "Plot power curves",
              status  = "info",
              value   = T,
              slim    = T
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_plots",
                size    = "m",
                colour  = "black"),
            shiny::uiOutput("design_normal_density"),
            shiny::hr(),
            shiny::actionButton(
              inputId = "design_normal_reset",
              label   = "  Reset inputs  ",
              icon    = shiny::icon(name = "eraser"),
              width   = "100%"
            ),
            shiny::hr(),
            shiny::uiOutput("design_normal_warning"),
            shiny::actionButton(
              inputId = "design_normal_update",
              label   = "  Update outputs  ",
              icon    = shiny::icon(name = "check-square-o"),
              width   = "100%"
            ),
            shiny::hr(),
            shiny::textInput(
              inputId = "design_normal_filename",
              label   = "Report filename:",
              value   = "multiarm_design"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_filename",
                size    = "m",
                colour  = "black"
              ),
            tags$head(tags$style(".full_width{width:100%;}")),
            shiny::radioButtons(
              inputId = "design_normal_format",
              label   = "Download format",
              choices = c("PDF"  = "pdf",
                          "HTML" = "html",
                          "Word" = "word"),
              selected = "pdf",
              inline   = T
            ),
            shiny::downloadButton(
              outputId = "design_normal_report",
              label    = "  Download report  ",
              class    = "full_width"
            )
          ),
          shinydashboard::box(
            title       = "Design summary",
            width       = 8,
            solidHeader = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::withMathJax(
                shiny::htmlOutput("design_normal_summary")
              ),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        ##### Row 2: Value box outputs #########################################
        shiny::fluidRow(
          shinydashboard::valueBoxOutput("design_normal_n_box"),
          shinydashboard::valueBoxOutput("design_normal_fwer_box"),
          shinydashboard::valueBoxOutput("design_normal_power_box")
        ),
        ##### Rows 3 & 4: Operating characteristics summary ####################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Operating characteristics summary: Error-rates",
            width       = 12,
            height      = 565,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shiny::column(
              width = 12,
              align = "center",
              shinycssloaders::withSpinner(
                DT::DTOutput("design_normal_table_error",
                             height = "500px"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title       =
              "Operating characteristics summary: Power & other quantities",
            width       = 12,
            height      = 565,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shiny::column(
              width = 12,
              align = "center",
              shinycssloaders::withSpinner(
                DT::DTOutput("design_normal_table_other",
                             height = "500px"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          )
        ),
        ##### Rows 5 & 6: Plots ################################################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Equal treatment effects: Error-rates",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                "design_normal_equal_error",
                dblclick = "design_normal_equal_error_dblclick",
                brush    = shiny::brushOpts(id         =
                                              "design_normal_equal_error_brush",
                                            resetOnNew = T)
              ),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          ),
          shinydashboard::box(
            title       = "Equal treatment effects: Power",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                "design_normal_equal_power",
                dblclick = "design_normal_equal_power_dblclick",
                brush    = shiny::brushOpts(id         =
                                              "design_normal_equal_power_brush",
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
            title       = "Equal treatment effects: Other",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                "design_normal_equal_other",
                dblclick = "design_normal_equal_other_dblclick",
                brush    = shiny::brushOpts(id         =
                                              "design_normal_equal_other_brush",
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
                "design_normal_shifted",
                dblclick = "design_normal_shifted_dblclick",
                brush    = shiny::brushOpts(id         =
                                              "design_normal_shifted_brush",
                                            resetOnNew = T)
              ),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        ##### Row 7: Session information #######################################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Session Information",
            status      = "primary",
            solidHeader = T,
            width       = 12,
            collapsible = T,
            collapsed   = T,
            shiny::verbatimTextOutput("design_normal_debug")
          )
        )
      ),






      ##### Tab: Design (Bernoulli) ############################################
      shinydashboard::tabItem(
        tabName = "design_bernoulli",
        ##### Row 1: Design parameters & Design summary ########################
        shiny::fluidRow(
          shinydashboard::box(
            shiny::withMathJax(),
            shinyalert::useShinyalert(),
            shinyFeedback::useShinyFeedback(),
            shinyjs::useShinyjs(),
            id          = "design_bernoulli_parameters",
            title       = "Design parameters",
            width       = 4,
            solidHeader = T,
            status      = "primary",
            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
            shiny::sliderInput(
              inputId = "design_bernoulli_K",
              label   = "Number of experimental treatment arms:",
              min     = 2,
              max     = 5,
              value   = 2,
              step    = 1
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_K",
                size    = "m",
                colour  = "black"
              ),
            shiny::selectInput(
              inputId  = "design_bernoulli_correction",
              label    = "Multiple comparison correction:",
              choices  =
                list("Per-hypothesis type-I error-rate control" =
                       list("No multiple comparison correction" = "none"),
                     "Familywise error-rate control: Single-step" =
                       list("Bonferroni"      = "bonferroni",
                            "Dunnett"         = "dunnett",
                            "Sidak"           = "sidak"),
                     "Familywise error-rate control: Step-wise" =
                       list("Hochberg"           = "hochberg",
                            "Holm-Bonferroni"    = "holm_bonferroni",
                            "Holm-Sidak"         = "holm_sidak",
                            "Step-down Dunnett"  = "step_down_dunnett"),
                     "False discovery rate control" =
                       list("Benjamini-Hochberg"  = "benjamini_hochberg",
                            "Benjamini-Yekutieli" = "benjamini_yekutieli")),
              selected = "dunnett"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_correction",
                size    = "m",
                colour  = "black"
              ),
            shiny::numericInput(
              inputId = "design_bernoulli_alpha",
              label   = "Significance level:",
              value   = 0.05,
              min     = 0,
              max     = 1,
              step    = 0.01
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_alpha",
                size    = "m",
                colour  = "black"),
            shiny::selectInput(
              inputId = "design_bernoulli_power",
              label   = "Type of power to control:",
              choices = c("Conjunctive" = "conjunctive",
                          "Disjunctive" = "disjunctive",
                          "Marginal"    = "marginal"),
              selected = "marginal"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_power",
                size    = "m",
                colour  = "black"),
            shiny::numericInput(
              inputId = "design_bernoulli_beta",
              label   = "Desired power:",
              value   = 0.8,
              min     = 0,
              max     = 1,
              step    = 0.025
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_beta",
                size    = "m",
                colour  = "black"),
            shiny::numericInput(
              inputId = "design_bernoulli_pi0",
              label   = "Control arm response rate:",
              value   = 0.3,
              min     = 0,
              max     = 1,
              step    = 0.1
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_bernoulli_pi0",
                size    = "m",
                colour  = "black"
              ),
            shiny::uiOutput("design_bernoulli_delta"),
            #shiny::uiOutput("design_bernoulli_delta0"),
            shiny::selectInput(
              inputId = "design_bernoulli_ratio_type",
              label   = "Allocation ratios:",
              choices =
                list("Explicit" =
                       list("Equal across all arms"          = "equal_all",
                            "Equal across experimental arms" = "equal_exp",
                            "Unequal across all arms"        = "unequal",
                            "root-K rule"                    = "root_K"),
                     "Implicit" = list("A-optimal" = "A",
                                       "D-optimal" = "D",
                                       "E-optimal" = "E")),
              selected = "equal_all"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_ratio_type",
                size    = "m",
                colour  = "black"
              ),
            shiny::uiOutput("design_bernoulli_ratio"),
            shinyWidgets::prettySwitch(
              inputId = "design_bernoulli_integer",
              label   = "Require integer sample sizes",
              status  = "info",
              value   = F,
              slim    = T
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_integer",
                size    = "m",
                colour  = "black"
              ),
            shinyWidgets::prettySwitch(
              inputId = "design_bernoulli_plots",
              label   = "Plot power curves",
              status  = "info",
              value   = T,
              slim    = T
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_plots",
                size    = "m",
                colour  = "black"),
            shiny::uiOutput("design_bernoulli_density"),
            shiny::hr(),
            shiny::actionButton(
              inputId = "design_bernoulli_reset",
              label   = "  Reset inputs  ",
              icon    = shiny::icon(name = "eraser"),
              width   = "100%"
            ),
            shiny::hr(),
            shiny::uiOutput("design_bernoulli_warning"),
            shiny::actionButton(
              inputId = "design_bernoulli_update",
              label   = "  Update outputs  ",
              icon    = shiny::icon(name = "check-square-o"),
              width   = "100%"
            ),
            shiny::hr(),
            shiny::textInput(
              inputId = "design_bernoulli_filename",
              label   = "Report filename:",
              value   = "multiarm_design"
            ) %>%
              shinyhelper::helper(
                type    = "markdown",
                title   = "",
                content = "design_normal_filename",
                size    = "m",
                colour  = "black"
              ),
            tags$head(tags$style(".full_width{width:100%;}")),
            shiny::radioButtons(
              inputId = "design_bernoulli_format",
              label   = "Download format",
              choices = c("PDF"  = "pdf",
                          "HTML" = "html",
                          "Word" = "word"),
              selected = "pdf",
              inline   = T
            ),
            shiny::downloadButton(
              outputId = "design_bernoulli_report",
              label    = "  Download report  ",
              class    = "full_width"
            )
          ),
          shinydashboard::box(
            title       = "Design summary",
            width       = 8,
            solidHeader = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::withMathJax(
                shiny::htmlOutput("design_bernoulli_summary")
              ),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        ##### Row 2: Value box outputs #########################################
        shiny::fluidRow(
          shinydashboard::valueBoxOutput("design_bernoulli_n_box"),
          shinydashboard::valueBoxOutput("design_bernoulli_fwer_box"),
          shinydashboard::valueBoxOutput("design_bernoulli_power_box")
        ),
        ##### Rows 3 & 4: Operating characteristics summary ####################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Operating characteristics summary: Error-rates",
            width       = 12,
            height      = 565,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shiny::column(
              width = 12,
              align = "center",
              shinycssloaders::withSpinner(
                DT::DTOutput("design_bernoulli_table_error",
                             height = "500px"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          )
        ),
        shiny::fluidRow(
          shinydashboard::box(
            title       =
              "Operating characteristics summary: Power & other quantities",
            width       = 12,
            height      = 565,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shiny::column(
              width = 12,
              align = "center",
              shinycssloaders::withSpinner(
                DT::DTOutput("design_bernoulli_table_other",
                             height = "500px"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          )
        ),
        ##### Rows 5 & 6: Plots ################################################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Equal treatment effects: Error-rates",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                "design_bernoulli_equal_error",
                dblclick = "design_bernoulli_equal_error_dblclick",
                brush    = shiny::brushOpts(
                  id         = "design_bernoulli_equal_error_brush",
                  resetOnNew = T)
              ),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          ),
          shinydashboard::box(
            title       = "Equal treatment effects: Power",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                "design_bernoulli_equal_power",
                dblclick = "design_bernoulli_equal_power_dblclick",
                brush    = shiny::brushOpts(
                  id         = "design_bernoulli_equal_power_brush",
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
            title       = "Equal treatment effects: Other",
            width       = 6,
            solidHeader = T,
            collapsible = T,
            status      = "primary",
            shinycssloaders::withSpinner(
              shiny::plotOutput(
                "design_bernoulli_equal_other",
                dblclick = "design_bernoulli_equal_other_dblclick",
                brush    = shiny::brushOpts(
                  id         = "design_bernoulli_equal_other_brush",
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
                "design_bernoulli_shifted",
                dblclick = "design_bernoulli_shifted_dblclick",
                brush    = shiny::brushOpts(
                  id         = "design_bernoulli_shifted_brush",
                  resetOnNew = T)
              ),
              type  = 6,
              color = "#3C8DBC",
              size  = 1/3
            )
          )
        ),
        ##### Row 7: Session information #######################################
        shiny::fluidRow(
          shinydashboard::box(
            title       = "Session Information",
            status      = "primary",
            solidHeader = T,
            width       = 12,
            collapsible = T,
            collapsed   = T,
            shiny::verbatimTextOutput("design_bernoulli_debug")
          )
        )
      ),
      ##### Tab: About #########################################################
      shinydashboard::tabItem(
        tabName = "about",
        h1("About"),
        p("This graphical user interface (GUI) is built upon (and in to)",
          " v.0.10 of the R package multiarm, written by Michael Grayling",
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
          "URL: http://www.github.com/mjg211/multiarm/."),
        p(),
        p("A selection of references related to the methodology used in",
          "multiarm are given below."),
        h4("References"),
        p("Benjamini Y, Hochberg Y (1995) Controlling the false discovery ",
          "rate: a practical and powerful approach to multiple testing.",
          em("J R Stat Soc B"), HTML("<b>57</b>(1)<b>:</b>289-300.")),
        p("Benjamini Y, Yekutieli D (2001) The control of the false discovery",
          "rate in multiple testing under dependency.", em("Ann Stat"),
          HTML("<b>29</b>(4)<b>:</b>1165–88.")),
        p("Bonferroni CE (1936) Teoria statistica delle classi e calcolo delle",
          "probabilita.", HTML("<i>Pubblicazioni del R Istituto Superiore di",
          "Scienze Economiche e Commerciali di Firenze</i>.")),
        p("Dunnett CW (1955) A multiple comparison procedure for comparing",
          "several treatments with a control.", em("J Am Stat Assoc"),
          HTML("<b>50</b>(272)<b>:</b>1096-121.")),
        p("Hochberg Y (1988) A sharper bonferroni procedure for multiple",
          "tests of significance.", em("Biometrika"),
          HTML("<b>75</b>(4)<b>:</b>800-2.")),
        p("Holm S (1979) A simple sequentially rejective multiple test ",
          "procedure.", em("Scand J Stat"), HTML("<b>6</b>(2)<b>:</b>65-70.")),
        p("Sidak Z (1967) Rectangular confidence regions for the means of ",
          "multivariate normal distributions.", em("J Am Stat Assoc"),
          HTML("<b>62</b>(318)<b>:</b>626-33."))
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
  ranges_design_normal_equal_error    <- shiny::reactiveValues(x = NULL,
                                                               y = NULL)
  ranges_design_normal_equal_power    <- shiny::reactiveValues(x = NULL,
                                                               y = NULL)
  ranges_design_normal_equal_other    <- shiny::reactiveValues(x = NULL,
                                                               y = NULL)
  ranges_design_normal_shifted        <- shiny::reactiveValues(x = NULL,
                                                               y = NULL)
  ranges_design_bernoulli_equal_error <- shiny::reactiveValues(x = NULL,
                                                               y = NULL)
  ranges_design_bernoulli_equal_power <- shiny::reactiveValues(x = NULL,
                                                               y = NULL)
  ranges_design_bernoulli_equal_other <- shiny::reactiveValues(x = NULL,
                                                               y = NULL)
  ranges_design_bernoulli_shifted     <- shiny::reactiveValues(x = NULL,
                                                               y = NULL)

  ##### Design (Normal): shinyFeedback warning messages ########################

  shiny::observeEvent(input$design_normal_alpha, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_normal_alpha",
      condition = any(input$design_normal_alpha <= 0,
                      input$design_normal_alpha >= 1),
      text      = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_normal_beta, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_normal_beta",
      condition = any(input$design_normal_beta <= 0,
                      input$design_normal_beta >= 1),
      text      = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_normal_delta1, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_normal_delta1",
      condition = (input$design_normal_delta1 <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_normal_delta0, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_normal_delta0",
      condition = (input$design_normal_delta0 >= input$design_normal_delta1),
      text      =
        "Must be strictly smaller than the interesting treatment effect")
  })

  shiny::observeEvent(input$design_normal_sigma, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_normal_sigma",
      condition = (input$design_normal_sigma <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_normal_sigma_0, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_normal_sigma_0",
      condition = (input$design_normal_sigma_0 <= 0),
      text      = "Must be strictly positive")
  })

  shiny::observeEvent(c(input$design_normal_sigma_1,
                        input$design_normal_sigma_2,
                        input$design_normal_sigma_3,
                        input$design_normal_sigma_4,
                        input$design_normal_sigma_5), {
    vals <- c(input$design_normal_sigma_1, input$design_normal_sigma_2,
              input$design_normal_sigma_3, input$design_normal_sigma_4,
              input$design_normal_sigma_5)
    for (i in 1:5) {
      shinyFeedback::feedbackDanger(
        inputId   = paste0("design_normal_sigma_", i),
        condition = (vals[i] <= 0),
        text      = "Must be strictly positive")
    }
  })

  shiny::observeEvent(c(input$design_normal_ratio_1,
                        input$design_normal_ratio_2,
                        input$design_normal_ratio_3,
                        input$design_normal_ratio_4,
                        input$design_normal_ratio_5), {
    vals <- c(input$design_normal_ratio_1, input$design_normal_ratio_2,
              input$design_normal_ratio_3, input$design_normal_ratio_4,
              input$design_normal_ratio_5)
    for (i in 1:5) {
      shinyFeedback::feedbackDanger(
        inputId   = paste0("design_normal_ratio_", i),
        condition = (vals[i] <= 0),
        text      = "Must be strictly positive")
    }
  })

  shiny::observeEvent(input$design_normal_filename, {
    shinyFeedback::feedbackWarning(
      inputId   = "design_normal_filename",
      condition = any(strsplit(input$design_normal_filename,
                               split = "")[[1]] %in%
                        c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text      = paste0('It is generally inadvisable to use the characters /',
                         ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Design (Normal): Dynamic UI elements ###################################

  output$design_normal_delta0 <- renderUI({
    shiny::numericInput(
      inputId = "design_normal_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = NA,
      max     = input$design_normal_delta1,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_normal_delta0",
        size    = "m",
        colour  = "black"
      )
  })

  output$design_normal_sigma <- renderUI({
    if (input$design_normal_sigma_type == "equal_all") {
      shiny::numericInput(
        inputId = "design_normal_sigma",
        label   = paste0("Standard deviation of the responses (arms 0, ..., ",
                         input$design_normal_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.1
      )
    } else if (input$design_normal_sigma_type == "equal_exp") {
      shiny::tagList(
        shiny::numericInput(
          inputId = "design_normal_sigma_0",
          label   = "Standard deviation of the control arm responses (arm 0):",
          value   = 1,
          min     = 0,
          max     = NA,
          step    = 0.1
        ),
        shiny::numericInput(
          inputId = "design_normal_sigma_1",
          label   =
            paste0("Standard deviation of the experimental arm responses ",
                   "(arms 1, ..., ", input$design_normal_K, ")"),
          value   = 1,
          min     = 0,
          max     = NA,
          step    = 0.1
        )
      )
    } else {
      inputTagList   <-
        shiny::tagList(
          shiny::numericInput(
            inputId = "design_normal_sigma_0",
            label   =
              "Standard deviation of the control arm responses (arm 0):",
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.1
          )
        )
      lapply(1:input$design_normal_K, function(i) {
        newInput     <-
          shiny::numericInput(
            inputId = paste0("design_normal_sigma_", i),
            label   = paste0("Standard deviation of experimental arm ", i,
                             " responses:"),
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.1
          )
        inputTagList <<- tagAppendChild(inputTagList, newInput)
      })
      inputTagList
    }
  })

  output$design_normal_ratio <- renderUI({
    if (input$design_normal_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_normal_ratio_1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_normal_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    } else if (input$design_normal_ratio_type == "unequal") {
      inputTagList     <-
        shiny::tagList(
          shiny::numericInput(
            inputId = "design_normal_ratio_1",
            label   = "Allocation ratio for experimental arm 1:",
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25
          )
        )
      lapply(2:input$design_normal_K, function(i) {
        newInput     <-
          shiny::numericInput(
            inputId = paste0("design_normal_ratio_", i),
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

  output$design_normal_warning <- renderUI({
    if (any(all(input$design_normal_K %in% c(4, 5),
                input$design_normal_correction %in%
                  c("benjamini_hochberg", "benjamini_yekutieli", "hochberg",
                    "holm_bonferroni", "holm_sidak", "step_down_dunnett")),
            all(input$design_normal_K == 5, input$design_normal_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_normal_density <- renderUI({
    if (input$design_normal_plots) {
      shiny::selectInput(
        inputId = "design_normal_density",
        label   = "Plot quality:",
        choices = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                    "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_normal_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_normal_reset, {
    shinyjs::reset("design_normal_parameters")
  })

  ##### Design (Normal): Plot zoom set-up ######################################

  shiny::observeEvent(input$design_normal_equal_error_dblclick, {
    brush_error                            <-
      input$design_normal_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_normal_equal_error$x   <- c(brush_error$xmin,
                                                brush_error$xmax)
      ranges_design_normal_equal_error$y   <- c(brush_error$ymin,
                                                brush_error$ymax)
    } else {
      ranges_design_normal_equal_error$x   <-
        ranges_design_normal_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_normal_equal_power_dblclick, {
    brush_power                            <-
      input$design_normal_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_normal_equal_power$x   <- c(brush_power$xmin,
                                                brush_power$xmax)
      ranges_design_normal_equal_power$y   <- c(brush_power$ymin,
                                                brush_power$ymax)
    } else {
      ranges_design_normal_equal_power$x   <-
        ranges_design_normal_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_normal_equal_other_dblclick, {
    brush_other                            <- input$design_normal_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_normal_equal_other$x   <- c(brush_other$xmin,
                                                brush_other$xmax)
      ranges_design_normal_equal_other$y   <- c(brush_other$ymin,
                                                brush_other$ymax)
    } else {
      ranges_design_normal_equal_other$x   <-
        ranges_design_normal_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_normal_shifted_dblclick, {
    brush_shifted                      <- input$design_normal_shifted_brush
    if (!is.null(brush_shifted)) {
      ranges_design_normal_shifted$x   <- c(brush_shifted$xmin,
                                            brush_shifted$xmax)
      ranges_design_normal_shifted$y   <- c(brush_shifted$ymin,
                                            brush_shifted$ymax)
    } else {
      ranges_design_normal_shifted$x   <-
        ranges_design_normal_shifted$y <- NULL
    }
  })

  ##### Design (Normal): des_normal() ##########################################

  des_normal <- shiny::eventReactive(input$design_normal_update, {
    K                             <- input$design_normal_K
    seq_K                         <- 1:K
    progress                      <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs",
                 value   = 0)
    if (input$design_normal_sigma_type == "equal_all") {
      sigma                       <- rep(input$design_normal_sigma, K + 1)
    } else if (input$design_normal_sigma_type == "equal_exp") {
      sigma                       <- c(input$design_normal_sigma_0,
                                       rep(input$design_normal_sigma_1, K))
    } else if (input$design_normal_sigma_type == "unequal") {
      sigma                       <- numeric(K + 1)
      for (i in c(0, seq_K)) {
        sigma[i + 1]              <- input[[paste0("design_normal_sigma_", i)]]
      }
    }
    if (input$design_normal_ratio_type == "equal_all") {
      ratio                       <- rep(1, K)
    } else if (input$design_normal_ratio_type == "equal_exp") {
      ratio                       <- rep(input$design_normal_ratio_1, K)
    } else if (input$design_normal_ratio_type == "unequal") {
      ratio                       <- numeric(K)
      for (i in seq_K) {
        ratio[i]                  <- input[[paste0("design_normal_ratio_", i)]]
      }
    } else if (input$design_normal_ratio_type == "root_K") {
      ratio                       <- rep(1/sqrt(K), K)
    } else {
      ratio                       <- input$design_normal_ratio_type
    }
    design                        <-
      multiarm::des_ma(K          = input$design_normal_K,
                       alpha      = input$design_normal_alpha,
                       beta       = 1 - input$design_normal_beta,
                       delta1     = input$design_normal_delta1,
                       delta0     = input$design_normal_delta0,
                       sigma      = sigma,
                       ratio      = ratio,
                       correction = input$design_normal_correction,
                       power      = input$design_normal_power,
                       integer    = input$design_normal_integer)
    progress$inc(amount  = 0.25 + as.numeric(!input$design_normal_plots),
                 message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_normal_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_normal_summary.html"),
      params        = list(K          = design$K,
                           alpha      = design$alpha,
                           beta       = design$beta,
                           delta1     = design$delta1,
                           delta0     = design$delta0,
                           sigma      = design$sigma,
                           ratio_type = input$design_normal_ratio_type,
                           ratio_init = c(input$design_normal_ratio_1,
                                          input$design_normal_ratio_2,
                                          input$design_normal_ratio_3,
                                          input$design_normal_ratio_4,
                                          input$design_normal_ratio_5),
                           ratio      = design$ratio,
                           correction = design$correction,
                           power      = design$power,
                           integer    = design$integer,
                           large_N    = design$N,
                           small_n    = design$n,
                           opchar     = design$opchar,
                           gamma         = design$gamma,
                           gammaO        = design$gammaO,
                           plots      = input$design_normal_plots)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_normal_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_normal_summary_modified.html")
    )
    design$data_og                <- design$opchar
    if (input$design_normal_plots) {
      alpha                       <- design$alpha
      beta                        <- design$beta
      delta0                      <- design$delta0
      delta1                      <- design$delta1
      delta                       <- delta1 - delta0
      density                     <- as.numeric(input$design_normal_density)
      progress$inc(amount  = 0.25,
                   message = "Rendering plots")
      length_out                  <- ceiling(0.5*density)
      tau                         <- matrix(c(seq(-design$delta1, -1e-6,
                                                  length.out = length_out),
                                              seq(1e-6, 2*design$delta1,
                                                  length.out = length_out)),
                                            2*length_out, K)
      opchar_equal_og             <- opchar_ma(design, tau)$opchar
      opchar_equal                <- tidyr::gather(opchar_equal_og, "type", "P",
                                                   `Pdis`:`Spec`)
      opchar_equal$type           <-
        factor(opchar_equal$type,
               c("Pdis", "Pcon",
                 paste0(rep(c("P", "FWERI", "FWERII"), each = K), seq_K),
                 "PHER", "FDR", "pFDR", "FNDR", "Sens", "Spec"))
      labels_power                <- numeric(K + 2)
      labels_power[1:2]           <- c(parse(text = "italic(P)[dis]"),
                                       parse(text = "italic(P)[con]"))
      for (i in seq_K + 2) {
        labels_power[i]           <-
          parse(text = paste("italic(P)[", i - 2, "]", sep = ""))
      }
      colours_power               <- ggthemes::ptol_pal()(2 + K)
      design$equal_power          <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal,
                               type %in% c("Pdis", "Pcon", paste0("P", seq_K))),
          ggplot2::aes(x   = tau1,
                       y   = P,
                       col = type)) +
        ggplot2::scale_colour_manual(values = colours_power,
                                     labels = labels_power) +
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
        ggplot2::geom_vline(xintercept = 0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = delta1,
                            linetype   = 2)
      if (K == 2) {
        design$equal_power        <- design$equal_power +
          ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], sep = "")))
      } else if (K == 3) {
        design$equal_power        <- design$equal_power +
          ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], " = ", tau[3],
                                         sep = "")))
      } else {
        design$equal_power        <- design$equal_power +
          ggplot2::xlab(bquote(paste(tau[1], " = ... = ", tau[.(K)],
                                     sep = "")))
      }
      labels_error                <- numeric(2*K + 1)
      for (i in seq_K) {
        labels_error[i]           <-
          parse(text = paste("italic(FWER)[italic(I)][", i, "]", sep = ""))
        labels_error[K + i]       <-
          parse(text = paste("italic(FWER)[italic(II)][", i, "]", sep = ""))
      }
      labels_error[2*K + 1]       <- parse(text = "italic(PHER)")
      colours_error               <- ggthemes::ptol_pal()(2*K + 1)
      design$equal_error          <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal,
                               (type %in% c(paste0("FWERI", seq_K),
                                            paste0("FWERII", seq_K), "PHER")) &
                                 (tau1 <= 0)),
          ggplot2::aes(x   = tau1,
                       y   = P,
                       col = type)) +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal,
                               (type %in% c(paste0("FWERI", seq_K),
                                            paste0("FWERII", seq_K), "PHER")) &
                                 (tau1 > 0)),
          ggplot2::aes(x   = tau1,
                       y   = P,
                       col = type)) +
        ggplot2::scale_colour_manual(values = colours_error,
                                     labels = labels_error) +
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
        ggplot2::geom_vline(xintercept = 0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = delta1,
                            linetype   = 2)
      if (K == 2) {
        design$equal_error        <- design$equal_error +
          ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], sep = "")))
      } else if (K == 3) {
        design$equal_error        <- design$equal_error +
          ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], " = ", tau[3],
                                         sep = "")))
      } else {
        design$equal_error        <- design$equal_error +
          ggplot2::xlab(bquote(paste(tau[1], " = ... = ", tau[.(K)], sep = "")))
      }
      labels_other                <-
        c(parse(text = "italic(FDR)"),  parse(text = "italic(pFDR)"),
          parse(text = "italic(FNDR)"), parse(text = "italic(Sensitivity)"),
          parse(text = "italic(Specificity)"))
      colours_other               <- ggthemes::ptol_pal()(5)
      design$equal_other          <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal,
                               (type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                            "Spec")) & (tau1 <= 0)),
          ggplot2::aes(x   = tau1,
                       y   = P,
                       col = type)) +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal,
                               (type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                            "Spec")) & (tau1 > 0)),
          ggplot2::aes(x   = tau1,
                       y   = P,
                       col = type)) +
        ggplot2::scale_colour_manual(values = colours_other,
                                     labels = labels_other) +
        ggplot2::ylab("Rate") +
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
        ggplot2::geom_vline(xintercept = 0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = delta1,
                            linetype   = 2)
      if (K == 2) {
        design$equal_other        <- design$equal_other +
          ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], sep = "")))
      } else if (K == 3) {
        design$equal_other        <- design$equal_other +
          ggplot2::xlab(expression(paste(tau[1], " = ", tau[2], " = ", tau[3],
                                         sep = "")))
      } else {
        design$equal_other        <- design$equal_other +
          ggplot2::xlab(bquote(paste(tau[1], " = ... = ", tau[.(K)], sep = "")))
      }
      progress$inc(amount  = 0.25,
                   message = "Rendering plots")
      opchar_matrix               <- NULL
      tau_init                    <- matrix(seq(-delta1, 2*delta1,
                                                length.out = density) - delta,
                                            density, K)
      for (k in seq_K) {
        tau                       <- tau_init
        tau[, k]                  <- tau[, k] + delta
        opchar_k                  <- multiarm::opchar_ma(design, tau)$opchar
        opchar_matrix             <-
          rbind(opchar_matrix, cbind(as.matrix(opchar_k[, c(k, K + 2 + k)]),
                                     as.matrix(opchar_k)))
        progress$inc(amount  = 0.25/(K + 1),
                     message = "Rendering plots")
      }
      opchar_shifted_og           <- tibble::as_tibble(opchar_matrix)
      opchar_shifted              <- opchar_shifted_og[, 1:2]
      colnames(opchar_shifted)    <- c("tauk", "P")
      opchar_shifted              <-
        dplyr::mutate(opchar_shifted,
                      type = factor(rep(paste0("P", seq_K), each = density)))
      opchar_shifted_og           <- opchar_shifted_og[, -(1:2)]
      labels                      <- numeric(K)
      for (i in seq_K) {
        labels[i]                 <- parse(text = paste0("italic(P)[", i, "]"))
      }
      design$shifted              <- ggplot2::ggplot() +
        ggplot2::geom_line(data = opchar_shifted,
                           ggplot2::aes(x   = tauk,
                                        y   = P,
                                        col = type)) +
        ggplot2::scale_colour_manual(values = colours_power[-(1:2)],
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
        ggplot2::geom_vline(xintercept = 0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = delta1,
                            linetype   = 2)
      colnames(opchar_shifted_og) <- colnames(opchar_equal_og)
      opchar_equal_og             <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(opchar_equal_og, 3))))
      opchar_shifted_og           <-
        as.matrix(dplyr::distinct(
          tibble::as_tibble(round(opchar_shifted_og, 3))))
      design$data                 <-
        data.frame(rbind(design$opchar, opchar_equal_og, opchar_shifted_og),
                   row.names =
                     c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                       paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                       paste0("Equal: #", 1:nrow(opchar_equal_og)),
                       paste0("Shifted: <i>&tau;</i><sub>",
                              rep(seq_K, each = nrow(opchar_shifted_og)/K),
                              "</sub>, #",
                              rep(1:(nrow(opchar_shifted_og)/K), K))))
      colnames(design$data)       <-
        c(paste0("<i>&tau;</i><sub>", seq_K, "</sub>"),
          paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
          paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
          paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"), "<i>PHER</i>",
          "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>", "<i>Sensitivity</i>",
          "<i>Specificity</i>")
    } else {
      design$data                 <-
        data.frame(design$opchar,
                   row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      colnames(design$data)       <-
        c(paste0("<i>&tau;</i><sub>", seq_K, "</sub>"),
          paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
          paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
          paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
          "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
          "<i>Sensitivity</i>", "<i>Specificity</i>")
      design$equal_error          <- design$equal_power <- design$equal_other <-
        design$shifted <- design$delta <- NULL
    }
    progress$inc(amount  = 0.25 + as.numeric(!input$design_normal_plots),
                 message = "Outputting results")
    design
  })

  ##### Design (Normal): Value boxes ###########################################

  output$design_normal_n_box <- shinydashboard::renderValueBox({
    input$design_normal_update
    shinydashboard::valueBox(
      value    = round(des_normal()$N, 1),
      subtitle = "Total required sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_normal_fwer_box <- shinydashboard::renderValueBox({
    input$design_normal_update
    correction      <- shiny::isolate(input$design_normal_correction)
    if (!(correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                            "none"))) {
      if (des_normal()$opchar$FWERI1[1] <=
            shiny::isolate(input$design_normal_alpha) + 1e-4) {
        icon_choice <- "thumbs-up"
      } else {
        icon_choice <- "thumbs-down"
      }
    } else {
      icon_choice   <- ""
    }
    shinydashboard::valueBox(
      value    = round(des_normal()$opchar$FWERI1[1], 3),
      subtitle = "Maximum familywise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_normal_power_box <- shinydashboard::renderValueBox({
    input$design_normal_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(input$design_normal_power)]
    K                 <- isolate(input$design_normal_K)
    if (input$design_normal_power == "conjunctive") {
      value_power_box <- des_normal()$opchar$Pcon[2]
    } else if (input$design_normal_power == "disjunctive") {
      value_power_box <- des_normal()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(des_normal()$opchar[-(1:2), (K + 3):(2*K + 2)])))
    }
    if (value_power_box >= shiny::isolate(input$design_normal_beta) - 1e-3) {
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

  ##### Design (Normal): Summary ###############################################

  output$design_normal_summary <- shiny::renderUI({
    input$design_normal_update
    N <- des_normal()$N
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_normal_summary_modified.html")
      )
    )
  })

  ##### Design (Normal): Table #################################################

  output$design_normal_table_error <- DT::renderDT({
    DT::datatable(
      round(
        des_normal()$data[, c(1:des_normal()$K,
                              (2*des_normal()$K + 3):(4*des_normal()$K + 2))],
        3),
      escape        = F,
      fillContainer = T
    )
  })

  output$design_normal_table_other <- DT::renderDT({
    DT::datatable(
      round(
        des_normal()$data[, -((2*des_normal()$K + 3):(4*des_normal()$K + 2))],
        3),
      escape        = F,
      fillContainer = T
    )
  })

  ##### Design (Normal): Plots #################################################

  output$design_normal_equal_error <- shiny::renderPlot({
    input$design_normal_update
    if (shiny::isolate(input$design_normal_plots)) {
      des_normal()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_normal_equal_error$x,
                                 ylim   = ranges_design_normal_equal_error$y,
                                 expand = F)
    }
  })

  output$design_normal_equal_power <- shiny::renderPlot({
    input$design_normal_update
    if (shiny::isolate(input$design_normal_plots)) {
      des_normal()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_normal_equal_power$x,
                                 ylim   = ranges_design_normal_equal_power$y,
                                 expand = F)
    }
  })

  output$design_normal_equal_other <- shiny::renderPlot({
    input$design_normal_update
    if (shiny::isolate(input$design_normal_plots)) {
      des_normal()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_normal_equal_other$x,
                                 ylim   = ranges_design_normal_equal_other$y,
                                 expand = F)
    }
  })

  output$design_normal_shifted <- shiny::renderPlot({
    input$design_normal_update
    if (shiny::isolate(input$design_normal_plots)) {
      des_normal()$shifted +
        ggplot2::coord_cartesian(xlim   = ranges_design_normal_shifted$x,
                                 ylim   = ranges_design_normal_shifted$y,
                                 expand = F)
    }
  })

  ##### Design (Normal): Report ################################################

  output$design_normal_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_normal_filename, sep = '.',
            switch(input$design_normal_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_normal_report.Rmd")
      file.copy("design_normal_report.Rmd", tempReport, overwrite = T)
      params     <- list(K            = des_normal()$K,
                         alpha        = des_normal()$alpha,
                         beta         = des_normal()$beta,
                         delta1       = des_normal()$delta1,
                         delta0       = des_normal()$delta0,
                         sigma        = des_normal()$sigma,
                         ratio_type   = input$design_normal_ratio_type,
                         ratio_init   = c(input$design_normal_ratio_1,
                                          input$design_normal_ratio_2,
                                          input$design_normal_ratio_3,
                                          input$design_normal_ratio_4,
                                          input$design_normal_ratio_5),
                         ratio        = des_normal()$ratio,
                         correction   = des_normal()$correction,
                         power        = des_normal()$power,
                         integer      = des_normal()$integer,
                         large_N      = des_normal()$N,
                         small_n      = des_normal()$n,
                         opchar       = des_normal()$opchar,
                         gamma           = des_normal()$gamma,
                         gammaO          = des_normal()$gammaO,
                         plots        = input$design_normal_plots,
                         equal_error  = des_normal()$equal_error,
                         equal_power  = des_normal()$equal_power,
                         equal_other  = des_normal()$equal_other,
                         shifted      = des_normal()$shifted,
                         data         = des_normal()$data_og)
      if (input$design_normal_format == "pdf") {
        format   <- "pdf_document"
      } else if (input$design_normal_format == "html") {
        format   <- "html_document"
      } else {
        format   <- "word_document"
      }
      rmarkdown::render(tempReport,
                        output_format = format,
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Design (Bernoulli): shinyFeedback warning messages #####################

  shiny::observeEvent(input$design_bernoulli_alpha, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_bernoulli_alpha",
      condition = any(input$design_bernoulli_alpha <= 0,
                      input$design_bernoulli_alpha >= 1),
      text      = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_bernoulli_beta, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_bernoulli_beta",
      condition = any(input$design_bernoulli_beta <= 0,
                      input$design_bernoulli_beta >= 1),
      text      = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_bernoulli_pi0, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_bernoulli_pi0",
      condition = any(input$design_bernoulli_pi0 <= 0,
                      input$design_bernoulli_pi0 >= 1),
      text      = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_bernoulli_delta1, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_bernoulli_delta1",
      condition = any(input$design_bernoulli_delta1 <= 0,
                      input$design_bernoulli_delta1 >=
                        1 - input$design_bernoulli_pi0),
      text      = paste0("Must be strictly between 0 and one minus the control",
                         " arm response rate"))
  })

  shiny::observeEvent(input$design_bernoulli_delta0, {
    shinyFeedback::feedbackDanger(
      inputId   = "design_bernoulli_delta0",
      condition = any(input$design_bernoulli_delta0 >=
                        input$design_bernoulli_delta1,
                      input$design_bernoulli_delta0 <=
                        -input$design_bernoulli_pi0),
      text      =
        "Must be strictly between -p0 and the interesting treatment effect")
  })

  shiny::observeEvent(c(input$design_bernoulli_ratio_1,
                        input$design_bernoulli_ratio_2,
                        input$design_bernoulli_ratio_3,
                        input$design_bernoulli_ratio_4,
                        input$design_bernoulli_ratio_5), {
    vals <- c(input$design_bernoulli_ratio_1, input$design_bernoulli_ratio_2,
              input$design_bernoulli_ratio_3, input$design_bernoulli_ratio_4,
              input$design_bernoulli_ratio_5)
    for (i in 1:5) {
      shinyFeedback::feedbackDanger(
        inputId   = paste0("design_bernoulli_ratio_", i),
        condition = (vals[i] <= 0),
        text      = "Must be strictly positive")
    }
  })

  shiny::observeEvent(input$design_bernoulli_filename, {
    shinyFeedback::feedbackWarning(
      inputId   = "design_bernoulli_filename",
      condition = any(strsplit(input$design_bernoulli_filename,
                               split = "")[[1]] %in%
                        c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text      = paste0('It is generally inadvisable to use the characters /',
                         ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Design (Bernoulli): Dynamic UI elements ################################

  output$design_bernoulli_delta <- renderUI({
    inputTagList     <-
      shiny::tagList(
        (shiny::numericInput(
          inputId = "design_bernoulli_delta1",
          label   = "Interesting treatment effect:",
          value   = 0.2,
          min     = 0,
          max     = 1 - input$design_bernoulli_pi0,
          step    = 0.1
        ) %>%
          shinyhelper:: helper(
            type    = "markdown",
            title   = "",
            content = "design_bernoulli_delta1",
            size    = "m",
            colour  = "black"
          ))
      )
    newInput     <- (shiny::numericInput(
      inputId = "design_bernoulli_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = -input$design_bernoulli_pi0,
      max     = 1 - input$design_bernoulli_pi0,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_bernoulli_delta0",
        size    = "m",
        colour  = "black"
      ))
    inputTagList <- tagAppendChild(inputTagList, newInput)
    inputTagList
  })

  #output$design_bernoulli_delta0 <- renderUI({
  # shiny::numericInput(
  #   inputId = "design_bernoulli_delta0",
  #   label   = "Uninteresting treatment effect:",
  #   value   = 0,
  #   min     = -input$design_bernoulli_p0,
  #   max     = input$design_bernoulli_delta1,
  #   step    = 0.1
  # ) %>%
  #   shinyhelper:: helper(
  #     type    = "markdown",
        #    title   = "",
        #      content = "design_bernoulli_delta0",
        #      size    = "m",
        #      colour  = "black"
        #    )
    #})

  # output$design_bernoulli_delta1 <- renderUI({
    #   shiny::numericInput(
      #     inputId = "design_bernoulli_delta1",
      #     label   = "Interesting treatment effect:",
      #    value   = 0,
      #  min     = 0,
      #     max     = 1 - input$design_bernoulli_p0,
      #  step    = 0.1
      #) %>%
      #    shinyhelper:: helper(
  #      type    = "markdown",
  #      title   = "",
  #      content = "design_bernoulli_delta1",
  #      size    = "m",
  #      colour  = "black"
  #    )
  #})

  output$design_bernoulli_ratio <- renderUI({
    if (input$design_bernoulli_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_bernoulli_ratio_1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_bernoulli_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    } else if (input$design_bernoulli_ratio_type == "unequal") {
      inputTagList     <-
        shiny::tagList(
          shiny::numericInput(
            inputId = "design_bernoulli_ratio_1",
            label   = "Allocation ratio for experimental arm 1:",
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25
          )
        )
      lapply(2:input$design_bernoulli_K, function(i) {
        newInput     <-
          shiny::numericInput(
            inputId = paste0("design_bernoulli_ratio_", i),
            label   = paste0("Allocation ratio for experimental arm ", i, ":"),
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25)
        inputTagList <<- tagAppendChild(inputTagList, newInput)
      })
      inputTagList
    } else if (input$design_bernoulli_ratio_type %in% c("A", "D", "E")) {
      shiny::selectInput(
        inputId = "design_bernoulli_ratio_scenario",
        label   =
          "Treatment effect scenario to optimise allocation ratios for:",
        choices = c("Global null hypothesis"        = "HG",
                    "Global alternative hypothesis" = "HA"),
        selected = "HG"
      )
    }
  })

  output$design_bernoulli_warning <- renderUI({
    if (any(all(input$design_bernoulli_K %in% c(4, 5),
                input$design_bernoulli_correction %in%
                c("benjamini_hochberg", "benjamini_yekutieli", "hochberg",
                  "holm_bonferroni", "holm_sidak", "step_down_dunnett")),
            all(input$design_bernoulli_K == 5, input$design_bernoulli_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_bernoulli_density <- renderUI({
    if (input$design_bernoulli_plots) {
      shiny::selectInput(
        inputId = "design_bernoulli_density",
        label   = "Plot quality:",
        choices = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                    "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_normal_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_bernoulli_reset, {
    shinyjs::reset("design_bernoulli_parameters")
  })

  ##### Design (Bernoulli): Plot zoom set-up ###################################

  shiny::observeEvent(input$design_bernoulli_equal_error_dblclick, {
    brush_error                               <-
      input$design_bernoulli_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_bernoulli_equal_error$x   <- c(brush_error$xmin,
                                                   brush_error$xmax)
      ranges_design_bernoulli_equal_error$y   <- c(brush_error$ymin,
                                                   brush_error$ymax)
    } else {
      ranges_design_bernoulli_equal_error$x   <-
        ranges_design_bernoulli_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_bernoulli_equal_power_dblclick, {
    brush_power                               <-
      input$design_bernoulli_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_bernoulli_equal_power$x   <- c(brush_power$xmin,
                                                   brush_power$xmax)
      ranges_design_bernoulli_equal_power$y   <- c(brush_power$ymin,
                                                   brush_power$ymax)
    } else {
      ranges_design_bernoulli_equal_power$x   <-
        ranges_design_bernoulli_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_bernoulli_equal_other_dblclick, {
    brush_other                               <-
      input$design_bernoulli_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_bernoulli_equal_other$x   <- c(brush_other$xmin,
                                                   brush_other$xmax)
      ranges_design_bernoulli_equal_other$y   <- c(brush_other$ymin,
                                                   brush_other$ymax)
    } else {
      ranges_design_bernoulli_equal_other$x   <-
        ranges_design_bernoulli_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_bernoulli_shifted_dblclick, {
    brush_shifted                         <-
      input$design_bernoulli_shifted_brush
    if (!is.null(brush_shifted)) {
      ranges_design_bernoulli_shifted$x   <- c(brush_shifted$xmin,
                                               brush_shifted$xmax)
      ranges_design_bernoulli_shifted$y   <- c(brush_shifted$ymin,
                                               brush_shifted$ymax)
    } else {
      ranges_design_bernoulli_shifted$x   <-
        ranges_design_bernoulli_shifted$y <- NULL
    }
  })

  ##### Design (Bernoulli): des_bernoulli() ####################################

  des_bernoulli <- shiny::eventReactive(input$design_bernoulli_update, {
    K                             <- input$design_bernoulli_K
    seq_K                         <- 1:K
    progress                      <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs",
                 value   = 0)
    if (input$design_bernoulli_ratio_type == "equal_all") {
      ratio                       <- rep(1, K)
    } else if (input$design_bernoulli_ratio_type == "equal_exp") {
      ratio                       <- rep(input$design_bernoulli_ratio_1, K)
    } else if (input$design_bernoulli_ratio_type == "unequal") {
      ratio                       <- numeric(K)
      for (i in seq_K) {
        ratio[i]                  <-
          input[[paste0("design_bernoulli_ratio_", i)]]
      }
    } else if (input$design_bernoulli_ratio_type == "root_K") {
      ratio                       <- rep(1/sqrt(K), K)
    }
    if (input$design_bernoulli_ratio_type %in% c("A", "D", "E")) {
      ratio                       <- input$design_bernoulli_ratio_type
      ratio_scenario              <- input$design_bernoulli_ratio_scenario
    } else {
      ratio_scenario              <- "HG"
    }
    design                        <-
      multiarm::des_ma_bern(K              = input$design_bernoulli_K,
                            alpha          = input$design_bernoulli_alpha,
                            beta           = 1 - input$design_bernoulli_beta,
                            pi0            = input$design_bernoulli_pi0,
                            delta1         = input$design_bernoulli_delta1,
                            delta0         = input$design_bernoulli_delta0,
                            ratio          = ratio,
                            correction     = input$design_bernoulli_correction,
                            power          = input$design_bernoulli_power,
                            integer        = input$design_bernoulli_integer,
                            ratio_scenario = ratio_scenario)
    progress$inc(amount  = 0.25 + as.numeric(!input$design_bernoulli_plots),
                 message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_bernoulli_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_bernoulli_summary.html"),
      params        = list(K              = design$K,
                           alpha          = design$alpha,
                           beta           = design$beta,
                           pi0            = design$pi0,
                           delta1         = design$delta1,
                           delta0         = design$delta0,
                           ratio_type     = input$design_bernoulli_ratio_type,
                           ratio_init     = c(input$design_bernoulli_ratio_1,
                                              input$design_bernoulli_ratio_2,
                                              input$design_bernoulli_ratio_3,
                                              input$design_bernoulli_ratio_4,
                                              input$design_bernoulli_ratio_5),
                           ratio_scenario = design$ratio_scenario,
                           ratio          = design$ratio,
                           correction     = design$correction,
                           power          = design$power,
                           integer        = design$integer,
                           large_N        = design$N,
                           small_n        = design$n,
                           opchar         = design$opchar,
                           gamma          = design$gamma,
                           gammaO         = design$gammaO,
                           plots          = input$design_bernoulli_plots)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_bernoulli_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_bernoulli_summary_modified.html")
    )
    design$data_og                <- design$opchar
    if (input$design_bernoulli_plots) {
      alpha                       <- design$alpha
      beta                        <- design$beta
      pi0                         <- design$pi0
      delta0                      <- design$delta0
      delta1                      <- design$delta1
      delta                       <- delta1 - delta0
      density                     <- as.numeric(input$design_bernoulli_density)
      delta_min                   <- -pi0 + 1e-6
      delta_max                   <- 1 - pi0 - 1e-6
      progress$inc(amount  = 0.25,
                   message = "Rendering plots")
      pi                          <- cbind(rep(pi0, density),
                                           matrix(0, nrow = density, ncol = K))
      pi[, 2]                     <- pi0 + c(seq(delta_min, -1e-6,
                                                 length.out = 0.5*density),
                                             seq(1e-6, delta_max,
                                                 length.out = 0.5*density))
      for (k in 3:(K + 1)) {
        pi[, k]                <- pi[, 2]
      }
      opchar_equal_og          <- opchar_ma_bern(design, pi)$opchar
      opchar_equal             <- tidyr::gather(opchar_equal_og, "type", "P",
                                                `Pdis`:`Spec`)
      opchar_equal$type        <- factor(opchar_equal$type,
                                         c("Pdis", "Pcon",
                                           paste0("P", seq_K),
                                           paste0("FWERI", seq_K),
                                           paste0("FWERII", seq_K), "PHER",
                                           "FDR", "pFDR", "FNDR", "Sens",
                                           "Spec"))
      labels_power             <- numeric(K + 2)
      labels_power[1:2]        <- c(parse(text = "italic(P)[dis]"),
                                    parse(text = "italic(P)[con]"))
      for (i in 3:(K + 2)) {
        labels_power[i]        <- parse(text = paste("italic(P)[", i - 2, "]",
                                                     sep = ""))
      }
      colours_power            <- ggthemes::ptol_pal()(2 + K)
      design$equal_power       <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal, type %in% c("Pdis", "Pcon",
                                                         paste0("P", seq_K))),
          ggplot2::aes(x   = pi1,
                       y   = P,
                       col = type)) +
        ggplot2::scale_colour_manual(values = colours_power,
                                     labels = labels_power) +
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
        ggplot2::geom_vline(xintercept = pi0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = pi0 + delta1,
                            linetype   = 2)
      if (K == 2) {
        design$equal_power <- design$equal_power +
          ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                     pi[2], sep = "")))
      } else if (K == 3) {
        design$equal_power <- design$equal_power +
          ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                     pi[2], " = ", pi[3], sep = "")))
      } else {
        design$equal_power <- design$equal_power +
          ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ... = ",
                                     pi[.(K)], sep = "")))
      }
      labels_error             <- numeric(2*K + 1)
      for (i in 1:K) {
        labels_error[i]           <-
          parse(text = paste("italic(FWER)[italic(I)][", i, "]", sep = ""))
        labels_error[K + i]   <-
          parse(text = paste("italic(FWER)[italic(II)][", i, "]", sep = ""))
      }
      labels_error[2*K + 1]   <- parse(text = "italic(PHER)")
      colours_error               <- ggthemes::ptol_pal()(2*K + 1)
      design$equal_error      <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal,
                               (type %in% c(paste0("FWERI", seq_K),
                                            paste0("FWERII", seq_K), "PHER")) &
                                 (pi1 <= pi0)),
          ggplot2::aes(x   = pi1,
                       y   = P,
                       col = type)) +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal,
                               (type %in% c(paste0("FWERI", seq_K),
                                            paste0("FWERII", seq_K), "PHER")) &
                                 (pi1 > pi0)),
          ggplot2::aes(x   = pi1,
                       y   = P,
                       col = type)) +
        ggplot2::scale_colour_manual(values = colours_error, labels = labels_error) +
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
        ggplot2::geom_vline(xintercept = pi0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = pi0 + delta1,
                            linetype   = 2)
      if (K == 2) {
        design$equal_error    <- design$equal_error +
          ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                     pi[2], sep = "")))
      } else if (K == 3) {
        design$equal_error    <- design$equal_error +
          ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                     pi[2], " = ", pi[3], sep = "")))
      } else {
        design$equal_error    <- design$equal_error +
          ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ... = ",
                                     pi[.(K)], sep = "")))
      }
      labels_other             <- c(parse(text = "italic(FDR)"),
                                    parse(text = "italic(pFDR)"),
                                    parse(text = "italic(FNDR)"),
                                    parse(text = "italic(Sensitivity)"),
                                    parse(text = "italic(Specificity)"))
      colours_other            <- ggthemes::ptol_pal()(5)
      design$equal_other   <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal,
                               (type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                            "Spec")) & (pi1 <= pi0)),
          ggplot2::aes(x   = pi1,
                       y   = P,
                       col = type)) +
        ggplot2::geom_line(
          data = dplyr::filter(opchar_equal,
                               (type %in% c("FDR", "pFDR", "FNDR", "Sens",
                                            "Spec")) & (pi1 > pi0)),
          ggplot2::aes(x   = pi1,
                       y   = P,
                       col = type)) +
        ggplot2::scale_colour_manual(values = colours_other,
                                     labels = labels_other) +
        ggplot2::ylab("Rate") +
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
        ggplot2::geom_vline(xintercept = pi0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = pi0 + delta1,
                            linetype   = 2)
      if (K == 2) {
        design$equal_other <- design$equal_other +
          ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                     pi[2], sep = "")))
      } else if (K == 3) {
        design$equal_other <- design$equal_other +
          ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ",
                                     pi[2], " = ", pi[3], sep = "")))
      } else {
        design$equal_other <- design$equal_other +
          ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ", pi[1], " = ... = ",
                                     pi[.(K)], sep = "")))
      }
      progress$inc(amount  = 0.25,
                   message = "Rendering plots")
      opchar_matrix            <- NULL
      for (k in 1:K) {
        pi                     <- cbind(rep(pi0, density),
                                        matrix(0, nrow = density, ncol = K))
        pi[, k + 1]            <- pi0 + seq(delta_min, delta_max,
                                            length.out = density)
        for (l in (1:K)[-k]) {
          pi[, l + 1]          <- pi[, k + 1] - delta
        }
        pi                     <- pi[as.logical(apply(pi >= 0, 1, prod)), ]
        opchar_k               <- opchar_ma_bern(design, pi)$opchar
        opchar_matrix          <- rbind(opchar_matrix,
                                        cbind(as.matrix(opchar_k[, c(k + 1,
                                                               K + 3 + k)]),
                                        as.matrix(opchar_k)))
      }
      opchar_shifted_og        <- tibble::as_tibble(opchar_matrix)
      opchar_shifted           <- opchar_shifted_og[, 1:2]
      colnames(opchar_shifted) <- c("pik", "P")
      opchar_shifted           <-
        dplyr::mutate(opchar_shifted,
                      type = factor(rep(paste0("P", 1:K),
                                        each = nrow(opchar_shifted)/K)))
      opchar_shifted_og           <- opchar_shifted_og[, -(1:2)]
      labels_shifted           <- numeric(K)
      for (i in 1:K) {
        labels_shifted[i]      <- parse(text = paste("italic(P)[", i, "]", sep = ""))
      }
      design$shifted           <- ggplot2::ggplot() +
        ggplot2::geom_line(data = opchar_shifted,
                           ggplot2::aes(x   = pik,
                                        y   = P,
                                        col = type)) +
        ggplot2::scale_colour_manual(values = colours_power[-(1:2)],
                                     labels = labels_shifted) +
        ggplot2::xlab(bquote(paste(pi[0], " = ", .(pi0), ", ... = ",
                                   pi[italic(k)-1], " + ", .(delta), " = ",
                                   pi[italic(k)], " = ", pi[italic(k)+1],
                                   " + ", .(delta), " = ... ", sep = ""))) +
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
        ggplot2::geom_vline(xintercept = pi0,
                            linetype   = 2) +
        ggplot2::geom_vline(xintercept = pi0 + delta1,
                            linetype   = 2)
      colnames(opchar_shifted_og) <- colnames(opchar_equal_og)
      opchar_equal_og             <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(opchar_equal_og, 3))))
      opchar_shifted_og           <-
        as.matrix(dplyr::distinct(
          tibble::as_tibble(round(opchar_shifted_og, 3))))
      design$data                 <-
        data.frame(rbind(design$opchar, opchar_equal_og, opchar_shifted_og),
                   row.names =
                     c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                       paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                       paste0("Equal: #", 1:nrow(opchar_equal_og)),
                       paste0("Shifted: <i>&tau;</i><sub>",
                              rep(seq_K, each = nrow(opchar_shifted_og)/K),
                              "</sub>, #",
                              rep(1:(nrow(opchar_shifted_og)/K), K))))
      colnames(design$data)       <-
        c(paste0("<i>&pi;</i><sub>", c(0, seq_K), "</sub>"),
          paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
          paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
          paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"), "<i>PHER</i>",
          "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>", "<i>Sensitivity</i>",
          "<i>Specificity</i>")
    } else {
      design$data                 <-
        data.frame(design$opchar,
                   row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      colnames(design$data)       <-
        c(paste0("<i>&pi;</i><sub>", c(0, seq_K), "</sub>"),
          paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
          paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
          paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
          "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
          "<i>Sensitivity</i>", "<i>Specificity</i>")
      design$equal_error          <- design$equal_power <- design$equal_other <-
        design$shifted <- design$delta <- NULL
    }
    progress$inc(amount  = 0.25 + as.numeric(!input$design_normal_plots),
                 message = "Outputting results")
    design
  })

  ##### Design (Bernoulli): Value boxes ########################################

  output$design_bernoulli_n_box <- shinydashboard::renderValueBox({
    input$design_bernoulli_update
    shinydashboard::valueBox(
      value    = round(des_bernoulli()$N, 1),
      subtitle = "Total required sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_bernoulli_fwer_box <- shinydashboard::renderValueBox({
    input$design_bernoulli_update
    correction      <- shiny::isolate(input$design_bernoulli_correction)
    if (!(correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                            "none"))) {
      if (des_bernoulli()$opchar$FWERI1[1] <=
          shiny::isolate(input$design_bernoulli_alpha) + 1e-4) {
        icon_choice <- "thumbs-up"
      } else {
        icon_choice <- "thumbs-down"
      }
    } else {
      icon_choice   <- ""
    }
    shinydashboard::valueBox(
      value    = round(des_bernoulli()$opchar$FWERI1[1], 3),
      subtitle = "Maximum familywise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_bernoulli_power_box <- shinydashboard::renderValueBox({
    input$design_bernoulli_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(input$design_bernoulli_power)]
    K                 <- isolate(input$design_bernoulli_K)
    if (input$design_bernoulli_power == "conjunctive") {
      value_power_box <- des_bernoulli()$opchar$Pcon[2]
    } else if (input$design_bernoulli_power == "disjunctive") {
      value_power_box <- des_bernoulli()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(des_bernoulli()$opchar[-(1:2), (K + 4):(2*K + 3)])))
    }
    if (value_power_box >= shiny::isolate(input$design_bernoulli_beta) - 1e-3) {
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

  ##### Design (Bernoulli): Summary ############################################

  output$design_bernoulli_summary <- shiny::renderUI({
    input$design_bernoulli_update
    N <- des_bernoulli()$N
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_bernoulli_summary_modified.html")
      )
    )
  })

  ##### Design (Bernoulli): Table ##############################################

  output$design_bernoulli_table_error <- DT::renderDT({
    DT::datatable(
      round(
        des_bernoulli()$data[, c(1:(des_bernoulli()$K + 1),
                              (2*des_bernoulli()$K + 4):(4*des_bernoulli()$K + 3))],
        3),
      escape        = F,
      fillContainer = T
    )
  })

  output$design_bernoulli_table_other <- DT::renderDT({
    DT::datatable(
      round(
        des_bernoulli()$data[, -((2*des_bernoulli()$K + 4):(4*des_bernoulli()$K + 3))],
        3),
      escape        = F,
      fillContainer = T
    )
  })

  ##### Design (Bernoulli): Plots ##############################################

  output$design_bernoulli_equal_error <- shiny::renderPlot({
    input$design_bernoulli_update
    if (shiny::isolate(input$design_bernoulli_plots)) {
      des_bernoulli()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_bernoulli_equal_error$x,
                                 ylim   = ranges_design_bernoulli_equal_error$y,
                                 expand = F)
    }
  })

  output$design_bernoulli_equal_power <- shiny::renderPlot({
    input$design_bernoulli_update
    if (shiny::isolate(input$design_bernoulli_plots)) {
      des_bernoulli()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_bernoulli_equal_power$x,
                                 ylim   = ranges_design_bernoulli_equal_power$y,
                                 expand = F)
    }
  })

  output$design_bernoulli_equal_other <- shiny::renderPlot({
    input$design_bernoulli_update
    if (shiny::isolate(input$design_bernoulli_plots)) {
      des_bernoulli()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_bernoulli_equal_other$x,
                                 ylim   = ranges_design_bernoulli_equal_other$y,
                                 expand = F)
    }
  })

  output$design_bernoulli_shifted <- shiny::renderPlot({
    input$design_bernoulli_update
    if (shiny::isolate(input$design_bernoulli_plots)) {
      des_bernoulli()$shifted +
        ggplot2::coord_cartesian(xlim   = ranges_design_bernoulli_shifted$x,
                                 ylim   = ranges_design_bernoulli_shifted$y,
                                 expand = F)
    }
  })

  ##### Design (Bernoulli): Report #############################################

  output$design_bernoulli_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_bernoulli_filename, sep = '.',
            switch(input$design_bernoulli_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_bernoulli_report.Rmd")
      file.copy("design_bernoulli_report.Rmd", tempReport, overwrite = T)
      params     <- list(K            = des_bernoulli()$K,
                         alpha        = des_bernoulli()$alpha,
                         beta         = des_bernoulli()$beta,
                         pi0          = des_bernoulli()$pi0,
                         delta1       = des_bernoulli()$delta1,
                         delta0       = des_bernoulli()$delta0,
                         ratio_type   = input$design_bernoulli_ratio_type,
                         ratio_init   = c(input$design_bernoulli_ratio_1,
                                          input$design_bernoulli_ratio_2,
                                          input$design_bernoulli_ratio_3,
                                          input$design_bernoulli_ratio_4,
                                          input$design_bernoulli_ratio_5),
                         ratio_scenario = des_bernoulli()$ratio_scenario,
                         ratio        = des_bernoulli()$ratio,
                         correction   = des_bernoulli()$correction,
                         power        = des_bernoulli()$power,
                         integer      = des_bernoulli()$integer,
                         large_N      = des_bernoulli()$N,
                         small_n      = des_bernoulli()$n,
                         opchar       = des_bernoulli()$opchar,
                         gamma           = des_bernoulli()$gamma,
                         gammaO          = des_bernoulli()$gammaO,
                         plots        = input$design_bernoulli_plots,
                         equal_error  = des_bernoulli()$equal_error,
                         equal_power  = des_bernoulli()$equal_power,
                         equal_other  = des_bernoulli()$equal_other,
                         shifted      = des_bernoulli()$shifted,
                         data         = des_bernoulli()$data_og)
      if (input$design_bernoulli_format == "pdf") {
        format   <- "pdf_document"
      } else if (input$design_bernoulli_format == "html") {
        format   <- "html_document"
      } else {
        format   <- "word_document"
      }
      rmarkdown::render(tempReport,
                        output_format = format,
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )
  ##### Session Info ###########################################################

  output$design_bernoulli_debug <- shiny::renderPrint({
    utils::sessionInfo()
  })

  output$design_normal_debug <- shiny::renderPrint({
    utils::sessionInfo()
  })

  ##### Close set-up ###########################################################

  session$onSessionEnded(stopApp)

}

shiny::shinyApp(ui, server)
