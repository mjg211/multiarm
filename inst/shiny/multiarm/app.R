##### Load required packages ###################################################

library(knitr)
library(magrittr)
library(multiarm)
options(shiny.sanitize.errors = TRUE)

sapply(c("design_dtl_bern_setting.Rmd", "design_dtl_norm_setting.Rmd",
         "design_dtl_pois_setting.Rmd", "design_gs_bern_setting.Rmd",
         "design_gs_norm_setting.Rmd", "design_gs_pois_setting.Rmd",
         "design_ss_bern_setting.Rmd", "design_ss_norm_setting.Rmd",
         "design_ss_pois_setting.Rmd"), knit, quiet = TRUE)

##### UI #######################################################################
ui <- function(request) {
  shinydashboard::dashboardPage(
    ##### Dashboard: Header ####################################################
    shinydashboard::dashboardHeader(
      title      = "multiarm",
      titleWidth = 200
    ),
    ##### Dashboard: Sidebar ###################################################
    shinydashboard::dashboardSidebar(
      width = 200,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text    = "Home",
          tabName = "home",
          icon    = shiny::icon(name = "home")
        ),
        shinydashboard::menuItem(
          text    = "Single-stage",
          tabName = "design_ss",
          icon    = shiny::icon("edit"),
          shinydashboard::menuSubItem(
            text    = "Bernoulli",
            tabName = "design_ss_bern"
          ),
          shinydashboard::menuSubItem(
            text    = "Normal",
            tabName = "design_ss_norm"
          ),
          shinydashboard::menuSubItem(
            text    = "Poisson",
            tabName = "design_ss_pois"
          )
        ),
        shinydashboard::menuItem(
          text    = "Group-sequential",
          tabName = "design_gs",
          icon    = shiny::icon("edit"),
          shinydashboard::menuSubItem(
            text    = "Bernoulli",
            tabName = "design_gs_bern"
          ),
          shinydashboard::menuSubItem(
            text    = "Normal",
            tabName = "design_gs_norm"
          ),
          shinydashboard::menuSubItem(
            text    = "Poisson",
            tabName = "design_gs_pois"
          )
        ),
        shinydashboard::menuItem(
          text    = "Drop-the-losers",
          tabName = "design_dtl",
          icon    = shiny::icon("edit"),
          shinydashboard::menuSubItem(
            text    = "Bernoulli",
            tabName = "design_dtl_bern"
          ),
          shinydashboard::menuSubItem(
            text    = "Normal",
            tabName = "design_dtl_norm"
          ),
          shinydashboard::menuSubItem(
            text    = "Poisson",
            tabName = "design_dtl_pois"
          )
        ),
        shinydashboard::menuItem(
          text    = "About",
          tabName = "about",
          icon    = shiny::icon(name = "question")
        ),
        shinydashboard::menuItem(
          text    = HTML("Source code<sup><i class='fa fa-external-link' ",
                         "style='font-size:8px'></i></sup>"),
          icon    = shiny::icon(name = "file-code-o"),
          href    = "https://github.com/mjg211/multiarm/"
        ),
        id = "sidebar"
      )
    ),
    ##### Dashboard: Body ######################################################
    shinydashboard::dashboardBody(
      #tags$head(includeScript("google-analytics.js")),
      tags$script(
        HTML(
          "var openTab = function(tabName){
           $('a', $('.sidebar')).each(function() {
             if(this.getAttribute('data-value') == tabName) {
               this.click()
             };
           });
        }"
        )
      ),
      rclipboard::rclipboardSetup(),
      shinybusy::add_busy_bar(color = "black"),
      sever::use_sever(),
      shinydashboard::tabItems(
        ##### Tab: Home ########################################################
        shinydashboard::tabItem(
          tabName = "home",
          shinydashboard::box(
            title       = p("multiarm: Design of single- and multi-stage ",
                            "multi-arm clinical trials"),
            status      = "primary",
            width       = 12,
            solidHeader = TRUE,
            shiny::includeMarkdown("home.md")
          )
        ),
        ##### Tab: Single-stage (Bernoulli) ####################################
        shinydashboard::tabItem(
          tabName = "design_ss_bern",
          shiny::includeMarkdown("design_ss_bern_header.md"),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Design setting",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("design_ss_bern_setting.md")
              )
            )
          ),
          ##### Row 1: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "design_ss_bern_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "design_ss_bern_K",
                label   = "Number of experimental treatment arms:",
                min     = 2,
                max     = 5,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_K",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::selectInput(
                inputId  = "design_ss_bern_correction",
                label    = "Multiple comparison correction:",
                choices  =
                  list("Per-hypothesis type-I error-rate control"  =
                         list("No multiple comparison correction" = "none"),
                       "Family-wise error-rate control: Single-step" =
                         list("Bonferroni" = "bonferroni",
                              "Dunnett"    = "dunnett",
                              "Sidak"      = "sidak"),
                       "Family-wise error-rate control: Step-wise" =
                         list("Hochberg"          = "hochberg",
                              "Holm-Bonferroni"   = "holm_bonferroni",
                              "Holm-Sidak"        = "holm_sidak",
                              "Step-down Dunnett" = "step_down_dunnett"),
                       "False discovery rate control"              =
                         list("Benjamini-Hochberg"  = "benjamini_hochberg",
                              "Benjamini-Yekutieli" = "benjamini_yekutieli")),
                selected = "dunnett"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_correction",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::numericInput(
                inputId = "design_ss_bern_alpha",
                label   = "Significance level:",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_alpha",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_ss_bern_power",
                label    = "Type of power to control:",
                choices  = c("Conjunctive" = "conjunctive",
                             "Disjunctive" = "disjunctive",
                             "Marginal"    = "marginal"),
                selected = "marginal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_power",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_ss_bern_beta",
                label   = "Desired power:",
                value   = 0.8,
                min     = 0,
                max     = 1,
                step    = 0.025
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_beta",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_ss_bern_pi0",
                label   = "Control arm response rate:",
                value   = 0.3,
                min     = 0,
                max     = 1,
                step    = 0.1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_pi0",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_ss_bern_delta"),
              shiny::selectInput(
                inputId  = "design_ss_bern_ratio_type",
                label    = "Allocation ratios:",
                choices  =
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
                  content = "design_ratio_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_ss_bern_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_ss_bern_integer",
                label   = "Require integer sample sizes",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_integer",
                  size    = "m",
                  colour  = "black"
                ),
              shinyWidgets::prettySwitch(
                inputId = "design_ss_bern_plots",
                label   = "Plot power curves",
                status  = "info",
                value   = TRUE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_plots",
                  size    = "m",
                  colour  = "black"),
              shiny::uiOutput("design_ss_bern_density"),
              shiny::hr(),
              shiny::actionButton(
                inputId = "design_ss_bern_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::uiOutput("design_ss_bern_warning"),
              shiny::actionButton(
                inputId = "design_ss_bern_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "design_ss_bern_filename",
                label   = "Report filename:",
                value   = "single_stage_bernoulli"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "design_ss_bern_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "design_ss_bern_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("design_ss_bern_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 2: Value box outputs #######################################
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("design_ss_bern_n_box"),
            shinydashboard::valueBoxOutput("design_ss_bern_fwer_box"),
            shinydashboard::valueBoxOutput("design_ss_bern_power_box")
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Code for reproduction",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "right",
                shinycssloaders::withSpinner(
                  shiny::uiOutput("design_ss_bern_clip"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              ),
              shiny::column(
                width = 12,
                align = "left",
                shinycssloaders::withSpinner(
                  shiny::verbatimTextOutput("design_ss_bern_code"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 3-5: Operating characteristics summary ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Operating characteristics summary: Key",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_ss_bern_table_key",
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
                "Operating characteristics summary: Error-rates",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_ss_bern_table_error",
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
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_ss_bern_table_other",
                               height = "500px"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 6 & 7: Operating characteristics plots ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Equal treatment effects: Error-rates",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_bern_equal_error",
                  dblclick = "design_ss_bern_equal_error_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_bern_equal_error_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_bern_equal_power",
                  dblclick = "design_ss_bern_equal_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_bern_equal_power_brush",
                    resetOnNew = TRUE)
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
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_bern_equal_other",
                  dblclick = "design_ss_bern_equal_other_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_bern_equal_other_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Shifted treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_bern_shifted_power",
                  dblclick = "design_ss_bern_shifted_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_bern_shifted_power_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 8: Session information #####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("design_ss_bern_debug")
            )
          )
        ),
        ##### Tab: Single-stage (normal) #######################################
        shinydashboard::tabItem(
          tabName = "design_ss_norm",
          shiny::includeMarkdown("design_ss_norm_header.md"),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Design setting",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("design_ss_norm_setting.md")
              )
            )
          ),
          ##### Row 1: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "design_ss_norm_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "design_ss_norm_K",
                label   = "Number of experimental treatment arms:",
                min     = 2,
                max     = 5,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_K",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::selectInput(
                inputId  = "design_ss_norm_correction",
                label    = "Multiple comparison correction:",
                choices  =
                  list("Per-hypothesis type-I error-rate control"  =
                         list("No multiple comparison correction" = "none"),
                       "Family-wise error-rate control: Single-step" =
                         list("Bonferroni" = "bonferroni",
                              "Dunnett"    = "dunnett",
                              "Sidak"      = "sidak"),
                       "Family-wise error-rate control: Step-wise" =
                         list("Hochberg"          = "hochberg",
                              "Holm-Bonferroni"   = "holm_bonferroni",
                              "Holm-Sidak"        = "holm_sidak",
                              "Step-down Dunnett" = "step_down_dunnett"),
                       "False discovery rate control"              =
                         list("Benjamini-Hochberg"  = "benjamini_hochberg",
                              "Benjamini-Yekutieli" = "benjamini_yekutieli")),
                selected = "dunnett"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_correction",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::numericInput(
                inputId = "design_ss_norm_alpha",
                label   = "Significance level:",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_alpha",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_ss_norm_power",
                label    = "Type of power to control:",
                choices  = c("Conjunctive" = "conjunctive",
                             "Disjunctive" = "disjunctive",
                             "Marginal"    = "marginal"),
                selected = "marginal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_power",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_ss_norm_beta",
                label   = "Desired power:",
                value   = 0.8,
                min     = 0,
                max     = 1,
                step    = 0.025
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_beta",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_ss_norm_delta1",
                label   = "Interesting treatment effect:",
                value   = 0.5,
                min     = 0,
                max     = NA,
                step    = 0.1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_delta1",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_ss_norm_delta0"),
              shiny::selectInput(
                inputId  = "design_ss_norm_sigma_type",
                label    = "Standard deviations:",
                choices  = c("Equal across all arms"          = "equal_all",
                             "Equal across experimental arms" = "equal_exp",
                             "Unequal across all arms"        = "unequal"),
                selected = "equal_all"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_sigma_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_ss_norm_sigma"),
              shiny::selectInput(
                inputId  = "design_ss_norm_ratio_type",
                label    = "Allocation ratios:",
                choices  =
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
                  content = "design_ratio_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_ss_norm_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_ss_norm_integer",
                label   = "Require integer sample sizes",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_integer",
                  size    = "m",
                  colour  = "black"
                ),
              shinyWidgets::prettySwitch(
                inputId = "design_ss_norm_plots",
                label   = "Plot power curves",
                status  = "info",
                value   = TRUE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_plots",
                  size    = "m",
                  colour  = "black"),
              shiny::uiOutput("design_ss_norm_density"),
              shiny::hr(),
              shiny::actionButton(
                inputId = "design_ss_norm_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::uiOutput("design_ss_norm_warning"),
              shiny::actionButton(
                inputId = "design_ss_norm_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "design_ss_norm_filename",
                label   = "Report filename:",
                value   = "single_stage_normal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "design_ss_norm_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "design_ss_norm_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("design_ss_norm_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 2: Value box outputs #######################################
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("design_ss_norm_n_box"),
            shinydashboard::valueBoxOutput("design_ss_norm_fwer_box"),
            shinydashboard::valueBoxOutput("design_ss_norm_power_box")
          ),
          ##### Rows 3-5: Operating characteristics summary ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Operating characteristics summary: Key",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_ss_norm_table_key",
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
              title       = "Operating characteristics summary: Error-rates",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_ss_norm_table_error",
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
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_ss_norm_table_other",
                               height = "500px"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 6 & 7: Operating characteristics plots ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Equal treatment effects: Error-rates",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_norm_equal_error",
                  dblclick = "design_ss_norm_equal_error_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_norm_equal_error_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_norm_equal_power",
                  dblclick = "design_ss_norm_equal_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_norm_equal_power_brush",
                    resetOnNew = TRUE)
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
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_norm_equal_other",
                  dblclick = "design_ss_norm_equal_other_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_norm_equal_other_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Shifted treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_norm_shifted_power",
                  dblclick = "design_ss_norm_shifted_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_norm_shifted_power_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 8: Session information #####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("design_ss_norm_debug")
            )
          )
        ),
        ##### Tab: Single-stage (Poisson) ######################################
        shinydashboard::tabItem(
          tabName = "design_ss_pois",
          shiny::includeMarkdown("design_ss_pois_header.md"),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Design setting",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("design_ss_pois_setting.md")
              )
            )
          ),
          ##### Row 1: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "design_ss_pois_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "design_ss_pois_K",
                label   = "Number of experimental treatment arms:",
                min     = 2,
                max     = 5,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_K",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::selectInput(
                inputId  = "design_ss_pois_correction",
                label    = "Multiple comparison correction:",
                choices  =
                  list("Per-hypothesis type-I error-rate control"  =
                         list("No multiple comparison correction" = "none"),
                       "Family-wise error-rate control: Single-step" =
                         list("Bonferroni" = "bonferroni",
                              "Dunnett"    = "dunnett",
                              "Sidak"      = "sidak"),
                       "Family-wise error-rate control: Step-wise" =
                         list("Hochberg"          = "hochberg",
                              "Holm-Bonferroni"   = "holm_bonferroni",
                              "Holm-Sidak"        = "holm_sidak",
                              "Step-down Dunnett" = "step_down_dunnett"),
                       "False discovery rate control"              =
                         list("Benjamini-Hochberg"  = "benjamini_hochberg",
                              "Benjamini-Yekutieli" = "benjamini_yekutieli")),
                selected = "dunnett"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_correction",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::numericInput(
                inputId = "design_ss_pois_alpha",
                label   = "Significance level:",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_alpha",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_ss_pois_power",
                label    = "Type of power to control:",
                choices  = c("Conjunctive" = "conjunctive",
                             "Disjunctive" = "disjunctive",
                             "Marginal"    = "marginal"),
                selected = "marginal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_power",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_ss_pois_beta",
                label   = "Desired power:",
                value   = 0.8,
                min     = 0,
                max     = 1,
                step    = 0.025
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_beta",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_ss_pois_lambda0",
                label   = "Control arm event rate:",
                value   = 5,
                min     = 0,
                step    = 0.1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_lambda0",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_ss_pois_delta"),
              shiny::selectInput(
                inputId  = "design_ss_pois_ratio_type",
                label    = "Allocation ratios:",
                choices  =
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
                  content = "design_ratio_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_ss_pois_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_ss_pois_integer",
                label   = "Require integer sample sizes",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_integer",
                  size    = "m",
                  colour  = "black"
                ),
              shinyWidgets::prettySwitch(
                inputId = "design_ss_pois_plots",
                label   = "Plot power curves",
                status  = "info",
                value   = TRUE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_plots",
                  size    = "m",
                  colour  = "black"),
              shiny::uiOutput("design_ss_pois_density"),
              shiny::hr(),
              shiny::actionButton(
                inputId = "design_ss_pois_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::uiOutput("design_ss_pois_warning"),
              shiny::actionButton(
                inputId = "design_ss_pois_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "design_ss_pois_filename",
                label   = "Report filename:",
                value   = "single_stage_poisson"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "design_ss_pois_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "design_ss_pois_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("design_ss_pois_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 2: Value box outputs #######################################
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("design_ss_pois_n_box"),
            shinydashboard::valueBoxOutput("design_ss_pois_fwer_box"),
            shinydashboard::valueBoxOutput("design_ss_pois_power_box")
          ),
          ##### Rows 3-5: Operating characteristics summary ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Operating characteristics summary: Key",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_ss_pois_table_key",
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
                "Operating characteristics summary: Error-rates",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_ss_pois_table_error",
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
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_ss_pois_table_other",
                               height = "500px"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 6 & 7: Operating characteristics plots ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Equal treatment effects: Error-rates",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_pois_equal_error",
                  dblclick = "design_ss_pois_equal_error_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_pois_equal_error_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_pois_equal_power",
                  dblclick = "design_ss_pois_equal_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_pois_equal_power_brush",
                    resetOnNew = TRUE)
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
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_pois_equal_other",
                  dblclick = "design_ss_pois_equal_other_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_pois_equal_other_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Shifted treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_ss_pois_shifted_power",
                  dblclick = "design_ss_pois_shifted_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_ss_pois_shifted_power_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 8: Session information #####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("design_ss_pois_debug")
            )
          )
        ),
        ##### Tab: Group-sequential (Bernoulli) ################################
        shinydashboard::tabItem(
          tabName = "design_gs_bern",
          shiny::includeMarkdown("design_gs_bern_header.md"),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Design setting",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("design_gs_bern_setting.md")
              )
            )
          ),
          ##### Row 1: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "design_gs_bern_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "design_gs_bern_K",
                label   = "Initial number of experimental treatment arms:",
                min     = 2,
                max     = 5,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_K",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::sliderInput(
                inputId = "design_gs_bern_J",
                label   = "Number of stages:",
                min     = 2,
                max     = 4,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_J",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::selectInput(
                inputId  = "design_gs_bern_stopping",
                label    = "Stopping rule:",
                choices  = c("Simultaneous" = "simultaneous",
                             "Separate"     = "separate"),
                selected = "separate"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_stopping",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_gs_bern_swss",
                label    = "Stage-wise sample size:",
                choices  = c("Fixed"    = "fixed",
                             "Variable" = "variable"),
                selected = "variable"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_swss",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_gs_bern_lower",
                label    = "Lower stopping boundaries:",
                choices  = c("Fixed"           = "fixed",
                             "O'Brien-Fleming" = "obf",
                             "Pocock"          = "pocock",
                             "Triangular"      = "triangular"),
                selected = "obf"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_lower",
                  size    = "m",
                  colour  = "black"
                ),
              uiOutput("design_gs_bern_lower_fixed"),
              shiny::selectInput(
                inputId  = "design_gs_bern_upper",
                label    = "Upper stopping boundaries:",
                choices  = c("Fixed"           = "fixed",
                             "O'Brien-Fleming" = "obf",
                             "Pocock"          = "pocock",
                             "Triangular"      = "triangular"),
                selected = "obf"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_upper",
                  size    = "m",
                  colour  = "black"
                ),
              uiOutput("design_gs_bern_upper_fixed"),
              shiny::numericInput(
                inputId = "design_gs_bern_alpha",
                label   = "Family-wise error-rate:",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_alpha",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_gs_bern_power",
                label    = "Type of power to control:",
                choices  = c("Conjunctive" = "conjunctive",
                             "Disjunctive" = "disjunctive",
                             "Marginal"    = "marginal"),
                selected = "marginal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_power",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_gs_bern_beta",
                label   = "Desired power:",
                value   = 0.8,
                min     = 0,
                max     = 1,
                step    = 0.025
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_beta",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_gs_bern_pi0",
                label   = "Control arm response rate:",
                value   = 0.3,
                min     = 0,
                max     = 1,
                step    = 0.1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_pi0",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_bern_delta"),
              shiny::selectInput(
                inputId  = "design_gs_bern_ratio_type",
                label    = "Allocation ratios:",
                choices  =
                  list("Equal across all arms"          = "equal_all",
                       "Equal across experimental arms" = "equal_exp",
                       "root-K rule"                    = "root_K"),
                selected = "equal_all"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_ratio_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_bern_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_gs_bern_integer",
                label   = "Require integer sample sizes",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_integer",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_bern_warning_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_gs_bern_plots",
                label   = "Plot power curves",
                status  = "info",
                value   = TRUE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_plots",
                  size    = "m",
                  colour  = "black"),
              shiny::uiOutput("design_gs_bern_density"),
              shiny::hr(),
              shiny::actionButton(
                inputId = "design_gs_bern_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::uiOutput("design_gs_bern_warning"),
              shiny::actionButton(
                inputId = "design_gs_bern_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "design_gs_bern_filename",
                label   = "Report filename:",
                value   = "multi_stage_group_sequential_bernoulli"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "design_gs_bern_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "design_gs_bern_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("design_gs_bern_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 2: Value box outputs #######################################
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("design_gs_bern_n_box"),
            shinydashboard::valueBoxOutput("design_gs_bern_fwer_box"),
            shinydashboard::valueBoxOutput("design_gs_bern_power_box")
          ),
          ##### Row 3: Stopping boundaries plot ################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Stopping boundaries",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput("design_gs_bern_stopping_boundaries"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Rows 4-6: Operating characteristics summary ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Operating characteristics summary: Key",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_gs_bern_table_key",
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
                "Operating characteristics summary: Error-rates",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_gs_bern_table_error",
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
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_gs_bern_table_other",
                               height = "500px"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 7-10: Operating characteristics plots #####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Equal treatment effects: Error-rates",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_bern_equal_error",
                  dblclick = "design_gs_bern_equal_error_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_bern_equal_error_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_bern_equal_power",
                  dblclick = "design_gs_bern_equal_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_bern_equal_power_brush",
                    resetOnNew = TRUE)
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
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_bern_equal_other",
                  dblclick = "design_gs_bern_equal_other_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_bern_equal_other_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Sample size",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_bern_equal_sample_size",
                  dblclick = "design_gs_bern_equal_sample_size_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_bern_equal_sample_size_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Shifted treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_bern_shifted_power",
                  dblclick = "design_gs_bern_shifted_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_bern_shifted_power_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Shifted treatment effects: Sample size",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_bern_shifted_sample_size",
                  dblclick = "design_gs_bern_shifted_sample_size_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_bern_shifted_sample_size_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Sample size distribution",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_bern_pmf_N",
                  dblclick = "design_gs_bern_pmf_N_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_bern_pmf_N_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 11: Session information ####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("design_gs_bern_debug")
            )
          )
        ),
        ##### Tab: Group-sequential (normal) ###################################
        shinydashboard::tabItem(
          tabName = "design_gs_norm",
          shiny::includeMarkdown("design_gs_norm_header.md"),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Design setting",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("design_ss_norm_setting.md")
              )
            )
          ),
          ##### Row 1: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "design_gs_norm_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "design_gs_norm_K",
                label   = "Initial number of experimental treatment arms:",
                min     = 2,
                max     = 5,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_K",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::sliderInput(
                inputId = "design_gs_norm_J",
                label   = "Number of stages:",
                min     = 2,
                max     = 4,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_J",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::selectInput(
                inputId  = "design_gs_norm_stopping",
                label    = "Stopping rule:",
                choices  = c("Simultaneous" = "simultaneous",
                             "Separate"     = "separate"),
                selected = "separate"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_stopping",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_gs_norm_swss",
                label    = "Stage-wise sample size:",
                choices  = c("Fixed"    = "fixed",
                             "Variable" = "variable"),
                selected = "variable"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_swss",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_gs_norm_lower",
                label    = "Lower stopping boundaries:",
                choices  = c("Fixed"           = "fixed",
                             "O'Brien-Fleming" = "obf",
                             "Pocock"          = "pocock",
                             "Triangular"      = "triangular"),
                selected = "obf"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_lower",
                  size    = "m",
                  colour  = "black"
                ),
              uiOutput("design_gs_norm_lower_fixed"),
              shiny::selectInput(
                inputId  = "design_gs_norm_upper",
                label    = "Upper stopping boundaries:",
                choices  = c("Fixed"           = "fixed",
                             "O'Brien-Fleming" = "obf",
                             "Pocock"          = "pocock",
                             "Triangular"      = "triangular"),
                selected = "obf"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_upper",
                  size    = "m",
                  colour  = "black"
                ),
              uiOutput("design_gs_norm_upper_fixed"),
              shiny::numericInput(
                inputId = "design_gs_norm_alpha",
                label   = "Family-wise error-rate:",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_alpha",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_gs_norm_power",
                label    = "Type of power to control:",
                choices  = c("Conjunctive" = "conjunctive",
                             "Disjunctive" = "disjunctive",
                             "Marginal"    = "marginal"),
                selected = "marginal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_power",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_gs_norm_beta",
                label   = "Desired power:",
                value   = 0.8,
                min     = 0,
                max     = 1,
                step    = 0.025
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_beta",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_gs_norm_delta1",
                label   = "Interesting treatment effect:",
                value   = 0.5,
                min     = 0,
                max     = NA,
                step    = 0.1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_delta1",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_norm_delta0"),
              shiny::selectInput(
                inputId  = "design_gs_norm_sigma_type",
                label    = "Standard deviations:",
                choices  = c("Equal across all arms"          = "equal_all",
                             "Equal across experimental arms" = "equal_exp"),
                selected = "equal_all"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_sigma_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_norm_sigma"),
              shiny::selectInput(
                inputId  = "design_gs_norm_ratio_type",
                label    = "Allocation ratios:",
                choices  =
                  list("Equal across all arms"          = "equal_all",
                       "Equal across experimental arms" = "equal_exp",
                       "root-K rule"                    = "root_K"),
                selected = "equal_all"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_ratio_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_norm_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_gs_norm_integer",
                label   = "Require integer sample sizes",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_integer",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_norm_warning_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_gs_norm_plots",
                label   = "Plot power curves",
                status  = "info",
                value   = TRUE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_plots",
                  size    = "m",
                  colour  = "black"),
              shiny::uiOutput("design_gs_norm_density"),
              shiny::hr(),
              shiny::actionButton(
                inputId = "design_gs_norm_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::uiOutput("design_gs_norm_warning"),
              shiny::actionButton(
                inputId = "design_gs_norm_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "design_gs_norm_filename",
                label   = "Report filename:",
                value   = "multi_stage_group_sequential_normal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "design_gs_norm_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "design_gs_norm_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("design_gs_norm_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 2: Value box outputs #######################################
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("design_gs_norm_n_box"),
            shinydashboard::valueBoxOutput("design_gs_norm_fwer_box"),
            shinydashboard::valueBoxOutput("design_gs_norm_power_box")
          ),
          ##### Row 3: Stopping boundaries plot ################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Stopping boundaries",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput("design_gs_norm_stopping_boundaries"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Rows 4-6: Operating characteristics summary ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Operating characteristics summary: Key",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_gs_norm_table_key",
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
                "Operating characteristics summary: Error-rates",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_gs_norm_table_error",
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
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_gs_norm_table_other",
                               height = "500px"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 7-10: Operating characteristics plots #####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Equal treatment effects: Error-rates",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_norm_equal_error",
                  dblclick = "design_gs_norm_equal_error_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_norm_equal_error_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_norm_equal_power",
                  dblclick = "design_gs_norm_equal_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_norm_equal_power_brush",
                    resetOnNew = TRUE)
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
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_norm_equal_other",
                  dblclick = "design_gs_norm_equal_other_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_norm_equal_other_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Sample size",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_norm_equal_sample_size",
                  dblclick = "design_gs_norm_equal_sample_size_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_norm_equal_sample_size_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Shifted treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_norm_shifted_power",
                  dblclick = "design_gs_norm_shifted_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_norm_shifted_power_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Shifted treatment effects: Sample size",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_norm_shifted_sample_size",
                  dblclick = "design_gs_norm_shifted_sample_size_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_norm_shifted_sample_size_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Sample size distribution",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_norm_pmf_N",
                  dblclick = "design_gs_norm_pmf_N_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_norm_pmf_N_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 11: Session information #####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("design_gs_norm_debug")
            )
          )
        ),
        ##### Tab: Group-sequential (Poisson) ##################################
        shinydashboard::tabItem(
          tabName = "design_gs_pois",
          shiny::includeMarkdown("design_gs_pois_header.md"),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Design setting",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("design_gs_pois_setting.md")
              )
            )
          ),
          ##### Row 1: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "design_gs_pois_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "design_gs_pois_K",
                label   = "Initial number of experimental treatment arms:",
                min     = 2,
                max     = 5,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_K",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::sliderInput(
                inputId = "design_gs_pois_J",
                label   = "Number of stages:",
                min     = 2,
                max     = 4,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_J",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::selectInput(
                inputId  = "design_gs_pois_stopping",
                label    = "Stopping rule:",
                choices  = c("Simultaneous" = "simultaneous",
                             "Separate"     = "separate"),
                selected = "separate"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_stopping",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_gs_pois_swss",
                label    = "Stage-wise sample size:",
                choices  = c("Fixed"    = "fixed",
                             "Variable" = "variable"),
                selected = "variable"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_swss",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_gs_pois_lower",
                label    = "Lower stopping boundaries:",
                choices  = c("Fixed"           = "fixed",
                             "O'Brien-Fleming" = "obf",
                             "Pocock"          = "pocock",
                             "Triangular"      = "triangular"),
                selected = "obf"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_lower",
                  size    = "m",
                  colour  = "black"
                ),
              uiOutput("design_gs_pois_lower_fixed"),
              shiny::selectInput(
                inputId  = "design_gs_pois_upper",
                label    = "Upper stopping boundaries:",
                choices  = c("Fixed"           = "fixed",
                             "O'Brien-Fleming" = "obf",
                             "Pocock"          = "pocock",
                             "Triangular"      = "triangular"),
                selected = "obf"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_upper",
                  size    = "m",
                  colour  = "black"
                ),
              uiOutput("design_gs_pois_upper_fixed"),
              shiny::numericInput(
                inputId = "design_gs_pois_alpha",
                label   = "Family-wise error-rate:",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_alpha",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_gs_pois_power",
                label    = "Type of power to control:",
                choices  = c("Conjunctive" = "conjunctive",
                             "Disjunctive" = "disjunctive",
                             "Marginal"    = "marginal"),
                selected = "marginal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_power",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_gs_pois_beta",
                label   = "Desired power:",
                value   = 0.8,
                min     = 0,
                max     = 1,
                step    = 0.025
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_beta",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_gs_pois_lambda0",
                label   = "Control arm event rate:",
                value   = 5,
                min     = 0,
                step    = 0.1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_lambda0",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_pois_delta"),
              shiny::selectInput(
                inputId  = "design_gs_pois_ratio_type",
                label    = "Allocation ratios:",
                choices  =
                  list("Equal across all arms"          = "equal_all",
                       "Equal across experimental arms" = "equal_exp",
                       "root-K rule"                    = "root_K"),
                selected = "equal_all"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_ratio_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_pois_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_gs_pois_integer",
                label   = "Require integer sample sizes",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_integer",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_gs_pois_warning_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_gs_pois_plots",
                label   = "Plot power curves",
                status  = "info",
                value   = TRUE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_plots",
                  size    = "m",
                  colour  = "black"),
              shiny::uiOutput("design_gs_pois_density"),
              shiny::hr(),
              shiny::actionButton(
                inputId = "design_gs_pois_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::uiOutput("design_gs_pois_warning"),
              shiny::actionButton(
                inputId = "design_gs_pois_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "design_gs_pois_filename",
                label   = "Report filename:",
                value   = "multi_stage_group_sequential_poisson"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "design_gs_pois_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "design_gs_pois_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("design_gs_pois_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 2: Value box outputs #######################################
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("design_gs_pois_n_box"),
            shinydashboard::valueBoxOutput("design_gs_pois_fwer_box"),
            shinydashboard::valueBoxOutput("design_gs_pois_power_box")
          ),
          ##### Row 3: Stopping boundaries plot ################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Stopping boundaries",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput("design_gs_pois_stopping_boundaries"),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Rows 4-6: Operating characteristics summary ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Operating characteristics summary: Key",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_gs_pois_table_key",
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
                "Operating characteristics summary: Error-rates",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_gs_pois_table_error",
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
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_gs_pois_table_other",
                               height = "500px"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 7-10: Operating characteristics plots #####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Equal treatment effects: Error-rates",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_pois_equal_error",
                  dblclick = "design_gs_pois_equal_error_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_pois_equal_error_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_pois_equal_power",
                  dblclick = "design_gs_pois_equal_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_pois_equal_power_brush",
                    resetOnNew = TRUE)
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
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_pois_equal_other",
                  dblclick = "design_gs_pois_equal_other_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_pois_equal_other_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Sample size",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_pois_equal_sample_size",
                  dblclick = "design_gs_pois_equal_sample_size_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_pois_equal_sample_size_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Shifted treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_pois_shifted_power",
                  dblclick = "design_gs_pois_shifted_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_pois_shifted_power_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Shifted treatment effects: Sample size",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_pois_shifted_sample_size",
                  dblclick = "design_gs_pois_shifted_sample_size_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_pois_shifted_sample_size_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Sample size distribution",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_gs_pois_pmf_N",
                  dblclick = "design_gs_pois_pmf_N_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_gs_pois_pmf_N_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 11: Session information ####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("design_gs_pois_debug")
            )
          )
        ),
        ##### Tab: Drop-the-losers (Bernoulli) #################################
        shinydashboard::tabItem(
          tabName = "design_dtl_bern",
          shiny::includeMarkdown("design_dtl_bern_header.md"),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Design setting",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("design_dtl_bern_setting.md")
              )
            )
          ),
          ##### Row 1: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "design_dtl_bern_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "design_dtl_bern_K",
                label   = "Initial number of experimental treatment arms:",
                min     = 2,
                max     = 5,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_K",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_bern_J"),
              shiny::uiOutput("design_dtl_bern_Kv_2"),
              shiny::uiOutput("design_dtl_bern_Kv_3"),
              shiny::uiOutput("design_dtl_bern_Kv_4"),
              shiny::selectInput(
                inputId  = "design_dtl_bern_swss",
                label    = "Stage-wise sample size:",
                choices  = c("Fixed"    = "fixed",
                             "Variable" = "variable"),
                selected = "variable"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_swss",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_dtl_bern_alpha",
                label   = "Family-wise error-rate:",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_alpha",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_dtl_bern_power",
                label    = "Type of power to control:",
                choices  = c("Disjunctive" = "disjunctive",
                             "Marginal"    = "marginal"),
                selected = "marginal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_power",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_dtl_bern_beta",
                label   = "Desired power:",
                value   = 0.8,
                min     = 0,
                max     = 1,
                step    = 0.025
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_beta",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_dtl_bern_pi0",
                label   = "Control arm response rate:",
                value   = 0.3,
                min     = 0,
                max     = 1,
                step    = 0.1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_pi0",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_bern_delta"),
              shiny::selectInput(
                inputId  = "design_dtl_bern_ratio_type",
                label    = "Allocation ratios:",
                choices  =
                  list("Equal across all arms"          = "equal_all",
                       "Equal across experimental arms" = "equal_exp",
                       "root-K rule"                    = "root_K"),
                selected = "equal_all"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_ratio_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_bern_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_dtl_bern_integer",
                label   = "Require integer sample sizes",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_integer",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_bern_warning_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_dtl_bern_plots",
                label   = "Plot power curves",
                status  = "info",
                value   = TRUE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_plots",
                  size    = "m",
                  colour  = "black"),
              shiny::uiOutput("design_dtl_bern_density"),
              shiny::hr(),
              shiny::actionButton(
                inputId = "design_dtl_bern_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::uiOutput("design_dtl_bern_warning"),
              shiny::actionButton(
                inputId = "design_dtl_bern_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "design_dtl_bern_filename",
                label   = "Report filename:",
                value   = "multi_stage_drop_the_losers_bernoulli"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "design_dtl_bern_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "design_dtl_bern_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("design_dtl_bern_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 2: Value box outputs #######################################
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("design_dtl_bern_n_box"),
            shinydashboard::valueBoxOutput("design_dtl_bern_fwer_box"),
            shinydashboard::valueBoxOutput("design_dtl_bern_power_box")
          ),
          ##### Rows 3-5: Operating characteristics summary ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Operating characteristics summary: Key",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_dtl_bern_table_key",
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
                "Operating characteristics summary: Error-rates",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_dtl_bern_table_error",
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
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_dtl_bern_table_other",
                               height = "500px"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 6 & 7: Operating characteristics plots ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Equal treatment effects: Error-rates",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_bern_equal_error",
                  dblclick = "design_dtl_bern_equal_error_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_bern_equal_error_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_bern_equal_power",
                  dblclick = "design_dtl_bern_equal_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_bern_equal_power_brush",
                    resetOnNew = TRUE)
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
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_bern_equal_other",
                  dblclick = "design_dtl_bern_equal_other_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_bern_equal_other_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Shifted treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_bern_shifted_power",
                  dblclick = "design_dtl_bern_shifted_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_bern_shifted_power_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 8: Session information #####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("design_dtl_bern_debug")
            )
          )
        ),
        ##### Tab: Drop-the-losers (normal) ####################################
        shinydashboard::tabItem(
          tabName = "design_dtl_norm",
          shiny::includeMarkdown("design_dtl_norm_header.md"),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Design setting",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("design_dtl_norm_setting.md")
              )
            )
          ),
          ##### Row 1: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "design_dtl_norm_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "design_dtl_norm_K",
                label   = "Initial number of experimental treatment arms:",
                min     = 2,
                max     = 5,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_K",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_norm_J"),
              shiny::uiOutput("design_dtl_norm_Kv_2"),
              shiny::uiOutput("design_dtl_norm_Kv_3"),
              shiny::uiOutput("design_dtl_norm_Kv_4"),
              shiny::selectInput(
                inputId  = "design_dtl_norm_swss",
                label    = "Stage-wise sample size:",
                choices  = c("Fixed"    = "fixed",
                             "Variable" = "variable"),
                selected = "variable"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_swss",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_dtl_norm_alpha",
                label   = "Family-wise error-rate:",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_alpha",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_dtl_norm_power",
                label    = "Type of power to control:",
                choices  = c("Disjunctive" = "disjunctive",
                             "Marginal"    = "marginal"),
                selected = "marginal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_power",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_dtl_norm_beta",
                label   = "Desired power:",
                value   = 0.8,
                min     = 0,
                max     = 1,
                step    = 0.025
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_beta",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_dtl_norm_delta1",
                label   = "Interesting treatment effect:",
                value   = 0.5,
                min     = 0,
                max     = NA,
                step    = 0.1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_delta1",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_norm_delta0"),
              shiny::selectInput(
                inputId  = "design_dtl_norm_sigma_type",
                label    = "Standard deviations:",
                choices  = c("Equal across all arms"          = "equal_all",
                             "Equal across experimental arms" = "equal_exp"),
                selected = "equal_all"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_sigma_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_norm_sigma"),
              shiny::selectInput(
                inputId  = "design_dtl_norm_ratio_type",
                label    = "Allocation ratios:",
                choices  =
                  list("Equal across all arms"          = "equal_all",
                       "Equal across experimental arms" = "equal_exp",
                       "root-K rule"                    = "root_K"),
                selected = "equal_all"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_ratio_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_norm_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_dtl_norm_integer",
                label   = "Require integer sample sizes",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_integer",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_norm_warning_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_dtl_norm_plots",
                label   = "Plot power curves",
                status  = "info",
                value   = TRUE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_plots",
                  size    = "m",
                  colour  = "black"),
              shiny::uiOutput("design_dtl_norm_density"),
              shiny::hr(),
              shiny::actionButton(
                inputId = "design_dtl_norm_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::uiOutput("design_dtl_norm_warning"),
              shiny::actionButton(
                inputId = "design_dtl_norm_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "design_dtl_norm_filename",
                label   = "Report filename:",
                value   = "multi_stage_drop_the_losers_normal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "design_dtl_norm_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "design_dtl_norm_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("design_dtl_norm_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 2: Value box outputs #######################################
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("design_dtl_norm_n_box"),
            shinydashboard::valueBoxOutput("design_dtl_norm_fwer_box"),
            shinydashboard::valueBoxOutput("design_dtl_norm_power_box")
          ),
          ##### Rows 3-5: Operating characteristics summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Operating characteristics summary: Key",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_dtl_norm_table_key",
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
                "Operating characteristics summary: Error-rates",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_dtl_norm_table_error",
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
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_dtl_norm_table_other",
                               height = "500px"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 6 & 7: Operating characteristics plots ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Equal treatment effects: Error-rates",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_norm_equal_error",
                  dblclick = "design_dtl_norm_equal_error_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_norm_equal_error_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_norm_equal_power",
                  dblclick = "design_dtl_norm_equal_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_norm_equal_power_brush",
                    resetOnNew = TRUE)
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
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_norm_equal_other",
                  dblclick = "design_dtl_norm_equal_other_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_norm_equal_other_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Shifted treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_norm_shifted_power",
                  dblclick = "design_dtl_norm_shifted_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_norm_shifted_power_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 8: Session information #####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("design_dtl_norm_debug")
            )
          )
        ),
        ##### Tab: Drop-the-losers (Poisson) ###################################
        shinydashboard::tabItem(
          tabName = "design_dtl_pois",
          shiny::includeMarkdown("design_dtl_pois_header.md"),
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Design setting",
              width       = 12,
              solidHeader = TRUE,
              status      = "primary",
              shiny::withMathJax(
                shiny::includeMarkdown("design_dtl_pois_setting.md")
              )
            )
          ),
          ##### Row 1: Design parameters & Design summary ######################
          shiny::fluidRow(
            shinydashboard::box(
              shiny::withMathJax(),
              shinyalert::useShinyalert(),
              shinyFeedback::useShinyFeedback(),
              shinyjs::useShinyjs(),
              id          = "design_dtl_pois_parameters",
              title       = "Design parameters",
              width       = 4,
              solidHeader = TRUE,
              status      = "primary",
              tags$style(type = "text/css",
                         ".irs-grid-pol.small {height: 0px;}"),
              shiny::sliderInput(
                inputId = "design_dtl_pois_K",
                label   = "Initial number of experimental treatment arms:",
                min     = 2,
                max     = 5,
                value   = 2,
                step    = 1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_K",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_pois_J"),
              shiny::uiOutput("design_dtl_pois_Kv_2"),
              shiny::uiOutput("design_dtl_pois_Kv_3"),
              shiny::uiOutput("design_dtl_pois_Kv_4"),
              shiny::selectInput(
                inputId  = "design_dtl_pois_swss",
                label    = "Stage-wise sample size:",
                choices  = c("Fixed"    = "fixed",
                             "Variable" = "variable"),
                selected = "variable"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_swss",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_dtl_pois_alpha",
                label   = "Family-wise error-rate:",
                value   = 0.05,
                min     = 0,
                max     = 1,
                step    = 0.01
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_alpha",
                  size    = "m",
                  colour  = "black"),
              shiny::selectInput(
                inputId  = "design_dtl_pois_power",
                label    = "Type of power to control:",
                choices  = c("Disjunctive" = "disjunctive",
                             "Marginal"    = "marginal"),
                selected = "marginal"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_power",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_dtl_pois_beta",
                label   = "Desired power:",
                value   = 0.8,
                min     = 0,
                max     = 1,
                step    = 0.025
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_beta",
                  size    = "m",
                  colour  = "black"),
              shiny::numericInput(
                inputId = "design_dtl_pois_lambda0",
                label   = "Control arm event rate:",
                value   = 5,
                min     = 0,
                step    = 0.1
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_lambda0",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_pois_delta"),
              shiny::selectInput(
                inputId  = "design_dtl_pois_ratio_type",
                label    = "Allocation ratios:",
                choices  =
                  list("Equal across all arms"          = "equal_all",
                       "Equal across experimental arms" = "equal_exp",
                       "root-K rule"                    = "root_K"),
                selected = "equal_all"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_ratio_type",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_pois_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_dtl_pois_integer",
                label   = "Require integer sample sizes",
                status  = "info",
                value   = FALSE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_integer",
                  size    = "m",
                  colour  = "black"
                ),
              shiny::uiOutput("design_dtl_pois_warning_ratio"),
              shinyWidgets::prettySwitch(
                inputId = "design_dtl_pois_plots",
                label   = "Plot power curves",
                status  = "info",
                value   = TRUE,
                slim    = TRUE
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_plots",
                  size    = "m",
                  colour  = "black"),
              shiny::uiOutput("design_dtl_pois_density"),
              shiny::hr(),
              shiny::actionButton(
                inputId = "design_dtl_pois_reset",
                label   = "  Reset inputs  ",
                icon    = shiny::icon(name = "eraser"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::uiOutput("design_dtl_pois_warning"),
              shiny::actionButton(
                inputId = "design_dtl_pois_update",
                label   = "  Update outputs  ",
                icon    = shiny::icon(name = "check-square-o"),
                width   = "100%"
              ),
              shiny::hr(),
              shiny::textInput(
                inputId = "design_dtl_pois_filename",
                label   = "Report filename:",
                value   = "multi_stage_drop_the_losers_poisson"
              ) %>%
                shinyhelper::helper(
                  type    = "markdown",
                  title   = "",
                  content = "design_filename",
                  size    = "m",
                  colour  = "black"
                ),
              tags$head(tags$style(".full_width{width:100%;}")),
              shiny::radioButtons(
                inputId  = "design_dtl_pois_format",
                label    = "Download format",
                choices  = c("PDF"  = "pdf",
                             "HTML" = "html",
                             "Word" = "word"),
                selected = "pdf",
                inline   = TRUE
              ),
              shiny::downloadButton(
                outputId = "design_dtl_pois_report",
                label    = "  Download report  ",
                class    = "full_width"
              )
            ),
            shinydashboard::box(
              title       = "Design summary",
              width       = 8,
              solidHeader = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::withMathJax(
                  shiny::htmlOutput("design_dtl_pois_summary")
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 2: Value box outputs #######################################
          shiny::fluidRow(
            shinydashboard::valueBoxOutput("design_dtl_pois_n_box"),
            shinydashboard::valueBoxOutput("design_dtl_pois_fwer_box"),
            shinydashboard::valueBoxOutput("design_dtl_pois_power_box")
          ),
          ##### Rows 3-5: Operating characteristics summary ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Operating characteristics summary: Key",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_dtl_pois_table_key",
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
                "Operating characteristics summary: Error-rates",
              width       = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_dtl_pois_table_error",
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
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed   = TRUE,
              status      = "primary",
              shiny::column(
                width = 12,
                align = "center",
                shinycssloaders::withSpinner(
                  DT::DTOutput("design_dtl_pois_table_other",
                               height = "500px"),
                  type  = 6,
                  color = "#3C8DBC",
                  size  = 1/3
                )
              )
            )
          ),
          ##### Rows 6 & 7: Operating characteristics plots ####################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Equal treatment effects: Error-rates",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_pois_equal_error",
                  dblclick = "design_dtl_pois_equal_error_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_pois_equal_error_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Equal treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_pois_equal_power",
                  dblclick = "design_dtl_pois_equal_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_pois_equal_power_brush",
                    resetOnNew = TRUE)
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
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_pois_equal_other",
                  dblclick = "design_dtl_pois_equal_other_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_pois_equal_other_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            ),
            shinydashboard::box(
              title       = "Shifted treatment effects: Power",
              width       = 6,
              solidHeader = TRUE,
              collapsible = TRUE,
              status      = "primary",
              shinycssloaders::withSpinner(
                shiny::plotOutput(
                  "design_dtl_pois_shifted_power",
                  dblclick = "design_dtl_pois_shifted_power_dblclick",
                  brush    = shiny::brushOpts(
                    id         = "design_dtl_pois_shifted_power_brush",
                    resetOnNew = TRUE)
                ),
                type  = 6,
                color = "#3C8DBC",
                size  = 1/3
              )
            )
          ),
          ##### Row 8: Session information #####################################
          shiny::fluidRow(
            shinydashboard::box(
              title       = "Session Information",
              status      = "primary",
              solidHeader = TRUE,
              width       = 12,
              collapsible = TRUE,
              collapsed   = TRUE,
              shiny::verbatimTextOutput("design_dtl_pois_debug")
            )
          )
        ),
        ##### Tab: About #######################################################
        shinydashboard::tabItem(
          tabName = "about",
          shiny::fluidRow(
            shinydashboard::box(
              title       = "About",
              status      = "primary",
              width       = 12,
              solidHeader = TRUE,
              shiny::includeMarkdown("about.md")
            ),
            shinydashboard::box(
              title       = "App timeline",
              status      = "primary",
              width       = 12,
              solidHeader = TRUE,
              shinydashboardPlus::timelineBlock(
                shinydashboardPlus::timelineEnd(color = "gray"),
                shinydashboardPlus::timelineItem(
                  title = strong("SCT Webinar"),
                  icon  = "laptop",
                  color = "gray",
                  time  = "Feb 2021",
                  p("The latest functionality for multi-stage designs will be",
                    "presented during an upcoming Society for Clinical Trials",
                    "webinar entitled",
                    em("How to design and run an adaptive clinical trial:",
                       "New resources and easy-to-use software"),
                    ", being run by Graham Wheeler, Munya Dimairo, and ",
                    "Michael Grayling.")
                ),
                shinydashboardPlus::timelineLabel("Jan 2021", color = "gray"),
                shinydashboardPlus::timelineItem(
                  title = strong("Poisson distributed outcomes now supported"),
                  icon  = "gears",
                  color = "gray",
                  time  = "15 Dec 2020",
                  p("A major upgrade to the app is pushed online to provide",
                    "support for Poisson distributed outcomes.")
                ),
                shinydashboardPlus::timelineItem(
                  title  = strong("500 unique users reached"),
                  border = FALSE,
                  icon   = "users",
                  color  = "gray",
                  time   = "15 Oct 2020",
                ),
                shinydashboardPlus::timelineItem(
                  title = strong("Multi-stage designs now supported"),
                  icon  = "gears",
                  color = "gray",
                  time  = "14 Oct 2020",
                  p("A major overhaul to the app is pushed online to provide",
                    "support for multi-stage designs.")
                ),
                shinydashboardPlus::timelineItem(
                  title = strong("SCT 2020 Virtual Meeting"),
                  icon  = "comments",
                  color = "gray",
                  time  = "1 Sep 2020",
                  p("A talk including a discussion of upcoming multi-stage app",
                    "functionality is given as part of the",
                    em("Society for Clinical Trials 2020 Virtual Meeting."))
                ),
                shinydashboardPlus::timelineLabel("June 2020", color = "gray"),
                shinydashboardPlus::timelineItem(
                  title  = strong("250 unique users reached"),
                  border = FALSE,
                  icon   = "users",
                  color  = "gray",
                  time   = "7 Feb 2020",
                ),
                shinydashboardPlus::timelineItem(
                  title = strong("Paper published in", em("BMC Cancer")),
                  icon  = "file-alt",
                  color = "gray",
                  time  = "31 Jan 2020",
                  p("An article on designing single-stage trials for Bernoulli",
                    "and normally distributed outcomes is published in",
                    em("BMC Cancer."))
                ),
                shinydashboardPlus::timelineLabel("Jan 2020", color = "gray"),
                shinydashboardPlus::timelineItem(
                  title = strong("ICTMC 2019"),
                  icon  = "comments",
                  color = "gray",
                  time  = "6-9 Oct 2019",
                  p("A poster on the app's single-stage functionality, along",
                    "with a talk that included a discussion of upcoming ",
                    "multi-stage functionality, are presented as part of the",
                    em("5th International Clinical Trials Methodology",
                       "Conference"), "in Brighton, UK.")
                ),
                shinydashboardPlus::timelineItem(
                  title = strong("Paper submitted for publication"),
                  icon  = "file-alt",
                  color = "gray",
                  time  = "20 June 2019",
                  "A paper on the app's single-stage functionality is",
                  "submitted for publication; a corresponding pre-print is",
                  "made available on arXiv."
                ),
                shinydashboardPlus::timelineItem(
                  title  =
                    strong("Support added for binary (Bernoulli) outcomes"),
                  border = FALSE,
                  icon   = "gears",
                  time   = "17 June 2019"
                ),
                shinydashboardPlus::timelineLabel("June 2019", color = "gray"),
                shinydashboardPlus::timelineItem(
                  title  = strong("100 unique users reached"),
                  border = FALSE,
                  icon   = "users",
                  color  = "gray",
                  time   = "6 May 2019",
                ),
                shinydashboardPlus::timelineItem(
                  title = strong("Initial beta version pushed online"),
                  icon  = "gears",
                  color = "gray",
                  time  = "23 Apr 2019",
                  p("Support originally available for continuous (normal)",
                    "outcomes only.")
                ),
                shinydashboardPlus::timelineStart(color = "gray")
              )
            ),
            shinydashboard::box(
              title       = "To-do list",
              status      = "primary",
              width       = 12,
              solidHeader = TRUE,
              shinydashboardPlus::todoList(
                sortable = FALSE,
                shinydashboardPlus::todoListItem(
                  label = "Add functionality for adjusted analysis",
                ),
                shinydashboardPlus::todoListItem(
                  label = "Add functionality for Bayesian designs",
                ),
                shinydashboardPlus::todoListItem(
                  label =
                    "Add functionality for ordinal (multinomial) outcomes",
                ),
                shinydashboardPlus::todoListItem(
                  label = p("Add functionality for multi-stage response ","
                            adaptive designs"),
                ),
                shinydashboardPlus::todoListItem(
                  label = "Add functionality for TiTE (Weibull) outcomes",
                ),
                shinydashboardPlus::todoListItem(
                  label = "Add landing page"
                ),
                shinydashboardPlus::todoListItem(
                  label = "Add walkthrough/tutorial overlay"
                ),
                shinydashboardPlus::todoListItem(
                  label = p("Expand background information/other details on ",
                            "multi-arm trials"),
                ),
                shinydashboardPlus::todoListItem(
                  label = "Fix minor tab linking problem",
                  p("Need correct highlighting of currently opened tab after ",
                    "linking")
                ),
                shinydashboardPlus::todoListItem(
                  label = "Make geom_hline() and geom_vline() optional in plots"
                ),
                shinydashboardPlus::todoListItem(
                  label = "Switch bookmarking to storing richer state",
                ),
                shinydashboardPlus::todoListItem(
                  label = "Switch simulation based calculations to Rcpp",
                ),
                shinydashboardPlus::todoListItem(
                  checked = TRUE,
                  label   = "Add functionality for count (Poisson) outcomes",
                ),
                shinydashboardPlus::todoListItem(
                  checked = TRUE,
                  label   = "Add functionality for binary (Bernoulli) outcomes"
                ),
                shinydashboardPlus::todoListItem(
                  checked = TRUE,
                  label   = "Add functionality for continuous (normal) outcomes"
                ),
                shinydashboardPlus::todoListItem(
                  checked = TRUE,
                  label   = p("Add functionality for multi-stage ",
                              "group-sequential designs")
                ),
                shinydashboardPlus::todoListItem(
                  checked = TRUE,
                  label   = p("Add functionality for multi-stage ",
                              "drop-the-losers designs")
                )
              )
            ),
            shinydashboard::box(
              title       = "References",
              status      = "primary",
              width       = 12,
              solidHeader = TRUE,
              shiny::includeMarkdown("references.md")
            ),
          )
        )
      )
    ),
    title = "multiarm",
    skin  = "blue"
  )
}

##### Server ###################################################################
server <- function(input, output, session) {
  ##### Initial set-up #########################################################

  shinyhelper::observe_helpers(withMathJax = TRUE)
  ranges_design_ss_bern_equal_error         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_bern_equal_power         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_bern_equal_other         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_bern_shifted_power       <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_norm_equal_error         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_norm_equal_power         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_norm_equal_other         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_norm_shifted_power       <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_pois_equal_error         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_pois_equal_power         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_pois_equal_other         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_ss_pois_shifted_power       <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_bern_equal_error         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_bern_equal_power         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_bern_equal_other         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_bern_equal_sample_size   <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_bern_shifted_power       <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_bern_shifted_sample_size <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_bern_pmf_N               <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_norm_equal_error         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_norm_equal_power         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_norm_equal_other         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_norm_equal_sample_size   <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_norm_shifted_power       <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_norm_shifted_sample_size <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_norm_pmf_N               <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_pois_equal_error         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_pois_equal_power         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_pois_equal_other         <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_pois_equal_sample_size   <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_pois_shifted_power       <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_pois_shifted_sample_size <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_gs_pois_pmf_N               <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_bern_equal_error        <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_bern_equal_power        <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_bern_equal_other        <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_bern_shifted_power      <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_norm_equal_error        <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_norm_equal_power        <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_norm_equal_other        <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_norm_shifted_power      <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_pois_equal_error        <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_pois_equal_power        <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_pois_equal_other        <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)
  ranges_design_dtl_pois_shifted_power      <- shiny::reactiveValues(x = NULL,
                                                                     y = NULL)

  ##### Single-stage (Bernoulli): shinyFeedback warning messages ###############

  shiny::observeEvent(input$design_ss_bern_alpha, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_bern_alpha",
      show    = any(input$design_ss_bern_alpha <= 0,
                    input$design_ss_bern_alpha >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_ss_bern_beta, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_bern_beta",
      show    = any(input$design_ss_bern_beta <= 0,
                    input$design_ss_bern_beta >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_ss_bern_pi0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_bern_pi0",
      show    = any(input$design_ss_bern_pi0 <= 0,
                    input$design_ss_bern_pi0 >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_ss_bern_delta1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_bern_delta1",
      show    = any(input$design_ss_bern_delta1 <= 0,
                    input$design_ss_bern_delta1 >=
                      1 - input$design_ss_bern_pi0),
      text    = paste0("Must be strictly between 0 and one minus the control",
                       " arm response rate"))
  })

  shiny::observeEvent(input$design_ss_bern_delta0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_bern_delta0",
      show    = any(input$design_ss_bern_delta0 >=
                      input$design_ss_bern_delta1,
                    input$design_ss_bern_delta0 <= -input$design_ss_bern_pi0),
      text    = paste0("Must be strictly between minus the control arm ",
                       "response rate and the interesting treatment effect"))
  })

  shiny::observeEvent(c(input$design_ss_bern_ratio1,
                        input$design_ss_bern_ratio2,
                        input$design_ss_bern_ratio3,
                        input$design_ss_bern_ratio4,
                        input$design_ss_bern_ratio5), {
                          vals <- c(input$design_ss_bern_ratio1, input$design_ss_bern_ratio2,
                                    input$design_ss_bern_ratio3, input$design_ss_bern_ratio4,
                                    input$design_ss_bern_ratio5)
                          for (i in 1:5) {
                            shinyFeedback::feedbackDanger(
                              inputId = paste0("design_ss_bern_ratio", i),
                              show    = (vals[i] <= 0),
                              text    = "Must be strictly positive")
                          }
                        })

  shiny::observeEvent(input$design_ss_bern_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "design_ss_bern_filename",
      show    = any(strsplit(input$design_ss_bern_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Single-stage (Bernoulli): Dynamic UI elements ##########################

  output$design_ss_bern_delta   <- renderUI({
    inputTagList <-
      shiny::tagList(
        (shiny::numericInput(
          inputId = "design_ss_bern_delta1",
          label   = "Interesting treatment effect:",
          value   = 0.2,
          min     = 0,
          max     = 1 - input$design_ss_bern_pi0,
          step    = 0.1
        ) %>%
          shinyhelper:: helper(
            type    = "markdown",
            title   = "",
            content = "design_delta1",
            size    = "m",
            colour  = "black"
          ))
      )
    newInput     <- (shiny::numericInput(
      inputId = "design_ss_bern_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = -input$design_ss_bern_pi0,
      max     = 1 - input$design_ss_bern_pi0,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black"
      ))
    inputTagList <- tagAppendChild(inputTagList, newInput)
    inputTagList
  })

  output$design_ss_bern_ratio   <- renderUI({
    if (input$design_ss_bern_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_ss_bern_ratio1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ",
                         input$design_ss_bern_K, "):"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    } else if (input$design_ss_bern_ratio_type == "unequal") {
      inputTagList   <-
        shiny::tagList(
          shiny::numericInput(
            inputId = "design_ss_bern_ratio1",
            label   = "Allocation ratio for experimental arm 1:",
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25
          )
        )
      lapply(2:input$design_ss_bern_K, function(i) {
        newInput     <-
          shiny::numericInput(
            inputId = paste0("design_ss_bern_ratio", i),
            label   = paste0("Allocation ratio for experimental arm ", i, ":"),
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25)
        inputTagList <<- tagAppendChild(inputTagList, newInput)
      })
      inputTagList
    } else if (input$design_ss_bern_ratio_type %in% c("A", "D", "E")) {
      shiny::selectInput(
        inputId  = "design_ss_bern_ratio_scenario",
        label    =
          "Treatment effect scenario to optimise allocation ratios for:",
        choices  = c("Global null hypothesis"        = "HG",
                     "Global alternative hypothesis" = "HA"),
        selected = "HG"
      )
    }
  })

  output$design_ss_bern_warning <- renderUI({
    if (any(all(input$design_ss_bern_K %in% c(4, 5),
                input$design_ss_bern_correction %in%
                c("benjamini_hochberg", "benjamini_yekutieli", "hochberg",
                  "holm_bonferroni", "holm_sidak", "step_down_dunnett")),
            all(input$design_ss_bern_K == 5,
                input$design_ss_bern_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_ss_bern_density <- renderUI({
    if (input$design_ss_bern_plots) {
      shiny::selectInput(
        inputId  = "design_ss_bern_density",
        label    = "Plot quality:",
        choices  = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                     "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_ss_bern_reset, {
    shinyjs::reset("design_ss_bern_parameters")
  })

  ##### Single-stage (Bernoulli): Plot zoom set-up #############################

  shiny::observeEvent(input$design_ss_bern_equal_error_dblclick, {
    brush_error                             <-
      input$design_ss_bern_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_ss_bern_equal_error$x   <- c(brush_error$xmin,
                                                 brush_error$xmax)
      ranges_design_ss_bern_equal_error$y   <- c(brush_error$ymin,
                                                 brush_error$ymax)
    } else {
      ranges_design_ss_bern_equal_error$x   <-
        ranges_design_ss_bern_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_ss_bern_equal_power_dblclick, {
    brush_power                             <-
      input$design_ss_bern_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_ss_bern_equal_power$x   <- c(brush_power$xmin,
                                                 brush_power$xmax)
      ranges_design_ss_bern_equal_power$y   <- c(brush_power$ymin,
                                                 brush_power$ymax)
    } else {
      ranges_design_ss_bern_equal_power$x   <-
        ranges_design_ss_bern_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_ss_bern_equal_other_dblclick, {
    brush_other                             <-
      input$design_ss_bern_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_ss_bern_equal_other$x   <- c(brush_other$xmin,
                                                 brush_other$xmax)
      ranges_design_ss_bern_equal_other$y   <- c(brush_other$ymin,
                                                 brush_other$ymax)
    } else {
      ranges_design_ss_bern_equal_other$x   <-
        ranges_design_ss_bern_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_ss_bern_shifted_power_dblclick, {
    brush_shifted_power                       <-
      input$design_ss_bern_shifted_power_brush
    if (!is.null(brush_shifted_power)) {
      ranges_design_ss_bern_shifted_power$x   <- c(brush_shifted_power$xmin,
                                                   brush_shifted_power$xmax)
      ranges_design_ss_bern_shifted_power$y   <- c(brush_shifted_power$ymin,
                                                   brush_shifted_power$ymax)
    } else {
      ranges_design_ss_bern_shifted_power$x   <-
        ranges_design_ss_bern_shifted_power$y <- NULL
    }
  })

  ##### Single-stage (Bernoulli): int_des_ss_bern() ############################

  int_des_ss_bern <- shiny::eventReactive(input$design_ss_bern_update, {
    K                     <- input$design_ss_bern_K
    seq_K                 <- 1:K
    progress              <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Identifying design", value = 0)
    if (input$design_ss_bern_ratio_type == "equal_all") {
      ratio               <- rep(1, K)
    } else if (input$design_ss_bern_ratio_type == "equal_exp") {
      ratio               <- rep(input$design_ss_bern_ratio1, K)
    } else if (input$design_ss_bern_ratio_type == "unequal") {
      ratio               <- numeric(K)
      for (i in seq_K) {
        ratio[i]          <- input[[paste0("design_ss_bern_ratio", i)]]
      }
    } else if (input$design_ss_bern_ratio_type == "root_K") {
      ratio               <- rep(1/sqrt(K), K)
    }
    if (input$design_ss_bern_ratio_type %in% c("A", "D", "E")) {
      ratio               <- input$design_ss_bern_ratio_type
      ratio_scenario      <- input$design_ss_bern_ratio_scenario
    } else {
      ratio_scenario      <- "HG"
    }
    design                <-
      multiarm::des_ss_bern(K              = input$design_ss_bern_K,
                            alpha          = input$design_ss_bern_alpha,
                            beta           = 1 - input$design_ss_bern_beta,
                            pi0            = input$design_ss_bern_pi0,
                            delta1         = input$design_ss_bern_delta1,
                            delta0         = input$design_ss_bern_delta0,
                            ratio          = ratio,
                            correction     = input$design_ss_bern_correction,
                            power          = input$design_ss_bern_power,
                            integer        = input$design_ss_bern_integer,
                            ratio_scenario = ratio_scenario)
    progress$inc(amount = 0.25, message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_ss_bern_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_ss_bern_summary.html"),
      params        = list(K              = design$K,
                           alpha          = design$alpha,
                           beta           = design$beta,
                           pi0            = design$pi0,
                           delta1         = design$delta1,
                           delta0         = design$delta0,
                           ratio_type     = input$design_ss_bern_ratio_type,
                           ratio_init     = c(input$design_ss_bern_ratio1,
                                              input$design_ss_bern_ratio2,
                                              input$design_ss_bern_ratio3,
                                              input$design_ss_bern_ratio4,
                                              input$design_ss_bern_ratio5),
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
                           plots          = input$design_ss_bern_plots)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_ss_bern_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_ss_bern_summary_modified.html")
    )
    design$data_og        <- design$opchar
    if (input$design_ss_bern_plots) {
      density             <- as.numeric(input$design_ss_bern_density)
      progress$inc(amount  = 0.25,
                   message = "Computing design operating characteristics")
      plots               <- plot(design, density = density, output = TRUE,
                                  print_plots = FALSE)
      design$equal_power  <- plots$plots$equal_power
      design$equal_error  <- plots$plots$equal_error
      design$equal_other  <- plots$plots$equal_other
      design$shifted_power <- plots$plots$shifted_power
      progress$inc(amount = 0.25, message = "Rendering plots")
      opchar              <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(plots$opchar, 3),
                                                    .name_repair = "minimal")))
      rows_shifted        <- (nrow(opchar) - density)/K
      design$data         <-
        data.frame(rbind(design$opchar, opchar),
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                                 paste0("Equal #", 1:density),
                                 paste0("Shifted <i>&pi;</i><sub>",
                                        rep(seq_K, each = rows_shifted),
                                        "</sub>, #", rep(1:rows_shifted, K))))
    } else {
      design$data         <-
        data.frame(design$opchar,
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      design$equal_error  <- design$equal_power <- design$equal_other <-
        design$shifted_power <- design$delta       <- NULL
    }
    colnames(design$data) <-
      c(paste0("<i>&pi;</i><sub>", c(0, seq_K), "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
        paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
        "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
        "<i>Sensitivity</i>", "<i>Specificity</i>")
    design$repro_code     <-
      paste0("# Running the following code within R will reproduce the design\n",
           "design <- des_ss_bern(K              = ", design$K,
           ",\n                      alpha          = ",
           design$alpha,
           ",\n                      beta           = ", design$beta,
           ",\n                      pi0            = ", design$pi0,
           ",\n                      delta1         = ",
           design$delta1,
           ",\n                      delta0         = ",
           design$delta0,
           ",\n                      ratio          = c(",
           paste(design$ratio, collapse = ", "), ")",
           ",\n                      correction     = ",
           design$correction,
           ",\n                      power          = ",
           design$power,
           ",\n                      integer        = ",
           design$integer,
           ",\n                      ratio_scenario = \"",
           design$ratio_scenario, "\")\n",
           "# Running the following code will then reproduce the data given ",
           "in the tables and the figures\n",
           "tables_and_figs <- plot(design",
           ",\n                        density     = ",
           as.numeric(input$design_ss_bern_density),
           ",\n                        output      = TRUE",
           ",\n                        print_plots = TRUE)")
    progress$inc(amount  = 0.25 + as.numeric(!input$design_ss_bern_plots),
                 message = "Outputting results")
    design
  })

  ##### Single-stage (Bernoulli): Value boxes ##################################

  output$design_ss_bern_n_box     <- shinydashboard::renderValueBox({
    input$design_ss_bern_update
    shinydashboard::valueBox(
      value    = round(int_des_ss_bern()$N, 1),
      subtitle = "Total required sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_ss_bern_fwer_box  <- shinydashboard::renderValueBox({
    input$design_ss_bern_update
    correction      <- shiny::isolate(input$design_ss_bern_correction)
    if (!(correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                            "none"))) {
      if (int_des_ss_bern()$opchar$FWERI1[1] <=
          shiny::isolate(input$design_ss_bern_alpha) + 1e-4) {
        icon_choice <- "thumbs-up"
      } else {
        icon_choice <- "thumbs-down"
      }
    } else {
      icon_choice   <- ""
    }
    shinydashboard::valueBox(
      value    = round(int_des_ss_bern()$opchar$FWERI1[1], 3),
      subtitle = "Maximum family-wise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_ss_bern_power_box <- shinydashboard::renderValueBox({
    input$design_ss_bern_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(input$design_ss_bern_power)]
    K                 <- isolate(input$design_ss_bern_K)
    if (input$design_ss_bern_power == "conjunctive") {
      value_power_box <- int_des_ss_bern()$opchar$Pcon[2]
    } else if (input$design_ss_bern_power == "disjunctive") {
      value_power_box <- int_des_ss_bern()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(int_des_ss_bern()$opchar[-(1:2),
                                                    (K + 4):(2*K + 3)])))
    }
    if (value_power_box >=
        shiny::isolate(input$design_ss_bern_beta) - 1e-3) {
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

  ##### Single-stage (Bernoulli): Code #########################################

  # Plots needs to be conditional. Need to do something properly about
  # ratio_scenario (whether it's needed) and when ratio follows sqrt(K) rule
  output$design_ss_bern_code <- renderText({
    input$design_ss_bern_update
    N <- int_des_ss_bern()$N
    int_des_ss_bern()$repro_code
  })

  output$design_ss_bern_clip <- renderUI({
    input$design_ss_bern_update
    N <- int_des_ss_bern()$N
    rclipboard::rclipButton("clipbtn", "Copy to Clipboard",
                            int_des_ss_bern()$repro_code, icon("clipboard"))
  })

  ##### Single-stage (Bernoulli): Summary ######################################

  output$design_ss_bern_summary <- shiny::renderUI({
    input$design_ss_bern_update
    N <- int_des_ss_bern()$N
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_ss_bern_summary_modified.html")
      )
    )
  })

  ##### Single-stage (Bernoulli): Table ########################################

  output$design_ss_bern_table_key   <- DT::renderDT({
    K                            <- int_des_ss_bern()$K
    table_key                    <-
      int_des_ss_bern()$data[, c(1:(K + 2), (K + 4):(2*K + 4), 4*K + 4)]
    colnames(table_key)[2*K + 3] <- "<i>FWER</i>"
    DT::datatable(
      round(table_key, 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_ss_bern_table_error <- DT::renderDT({
    K                            <- int_des_ss_bern()$K
    DT::datatable(
      round(
        int_des_ss_bern()$data[, c(1:(K + 1), (2*K + 4):(4*K + 4))],
        3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_ss_bern_table_other <- DT::renderDT({
    K                            <- int_des_ss_bern()$K
    DT::datatable(
      round(
        int_des_ss_bern()$data[, -((2*K + 4):(4*K + 4))],
        3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  ##### Single-stage (Bernoulli): Plots ########################################

  output$design_ss_bern_equal_error <- shiny::renderPlot({
    input$design_ss_bern_update
    if (shiny::isolate(input$design_ss_bern_plots)) {
      int_des_ss_bern()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_bern_equal_error$x,
                                 ylim   = ranges_design_ss_bern_equal_error$y,
                                 expand = TRUE)
    }
  })

  output$design_ss_bern_equal_power <- shiny::renderPlot({
    input$design_ss_bern_update
    if (shiny::isolate(input$design_ss_bern_plots)) {
      int_des_ss_bern()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_bern_equal_power$x,
                                 ylim   = ranges_design_ss_bern_equal_power$y,
                                 expand = TRUE)
    }
  })

  output$design_ss_bern_equal_other <- shiny::renderPlot({
    input$design_ss_bern_update
    if (shiny::isolate(input$design_ss_bern_plots)) {
      int_des_ss_bern()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_bern_equal_other$x,
                                 ylim   = ranges_design_ss_bern_equal_other$y,
                                 expand = TRUE)
    }
  })

  output$design_ss_bern_shifted_power <- shiny::renderPlot({
    input$design_ss_bern_update
    if (shiny::isolate(input$design_ss_bern_plots)) {
      int_des_ss_bern()$shifted_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_bern_shifted_power$x,
                                 ylim   = ranges_design_ss_bern_shifted_power$y,
                                 expand = TRUE)
    }
  })

  ##### Single-stage (Bernoulli): Report #######################################

  output$design_ss_bern_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_ss_bern_filename, sep = '.',
            switch(input$design_ss_bern_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_ss_bern_report.Rmd")
      file.copy("design_ss_bern_report.Rmd", tempReport, overwrite = TRUE)
      params     <- list(K              = int_des_ss_bern()$K,
                         alpha          = int_des_ss_bern()$alpha,
                         beta           = int_des_ss_bern()$beta,
                         pi0            = int_des_ss_bern()$pi0,
                         delta1         = int_des_ss_bern()$delta1,
                         delta0         = int_des_ss_bern()$delta0,
                         ratio_type     = input$design_ss_bern_ratio_type,
                         ratio_init     = c(input$design_ss_bern_ratio1,
                                            input$design_ss_bern_ratio2,
                                            input$design_ss_bern_ratio3,
                                            input$design_ss_bern_ratio4,
                                            input$design_ss_bern_ratio5),
                         ratio_scenario = int_des_ss_bern()$ratio_scenario,
                         ratio          = int_des_ss_bern()$ratio,
                         correction     = int_des_ss_bern()$correction,
                         power          = int_des_ss_bern()$power,
                         integer        = int_des_ss_bern()$integer,
                         large_N        = int_des_ss_bern()$N,
                         small_n        = int_des_ss_bern()$n,
                         opchar         = int_des_ss_bern()$opchar,
                         gamma          = int_des_ss_bern()$gamma,
                         gammaO         = int_des_ss_bern()$gammaO,
                         plots          = input$design_ss_bern_plots,
                         equal_error    = int_des_ss_bern()$equal_error,
                         equal_power    = int_des_ss_bern()$equal_power,
                         equal_other    = int_des_ss_bern()$equal_other,
                         shifted_power  = int_des_ss_bern()$shifted_power,
                         data           = int_des_ss_bern()$data_og)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$design_ss_bern_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Single-stage (normal): shinyFeedback warning messages ##################

  shiny::observeEvent(input$design_ss_norm_alpha, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_norm_alpha",
      show    = any(input$design_ss_norm_alpha <= 0,
                    input$design_ss_norm_alpha >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_ss_norm_beta, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_norm_beta",
      show    = any(input$design_ss_norm_beta <= 0,
                    input$design_ss_norm_beta >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_ss_norm_delta1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_norm_delta1",
      show    = (input$design_ss_norm_delta1 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_ss_norm_delta0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_norm_delta0",
      show    = (input$design_ss_norm_delta0 >= input$design_ss_norm_delta1),
      text    =
        "Must be strictly smaller than the interesting treatment effect")
  })

  shiny::observeEvent(input$design_ss_norm_sigma, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_norm_sigma",
      show    = (input$design_ss_norm_sigma <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_ss_norm_sigma0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_norm_sigma0",
      show    = (input$design_ss_norm_sigma0 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(c(input$design_ss_norm_sigma1,
                        input$design_ss_norm_sigma2,
                        input$design_ss_norm_sigma3,
                        input$design_ss_norm_sigma4,
                        input$design_ss_norm_sigma5), {
                          vals <- c(input$design_ss_norm_sigma1, input$design_ss_norm_sigma2,
                                    input$design_ss_norm_sigma3, input$design_ss_norm_sigma4,
                                    input$design_ss_norm_sigma5)
                          for (i in 1:5) {
                            shinyFeedback::feedbackDanger(
                              inputId = paste0("design_ss_norm_sigma", i),
                              show    = (vals[i] <= 0),
                              text    = "Must be strictly positive")
                          }
                        })

  shiny::observeEvent(c(input$design_ss_norm_ratio1,
                        input$design_ss_norm_ratio2,
                        input$design_ss_norm_ratio3,
                        input$design_ss_norm_ratio4,
                        input$design_ss_norm_ratio5), {
                          vals <- c(input$design_ss_norm_ratio1, input$design_ss_norm_ratio2,
                                    input$design_ss_norm_ratio3, input$design_ss_norm_ratio4,
                                    input$design_ss_norm_ratio5)
                          for (i in 1:5) {
                            shinyFeedback::feedbackDanger(
                              inputId = paste0("design_ss_norm_ratio", i),
                              show    = (vals[i] <= 0),
                              text    = "Must be strictly positive")
                          }
                        })

  shiny::observeEvent(input$design_ss_norm_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "design_ss_norm_filename",
      show    = any(strsplit(input$design_ss_norm_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Single-stage (normal): Dynamic UI elements #############################

  output$design_ss_norm_delta0  <- renderUI({
    shiny::numericInput(
      inputId = "design_ss_norm_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = NA,
      max     = input$design_ss_norm_delta1,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black"
      )
  })

  output$design_ss_norm_sigma   <- renderUI({
    if (input$design_ss_norm_sigma_type == "equal_all") {
      shiny::numericInput(
        inputId = "design_ss_norm_sigma",
        label   = paste0("Standard deviation of the responses (arms 0, ..., ",
                         input$design_ss_norm_K, "):"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.1
      )
    } else if (input$design_ss_norm_sigma_type == "equal_exp") {
      shiny::tagList(
        shiny::numericInput(
          inputId = "design_ss_norm_sigma0",
          label   = "Standard deviation of the control arm responses (arm 0):",
          value   = 1,
          min     = 0,
          max     = NA,
          step    = 0.1
        ),
        shiny::numericInput(
          inputId = "design_ss_norm_sigma1",
          label   =
            paste0("Standard deviation of the experimental arm responses ",
                   "(arms 1, ..., ", input$design_ss_norm_K, "):"),
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
            inputId = "design_ss_norm_sigma0",
            label   =
              "Standard deviation of the control arm responses (arm 0):",
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.1
          )
        )
      lapply(1:input$design_ss_norm_K, function(i) {
        newInput     <-
          shiny::numericInput(
            inputId = paste0("design_ss_norm_sigma", i),
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

  output$design_ss_norm_ratio   <- renderUI({
    if (input$design_ss_norm_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_ss_norm_ratio1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_ss_norm_K, "):"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    } else if (input$design_ss_norm_ratio_type == "unequal") {
      inputTagList   <-
        shiny::tagList(
          shiny::numericInput(
            inputId = "design_ss_norm_ratio1",
            label   = "Allocation ratio for experimental arm 1:",
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25
          )
        )
      lapply(2:input$design_ss_norm_K, function(i) {
        newInput     <-
          shiny::numericInput(
            inputId = paste0("design_ss_norm_ratio", i),
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

  output$design_ss_norm_warning <- renderUI({
    if (any(all(input$design_ss_norm_K %in% c(4, 5),
                input$design_ss_norm_correction %in%
                c("benjamini_hochberg", "benjamini_yekutieli", "hochberg",
                  "holm_bonferroni", "holm_sidak", "step_down_dunnett")),
            all(input$design_ss_norm_K == 5,
                input$design_ss_norm_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_ss_norm_density <- renderUI({
    if (input$design_ss_norm_plots) {
      shiny::selectInput(
        inputId  = "design_ss_norm_density",
        label    = "Plot quality:",
        choices  = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                     "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_ss_norm_reset, {
    shinyjs::reset("design_ss_norm_parameters")
  })

  ##### Single-stage (normal): Plot zoom set-up ################################

  shiny::observeEvent(input$design_ss_norm_equal_error_dblclick, {
    brush_error                             <-
      input$design_ss_norm_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_ss_norm_equal_error$x   <- c(brush_error$xmin,
                                                 brush_error$xmax)
      ranges_design_ss_norm_equal_error$y   <- c(brush_error$ymin,
                                                 brush_error$ymax)
    } else {
      ranges_design_ss_norm_equal_error$x   <-
        ranges_design_ss_norm_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_ss_norm_equal_power_dblclick, {
    brush_power                             <-
      input$design_ss_norm_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_ss_norm_equal_power$x   <- c(brush_power$xmin,
                                                 brush_power$xmax)
      ranges_design_ss_norm_equal_power$y   <- c(brush_power$ymin,
                                                 brush_power$ymax)
    } else {
      ranges_design_ss_norm_equal_power$x   <-
        ranges_design_ss_norm_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_ss_norm_equal_other_dblclick, {
    brush_other                             <-
      input$design_ss_norm_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_ss_norm_equal_other$x   <- c(brush_other$xmin,
                                                 brush_other$xmax)
      ranges_design_ss_norm_equal_other$y   <- c(brush_other$ymin,
                                                 brush_other$ymax)
    } else {
      ranges_design_ss_norm_equal_other$x   <-
        ranges_design_ss_norm_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_ss_norm_shifted_power_dblclick, {
    brush_shifted_power                       <-
      input$design_ss_norm_shifted_power_brush
    if (!is.null(brush_shifted_power)) {
      ranges_design_ss_norm_shifted_power$x   <- c(brush_shifted_power$xmin,
                                                   brush_shifted_power$xmax)
      ranges_design_ss_norm_shifted_power$y   <- c(brush_shifted_power$ymin,
                                                   brush_shifted_power$ymax)
    } else {
      ranges_design_ss_norm_shifted_power$x   <-
        ranges_design_ss_norm_shifted_power$y <- NULL
    }
  })

  ##### Single-stage (normal): int_des_ss_norm() ###############################

  int_des_ss_norm <- shiny::eventReactive(input$design_ss_norm_update, {
    K                     <- input$design_ss_norm_K
    seq_K                 <- 1:K
    progress              <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Identifying design", value = 0)
    if (input$design_ss_norm_sigma_type == "equal_all") {
      sigma               <- rep(input$design_ss_norm_sigma, K + 1)
    } else if (input$design_ss_norm_sigma_type == "equal_exp") {
      sigma               <- c(input$design_ss_norm_sigma0,
                               rep(input$design_ss_norm_sigma1, K))
    } else if (input$design_ss_norm_sigma_type == "unequal") {
      sigma               <- numeric(K + 1)
      for (i in c(0, seq_K)) {
        sigma[i + 1]      <- input[[paste0("design_ss_norm_sigma", i)]]
      }
    }
    if (input$design_ss_norm_ratio_type == "equal_all") {
      ratio               <- rep(1, K)
    } else if (input$design_ss_norm_ratio_type == "equal_exp") {
      ratio               <- rep(input$design_ss_norm_ratio1, K)
    } else if (input$design_ss_norm_ratio_type == "unequal") {
      ratio               <- numeric(K)
      for (i in seq_K) {
        ratio[i]          <- input[[paste0("design_ss_norm_ratio", i)]]
      }
    } else if (input$design_ss_norm_ratio_type == "root_K") {
      ratio               <- rep(1/sqrt(K), K)
    } else {
      ratio               <- input$design_ss_norm_ratio_type
    }
    design                <-
      multiarm::des_ss_norm(K          = input$design_ss_norm_K,
                            alpha      = input$design_ss_norm_alpha,
                            beta       = 1 - input$design_ss_norm_beta,
                            delta1     = input$design_ss_norm_delta1,
                            delta0     = input$design_ss_norm_delta0,
                            sigma      = sigma,
                            ratio      = ratio,
                            correction = input$design_ss_norm_correction,
                            power      = input$design_ss_norm_power,
                            integer    = input$design_ss_norm_integer)
    progress$inc(amount = 0.25, message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_ss_norm_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_ss_norm_summary.html"),
      params        = list(K          = design$K,
                           alpha      = design$alpha,
                           beta       = design$beta,
                           delta1     = design$delta1,
                           delta0     = design$delta0,
                           sigma      = design$sigma,
                           ratio_type = input$design_ss_norm_ratio_type,
                           ratio_init = c(input$design_ss_norm_ratio1,
                                          input$design_ss_norm_ratio2,
                                          input$design_ss_norm_ratio3,
                                          input$design_ss_norm_ratio4,
                                          input$design_ss_norm_ratio5),
                           ratio      = design$ratio,
                           correction = design$correction,
                           power      = design$power,
                           integer    = design$integer,
                           large_N    = design$N,
                           small_n    = design$n,
                           opchar     = design$opchar,
                           gamma      = design$gamma,
                           gammaO     = design$gammaO,
                           plots      = input$design_ss_norm_plots)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_ss_norm_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_ss_norm_summary_modified.html")
    )
    design$data_og        <- design$opchar
    if (input$design_ss_norm_plots) {
      density             <- as.numeric(input$design_ss_norm_density)
      progress$inc(amount  = 0.25,
                   message = "Computing design operating characteristics")
      plots               <- plot(design, density = density, output = TRUE,
                                  print_plots = FALSE)
      design$equal_power  <- plots$plots$equal_power
      design$equal_error  <- plots$plots$equal_error
      design$equal_other  <- plots$plots$equal_other
      design$shifted_power <- plots$plots$shifted_power
      progress$inc(amount = 0.25, message = "Rendering plots")
      opchar              <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(plots$opchar, 3),
                                                    .name_repair = "minimal")))
      design$data         <-
        data.frame(rbind(design$opchar, opchar),
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                                 paste0("Equal #", 1:density),
                                 paste0("Shifted <i>&tau;</i><sub>",
                                        rep(seq_K, each = density), "</sub>, #",
                                        rep(1:density, K))))
    } else {
      design$data         <-
        data.frame(design$opchar,
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      design$equal_error  <- design$equal_power <- design$equal_other <-
        design$shifted_power <- design$delta       <- NULL
    }
    colnames(design$data) <-
      c(paste0("<i>&tau;</i><sub>", seq_K, "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
        paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>", c("PHER", "FDR", "pFDR", "FNDR", "Sensitivity",
                        "Specificity"), "</i>"))
    progress$inc(amount  = 0.25 + 0.75*as.numeric(!input$design_ss_norm_plots),
                 message = "Outputting results")
    design
  })

  ##### Single-stage (normal): Value boxes #####################################

  output$design_ss_norm_n_box     <- shinydashboard::renderValueBox({
    input$design_ss_norm_update
    shinydashboard::valueBox(
      value    = round(int_des_ss_norm()$N, 1),
      subtitle = "Total required sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_ss_norm_fwer_box  <- shinydashboard::renderValueBox({
    input$design_ss_norm_update
    correction      <- shiny::isolate(input$design_ss_norm_correction)
    if (!(correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                            "none"))) {
      if (int_des_ss_norm()$opchar$FWERI1[1] <=
          shiny::isolate(input$design_ss_norm_alpha) + 1e-4) {
        icon_choice <- "thumbs-up"
      } else {
        icon_choice <- "thumbs-down"
      }
    } else {
      icon_choice   <- ""
    }
    shinydashboard::valueBox(
      value    = round(int_des_ss_norm()$opchar$FWERI1[1], 3),
      subtitle = "Maximum family-wise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_ss_norm_power_box <- shinydashboard::renderValueBox({
    input$design_ss_norm_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(input$design_ss_norm_power)]
    K                 <- isolate(input$design_ss_norm_K)
    if (input$design_ss_norm_power == "conjunctive") {
      value_power_box <- int_des_ss_norm()$opchar$Pcon[2]
    } else if (input$design_ss_norm_power == "disjunctive") {
      value_power_box <- int_des_ss_norm()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(int_des_ss_norm()$opchar[-(1:2), (K + 3):(2*K + 2)])))
    }
    if (value_power_box >= shiny::isolate(input$design_ss_norm_beta) - 1e-3) {
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

  ##### Single-stage (normal): Summary #########################################

  output$design_ss_norm_summary <- shiny::renderUI({
    input$design_ss_norm_update
    N <- int_des_ss_norm()$N
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_ss_norm_summary_modified.html")
      )
    )
  })

  ##### Single-stage (normal): Table ###########################################

  output$design_ss_norm_table_key   <- DT::renderDT({
    K                            <- int_des_ss_norm()$K
    table_key                    <-
      int_des_ss_norm()$data[, c(1:(K + 1), (K + 3):(2*K + 3), 4*K + 3)]
    colnames(table_key)[2*K + 2] <- "<i>FWER</i>"
    DT::datatable(
      round(table_key, 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_ss_norm_table_error <- DT::renderDT({
    K <- int_des_ss_norm()$K
    DT::datatable(
      round(int_des_ss_norm()$data[, c(1:K, (2*K + 3):(4*K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_ss_norm_table_other <- DT::renderDT({
    K <- int_des_ss_norm()$K
    DT::datatable(
      round(int_des_ss_norm()$data[, -((2*K + 3):(4*K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  ##### Single-stage (normal): Plots ###########################################

  output$design_ss_norm_equal_error <- shiny::renderPlot({
    input$design_ss_norm_update
    if (shiny::isolate(input$design_ss_norm_plots)) {
      int_des_ss_norm()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_norm_equal_error$x,
                                 ylim   = ranges_design_ss_norm_equal_error$y,
                                 expand = TRUE)
    }
  })

  output$design_ss_norm_equal_power <- shiny::renderPlot({
    input$design_ss_norm_update
    if (shiny::isolate(input$design_ss_norm_plots)) {
      int_des_ss_norm()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_norm_equal_power$x,
                                 ylim   = ranges_design_ss_norm_equal_power$y,
                                 expand = TRUE)
    }
  })

  output$design_ss_norm_equal_other <- shiny::renderPlot({
    input$design_ss_norm_update
    if (shiny::isolate(input$design_ss_norm_plots)) {
      int_des_ss_norm()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_norm_equal_other$x,
                                 ylim   = ranges_design_ss_norm_equal_other$y,
                                 expand = TRUE)
    }
  })

  output$design_ss_norm_shifted_power <- shiny::renderPlot({
    input$design_ss_norm_update
    if (shiny::isolate(input$design_ss_norm_plots)) {
      int_des_ss_norm()$shifted_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_norm_shifted_power$x,
                                 ylim   = ranges_design_ss_norm_shifted_power$y,
                                 expand = TRUE)
    }
  })

  ##### Single-stage (normal): Report ##########################################

  output$design_ss_norm_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_ss_norm_filename, sep = '.',
            switch(input$design_ss_norm_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_ss_norm_report.Rmd")
      file.copy("design_ss_norm_report.Rmd", tempReport, overwrite = TRUE)
      params     <- list(K            = int_des_ss_norm()$K,
                         alpha        = int_des_ss_norm()$alpha,
                         beta         = int_des_ss_norm()$beta,
                         delta1       = int_des_ss_norm()$delta1,
                         delta0       = int_des_ss_norm()$delta0,
                         sigma        = int_des_ss_norm()$sigma,
                         ratio_type   = input$design_ss_norm_ratio_type,
                         ratio_init   = c(input$design_ss_norm_ratio1,
                                          input$design_ss_norm_ratio2,
                                          input$design_ss_norm_ratio3,
                                          input$design_ss_norm_ratio4,
                                          input$design_ss_norm_ratio5),
                         ratio        = int_des_ss_norm()$ratio,
                         correction   = int_des_ss_norm()$correction,
                         power        = int_des_ss_norm()$power,
                         integer      = int_des_ss_norm()$integer,
                         large_N      = int_des_ss_norm()$N,
                         small_n      = int_des_ss_norm()$n,
                         opchar       = int_des_ss_norm()$opchar,
                         gamma        = int_des_ss_norm()$gamma,
                         gammaO       = int_des_ss_norm()$gammaO,
                         plots        = input$design_ss_norm_plots,
                         equal_error  = int_des_ss_norm()$equal_error,
                         equal_power  = int_des_ss_norm()$equal_power,
                         equal_other  = int_des_ss_norm()$equal_other,
                         shifted_power = int_des_ss_norm()$shifted_power,
                         data         = int_des_ss_norm()$data_og)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$design_ss_norm_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Single-stage (Poisson): shinyFeedback warning messages #################

  shiny::observeEvent(input$design_ss_pois_alpha, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_pois_alpha",
      show    = any(input$design_ss_pois_alpha <= 0,
                    input$design_ss_pois_alpha >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_ss_pois_beta, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_pois_beta",
      show    = any(input$design_ss_pois_beta <= 0,
                    input$design_ss_pois_beta >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_ss_pois_lambda0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_pois_lambda0",
      show    = (input$design_ss_pois_lambda0 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_ss_pois_delta1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_pois_delta1",
      show    = (input$design_ss_pois_delta1 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_ss_pois_delta0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_ss_pois_delta0",
      show    = any(input$design_ss_pois_delta0 >=
                      input$design_ss_pois_delta1,
                    input$design_ss_pois_delta0 <=
                      -input$design_ss_pois_lambda0),
      text    = paste0("Must be strictly between minus the control arm ",
                       "event rate and the interesting treatment effect"))
  })

  shiny::observeEvent(c(input$design_ss_pois_ratio1,
                        input$design_ss_pois_ratio2,
                        input$design_ss_pois_ratio3,
                        input$design_ss_pois_ratio4,
                        input$design_ss_pois_ratio5), {
    vals <- c(input$design_ss_pois_ratio1, input$design_ss_pois_ratio2,
              input$design_ss_pois_ratio3, input$design_ss_pois_ratio4,
              input$design_ss_pois_ratio5)
    for (i in 1:5) {
      shinyFeedback::feedbackDanger(
        inputId = paste0("design_ss_pois_ratio", i),
        show    = (vals[i] <= 0),
        text    = "Must be strictly positive")
    }
  })

  shiny::observeEvent(input$design_ss_pois_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "design_ss_pois_filename",
      show    = any(strsplit(input$design_ss_pois_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Single-stage (Poisson): Dynamic UI elements ############################

  output$design_ss_pois_delta   <- renderUI({
    inputTagList <-
      shiny::tagList(
        (shiny::numericInput(
          inputId = "design_ss_pois_delta1",
          label   = "Interesting treatment effect:",
          value   = 1,
          min     = 0,
          step    = 0.1
        ) %>%
          shinyhelper:: helper(
            type    = "markdown",
            title   = "",
            content = "design_delta1",
            size    = "m",
            colour  = "black"
          ))
      )
    newInput     <- (shiny::numericInput(
      inputId = "design_ss_pois_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = -input$design_ss_pois_lambda0,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black"
      ))
    inputTagList <- tagAppendChild(inputTagList, newInput)
    inputTagList
  })

  output$design_ss_pois_ratio   <- renderUI({
    if (input$design_ss_pois_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_ss_pois_ratio1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ",
                         input$design_ss_pois_K, "):"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    } else if (input$design_ss_pois_ratio_type == "unequal") {
      inputTagList   <-
        shiny::tagList(
          shiny::numericInput(
            inputId = "design_ss_pois_ratio1",
            label   = "Allocation ratio for experimental arm 1:",
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25
          )
        )
      lapply(2:input$design_ss_pois_K, function(i) {
        newInput     <-
          shiny::numericInput(
            inputId = paste0("design_ss_pois_ratio", i),
            label   = paste0("Allocation ratio for experimental arm ", i, ":"),
            value   = 1,
            min     = 0,
            max     = NA,
            step    = 0.25)
        inputTagList <<- tagAppendChild(inputTagList, newInput)
      })
      inputTagList
    } else if (input$design_ss_pois_ratio_type %in% c("A", "D", "E")) {
      shiny::selectInput(
        inputId  = "design_ss_pois_ratio_scenario",
        label    =
          "Treatment effect scenario to optimise allocation ratios for:",
        choices  = c("Global null hypothesis"        = "HG",
                     "Global alternative hypothesis" = "HA"),
        selected = "HG"
      )
    }
  })

  output$design_ss_pois_warning <- renderUI({
    if (any(all(input$design_ss_pois_K %in% c(4, 5),
                input$design_ss_pois_correction %in%
                c("benjamini_hochberg", "benjamini_yekutieli", "hochberg",
                  "holm_bonferroni", "holm_sidak", "step_down_dunnett")),
            all(input$design_ss_pois_K == 5,
                input$design_ss_pois_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_ss_pois_density <- renderUI({
    if (input$design_ss_pois_plots) {
      shiny::selectInput(
        inputId  = "design_ss_pois_density",
        label    = "Plot quality:",
        choices  = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                     "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_ss_pois_reset, {
    shinyjs::reset("design_ss_pois_parameters")
  })

  ##### Single-stage (Poisson): Plot zoom set-up ###############################

  shiny::observeEvent(input$design_ss_pois_equal_error_dblclick, {
    brush_error                             <-
      input$design_ss_pois_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_ss_pois_equal_error$x   <- c(brush_error$xmin,
                                                 brush_error$xmax)
      ranges_design_ss_pois_equal_error$y   <- c(brush_error$ymin,
                                                 brush_error$ymax)
    } else {
      ranges_design_ss_pois_equal_error$x   <-
        ranges_design_ss_pois_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_ss_pois_equal_power_dblclick, {
    brush_power                             <-
      input$design_ss_pois_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_ss_pois_equal_power$x   <- c(brush_power$xmin,
                                                 brush_power$xmax)
      ranges_design_ss_pois_equal_power$y   <- c(brush_power$ymin,
                                                 brush_power$ymax)
    } else {
      ranges_design_ss_pois_equal_power$x   <-
        ranges_design_ss_pois_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_ss_pois_equal_other_dblclick, {
    brush_other                             <-
      input$design_ss_pois_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_ss_pois_equal_other$x   <- c(brush_other$xmin,
                                                 brush_other$xmax)
      ranges_design_ss_pois_equal_other$y   <- c(brush_other$ymin,
                                                 brush_other$ymax)
    } else {
      ranges_design_ss_pois_equal_other$x   <-
        ranges_design_ss_pois_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_ss_pois_shifted_power_dblclick, {
    brush_shifted_power                       <-
      input$design_ss_pois_shifted_power_brush
    if (!is.null(brush_shifted_power)) {
      ranges_design_ss_pois_shifted_power$x   <- c(brush_shifted_power$xmin,
                                                   brush_shifted_power$xmax)
      ranges_design_ss_pois_shifted_power$y   <- c(brush_shifted_power$ymin,
                                                   brush_shifted_power$ymax)
    } else {
      ranges_design_ss_pois_shifted_power$x   <-
        ranges_design_ss_pois_shifted_power$y <- NULL
    }
  })

  ##### Single-stage (Poisson): int_des_ss_pois() ##############################

  int_des_ss_pois <- shiny::eventReactive(input$design_ss_pois_update, {
    K                     <- input$design_ss_pois_K
    seq_K                 <- 1:K
    progress              <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Identifying design", value = 0)
    if (input$design_ss_pois_ratio_type == "equal_all") {
      ratio               <- rep(1, K)
    } else if (input$design_ss_pois_ratio_type == "equal_exp") {
      ratio               <- rep(input$design_ss_pois_ratio1, K)
    } else if (input$design_ss_pois_ratio_type == "unequal") {
      ratio               <- numeric(K)
      for (i in seq_K) {
        ratio[i]          <- input[[paste0("design_ss_pois_ratio", i)]]
      }
    } else if (input$design_ss_pois_ratio_type == "root_K") {
      ratio               <- rep(1/sqrt(K), K)
    }
    if (input$design_ss_pois_ratio_type %in% c("A", "D", "E")) {
      ratio               <- input$design_ss_pois_ratio_type
      ratio_scenario      <- input$design_ss_pois_ratio_scenario
    } else {
      ratio_scenario      <- "HG"
    }
    design                <-
      multiarm::des_ss_pois(K              = input$design_ss_pois_K,
                            alpha          = input$design_ss_pois_alpha,
                            beta           = 1 - input$design_ss_pois_beta,
                            lambda0        = input$design_ss_pois_lambda0,
                            delta1         = input$design_ss_pois_delta1,
                            delta0         = input$design_ss_pois_delta0,
                            ratio          = ratio,
                            correction     = input$design_ss_pois_correction,
                            power          = input$design_ss_pois_power,
                            integer        = input$design_ss_pois_integer,
                            ratio_scenario = ratio_scenario)
    progress$inc(amount = 0.25, message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_ss_pois_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_ss_pois_summary.html"),
      params        = list(K              = design$K,
                           alpha          = design$alpha,
                           beta           = design$beta,
                           lambda0        = design$lambda0,
                           delta1         = design$delta1,
                           delta0         = design$delta0,
                           ratio_type     = input$design_ss_pois_ratio_type,
                           ratio_init     = c(input$design_ss_pois_ratio1,
                                              input$design_ss_pois_ratio2,
                                              input$design_ss_pois_ratio3,
                                              input$design_ss_pois_ratio4,
                                              input$design_ss_pois_ratio5),
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
                           plots          = input$design_ss_pois_plots)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_ss_pois_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_ss_pois_summary_modified.html")
    )
    design$data_og        <- design$opchar
    if (input$design_ss_pois_plots) {
      density             <- as.numeric(input$design_ss_pois_density)
      progress$inc(amount  = 0.25,
                   message = "Computing design operating characteristics")
      plots               <- plot(design, density = density, output = TRUE,
                                  print_plots = FALSE)
      design$equal_power  <- plots$plots$equal_power
      design$equal_error  <- plots$plots$equal_error
      design$equal_other  <- plots$plots$equal_other
      design$shifted_power <- plots$plots$shifted_power
      progress$inc(amount = 0.25, message = "Rendering plots")
      opchar              <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(plots$opchar, 3),
                                                    .name_repair = "minimal")))
      rows_shifted        <- (nrow(opchar) - density)/K
      design$data         <-
        data.frame(rbind(design$opchar, opchar),
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                                 paste0("Equal #", 1:density),
                                 paste0("Shifted <i>&lambda;</i><sub>",
                                        rep(seq_K, each = rows_shifted),
                                        "</sub>, #", rep(1:rows_shifted, K))))
    } else {
      design$data         <-
        data.frame(design$opchar,
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      design$equal_error  <- design$equal_power <- design$equal_other <-
        design$shifted_power <- design$delta       <- NULL
    }
    colnames(design$data) <-
      c(paste0("<i>&lambda;</i><sub>", c(0, seq_K), "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
        paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
        "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
        "<i>Sensitivity</i>", "<i>Specificity</i>")
    progress$inc(amount  = 0.25 + as.numeric(!input$design_ss_pois_plots),
                 message = "Outputting results")
    design
  })

  ##### Single-stage (Poisson): Value boxes ####################################

  output$design_ss_pois_n_box     <- shinydashboard::renderValueBox({
    input$design_ss_pois_update
    shinydashboard::valueBox(
      value    = round(int_des_ss_pois()$N, 1),
      subtitle = "Total required sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_ss_pois_fwer_box  <- shinydashboard::renderValueBox({
    input$design_ss_pois_update
    correction      <- shiny::isolate(input$design_ss_pois_correction)
    if (!(correction %in% c("benjamini_hochberg", "benjamini_yekutieli",
                            "none"))) {
      if (int_des_ss_pois()$opchar$FWERI1[1] <=
          shiny::isolate(input$design_ss_pois_alpha) + 1e-4) {
        icon_choice <- "thumbs-up"
      } else {
        icon_choice <- "thumbs-down"
      }
    } else {
      icon_choice   <- ""
    }
    shinydashboard::valueBox(
      value    = round(int_des_ss_pois()$opchar$FWERI1[1], 3),
      subtitle = "Maximum family-wise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_ss_pois_power_box <- shinydashboard::renderValueBox({
    input$design_ss_pois_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(input$design_ss_pois_power)]
    K                 <- isolate(input$design_ss_pois_K)
    if (input$design_ss_pois_power == "conjunctive") {
      value_power_box <- int_des_ss_pois()$opchar$Pcon[2]
    } else if (input$design_ss_pois_power == "disjunctive") {
      value_power_box <- int_des_ss_pois()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(int_des_ss_pois()$opchar[-(1:2),
                                                    (K + 4):(2*K + 3)])))
    }
    if (value_power_box >=
        shiny::isolate(input$design_ss_pois_beta) - 1e-3) {
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

  ##### Single-stage (Poisson): Summary ########################################

  output$design_ss_pois_summary <- shiny::renderUI({
    input$design_ss_pois_update
    N <- int_des_ss_pois()$N
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_ss_pois_summary_modified.html")
      )
    )
  })

  ##### Single-stage (Poisson): Table ##########################################

  output$design_ss_pois_table_key   <- DT::renderDT({
    K                            <- int_des_ss_pois()$K
    table_key                    <-
      int_des_ss_pois()$data[, c(1:(K + 2), (K + 4):(2*K + 4), 4*K + 4)]
    colnames(table_key)[2*K + 3] <- "<i>FWER</i>"
    DT::datatable(
      round(table_key, 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_ss_pois_table_error <- DT::renderDT({
    K                            <- int_des_ss_pois()$K
    DT::datatable(
      round(
        int_des_ss_pois()$data[, c(1:(K + 1), (2*K + 4):(4*K + 4))],
        3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_ss_pois_table_other <- DT::renderDT({
    K                            <- int_des_ss_pois()$K
    DT::datatable(
      round(
        int_des_ss_pois()$data[, -((2*K + 4):(4*K + 4))],
        3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  ##### Single-stage (Poisson): Plots ##########################################

  output$design_ss_pois_equal_error <- shiny::renderPlot({
    input$design_ss_pois_update
    if (shiny::isolate(input$design_ss_pois_plots)) {
      int_des_ss_pois()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_pois_equal_error$x,
                                 ylim   = ranges_design_ss_pois_equal_error$y,
                                 expand = TRUE)
    }
  })

  output$design_ss_pois_equal_power <- shiny::renderPlot({
    input$design_ss_pois_update
    if (shiny::isolate(input$design_ss_pois_plots)) {
      int_des_ss_pois()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_pois_equal_power$x,
                                 ylim   = ranges_design_ss_pois_equal_power$y,
                                 expand = TRUE)
    }
  })

  output$design_ss_pois_equal_other <- shiny::renderPlot({
    input$design_ss_pois_update
    if (shiny::isolate(input$design_ss_pois_plots)) {
      int_des_ss_pois()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_pois_equal_other$x,
                                 ylim   = ranges_design_ss_pois_equal_other$y,
                                 expand = TRUE)
    }
  })

  output$design_ss_pois_shifted_power <- shiny::renderPlot({
    input$design_ss_pois_update
    if (shiny::isolate(input$design_ss_pois_plots)) {
      int_des_ss_pois()$shifted_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_ss_pois_shifted_power$x,
                                 ylim   = ranges_design_ss_pois_shifted_power$y,
                                 expand = TRUE)
    }
  })

  ##### Single-stage (Poisson): Report #########################################

  output$design_ss_pois_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_ss_pois_filename, sep = '.',
            switch(input$design_ss_pois_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_ss_pois_report.Rmd")
      file.copy("design_ss_pois_report.Rmd", tempReport, overwrite = TRUE)
      params     <- list(K              = int_des_ss_pois()$K,
                         alpha          = int_des_ss_pois()$alpha,
                         beta           = int_des_ss_pois()$beta,
                         lambda0        = int_des_ss_pois()$lambda0,
                         delta1         = int_des_ss_pois()$delta1,
                         delta0         = int_des_ss_pois()$delta0,
                         ratio_type     = input$design_ss_pois_ratio_type,
                         ratio_init     = c(input$design_ss_pois_ratio1,
                                            input$design_ss_pois_ratio2,
                                            input$design_ss_pois_ratio3,
                                            input$design_ss_pois_ratio4,
                                            input$design_ss_pois_ratio5),
                         ratio_scenario = int_des_ss_pois()$ratio_scenario,
                         ratio          = int_des_ss_pois()$ratio,
                         correction     = int_des_ss_pois()$correction,
                         power          = int_des_ss_pois()$power,
                         integer        = int_des_ss_pois()$integer,
                         large_N        = int_des_ss_pois()$N,
                         small_n        = int_des_ss_pois()$n,
                         opchar         = int_des_ss_pois()$opchar,
                         gamma          = int_des_ss_pois()$gamma,
                         gammaO         = int_des_ss_pois()$gammaO,
                         plots          = input$design_ss_pois_plots,
                         equal_error    = int_des_ss_pois()$equal_error,
                         equal_power    = int_des_ss_pois()$equal_power,
                         equal_other    = int_des_ss_pois()$equal_other,
                         shifted_power  = int_des_ss_pois()$shifted_power,
                         data           = int_des_ss_pois()$data_og)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$design_ss_pois_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Group-sequential (Bernoulli): shinyFeedback warning messages ###########

  shiny::observeEvent(input$design_gs_bern_alpha, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_bern_alpha",
      show    = any(input$design_gs_bern_alpha <= 0,
                    input$design_gs_bern_alpha >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_gs_bern_beta, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_bern_beta",
      show    = any(input$design_gs_bern_beta <= 0,
                    input$design_gs_bern_beta >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_gs_bern_pi0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_bern_pi0",
      show    = any(input$design_gs_bern_pi0 <= 0,
                    input$design_gs_bern_pi0 >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_gs_bern_delta1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_bern_delta1",
      show    = any(input$design_gs_bern_delta1 <= 0,
                    input$design_gs_bern_delta1 >=
                      1 - input$design_gs_bern_pi0),
      text    = paste0("Must be strictly between 0 and one minus the control",
                       " arm response rate"))
  })

  shiny::observeEvent(input$design_gs_bern_delta0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_bern_delta0",
      show    = any(input$design_gs_bern_delta0 >=
                      input$design_gs_bern_delta1,
                    input$design_gs_bern_delta0 <= -input$design_gs_bern_pi0),
      text    = paste0("Must be strictly between minus the control arm ",
                       "response rate and the interesting treatment effect"))
  })

  shiny::observeEvent(input$design_gs_bern_ratio, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_bern_ratio",
      show    = (input$design_gs_bern_ratio <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_gs_bern_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "design_gs_bern_filename",
      show    = any(strsplit(input$design_gs_bern_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Group-sequential (Bernoulli): Dynamic UI elements ######################

  output$design_gs_bern_lower_fixed <- renderUI({
    if (input$design_gs_bern_lower == "fixed") {
      shiny::numericInput(
        inputId = "design_gs_bern_lower_fixed",
        label   = "Lower fixed stopping boundary:",
        value   = 0,
        min     = -3,
        max     = 1.5,
        step    = 0.1
      )
    }
  })

  output$design_gs_bern_upper_fixed <- renderUI({
    if (input$design_gs_bern_upper == "fixed") {
      shiny::numericInput(
        inputId = "design_gs_bern_upper_fixed",
        label   = "Upper fixed stopping boundary:",
        value   = 3,
        min     = 2,
        max     = 3,
        step    = 0.1
      )
    }
  })

  output$design_gs_bern_delta   <- renderUI({
    inputTagList <-
      shiny::tagList(
        (shiny::numericInput(
          inputId = "design_gs_bern_delta1",
          label   = "Interesting treatment effect:",
          value   = 0.2,
          min     = 0,
          max     = 1 - input$design_gs_bern_pi0,
          step    = 0.1
        ) %>%
          shinyhelper:: helper(
            type    = "markdown",
            title   = "",
            content = "design_delta1",
            size    = "m",
            colour  = "black"
          ))
      )
    newInput     <- (shiny::numericInput(
      inputId = "design_gs_bern_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = -input$design_gs_bern_pi0,
      max     = 1 - input$design_gs_bern_pi0,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black"
      ))
    inputTagList <- tagAppendChild(inputTagList, newInput)
    inputTagList
  })

  output$design_gs_bern_ratio   <- renderUI({
    if (input$design_gs_bern_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_gs_bern_ratio1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_gs_bern_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    }
  })

  output$design_gs_bern_warning_ratio <- renderUI({
    if (all(any(all(input$design_gs_bern_ratio_type == "equal_exp",
                    input$design_gs_bern_ratio1 != 1),
                input$design_gs_bern_ratio_type == "root_K"),
            input$design_gs_bern_integer == TRUE)) {
      shiny::p(shiny::strong("WARNING:"), " Requiring integer sample size ",
               "with unequal allocation between the control arm and the ",
               "experimental arms can cause confusing results.")
    }
  })

  output$design_gs_bern_warning <- renderUI({
    if (any(input$design_gs_bern_K %in% c(4, 5),
            all(input$design_gs_bern_K == 3, input$design_gs_bern_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_gs_bern_density <- renderUI({
    if (input$design_gs_bern_plots) {
      shiny::selectInput(
        inputId  = "design_gs_bern_density",
        label    = "Plot quality:",
        choices  = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                     "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_gs_bern_reset, {
    shinyjs::reset("design_gs_bern_parameters")
  })

  ##### Group-sequential (Bernoulli): Plot zoom set-up #########################

  shiny::observeEvent(input$design_gs_bern_equal_error_dblclick, {
    brush_error                             <-
      input$design_gs_bern_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_gs_bern_equal_error$x   <- c(brush_error$xmin,
                                                 brush_error$xmax)
      ranges_design_gs_bern_equal_error$y   <- c(brush_error$ymin,
                                                 brush_error$ymax)
    } else {
      ranges_design_gs_bern_equal_error$x   <-
        ranges_design_gs_bern_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_bern_equal_power_dblclick, {
    brush_power                             <-
      input$design_gs_bern_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_gs_bern_equal_power$x   <- c(brush_power$xmin,
                                                 brush_power$xmax)
      ranges_design_gs_bern_equal_power$y   <- c(brush_power$ymin,
                                                 brush_power$ymax)
    } else {
      ranges_design_gs_bern_equal_power$x   <-
        ranges_design_gs_bern_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_bern_equal_other_dblclick, {
    brush_other                             <-
      input$design_gs_bern_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_gs_bern_equal_other$x   <- c(brush_other$xmin,
                                                 brush_other$xmax)
      ranges_design_gs_bern_equal_other$y   <- c(brush_other$ymin,
                                                 brush_other$ymax)
    } else {
      ranges_design_gs_bern_equal_other$x   <-
        ranges_design_gs_bern_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_bern_equal_sample_size_dblclick, {
    brush_sample_size                             <-
      input$design_gs_bern_equal_sample_size_brush
    if (!is.null(brush_sample_size)) {
      ranges_design_gs_bern_equal_sample_size$x   <- c(brush_sample_size$xmin,
                                                       brush_sample_size$xmax)
      ranges_design_gs_bern_equal_sample_size$y   <- c(brush_sample_size$ymin,
                                                       brush_sample_size$ymax)
    } else {
      ranges_design_gs_bern_equal_sample_size$x   <-
        ranges_design_gs_bern_equal_sample_size$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_bern_shifted_power_dblclick, {
    brush_shifted_power                 <-
      input$design_gs_bern_shifted_power_brush
    if (!is.null(brush_shifted_power)) {
      ranges_design_gs_bern_shifted_power$x   <- c(brush_shifted_power$xmin,
                                                   brush_shifted_power$xmax)
      ranges_design_gs_bern_shifted_power$y   <- c(brush_shifted_power$ymin,
                                                   brush_shifted_power$ymax)
    } else {
      ranges_design_gs_bern_shifted_power$x   <-
        ranges_design_gs_bern_shifted_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_bern_shifted_sample_size_dblclick, {
    brush_shifted_sample_size                 <-
      input$design_gs_bern_shifted_sample_size_brush
    if (!is.null(brush_shifted_sample_size)) {
      ranges_design_gs_bern_shifted_sample_size$x   <- c(brush_shifted_sample_size$xmin,
                                                         brush_shifted_sample_size$xmax)
      ranges_design_gs_bern_shifted_sample_size$y   <- c(brush_shifted_sample_size$ymin,
                                                         brush_shifted_sample_size$ymax)
    } else {
      ranges_design_gs_bern_shifted_sample_size$x   <-
        ranges_design_gs_bern_shifted_sample_size$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_bern_pmf_N_dblclick, {
    brush_pmf_N                 <-
      input$design_gs_bern_pmf_N_brush
    if (!is.null(brush_pmf_N)) {
      ranges_design_gs_bern_pmf_N$x   <- c(brush_pmf_N$xmin,
                                           brush_pmf_N$xmax)
      ranges_design_gs_bern_pmf_N$y   <- c(brush_pmf_N$ymin,
                                           brush_pmf_N$ymax)
    } else {
      ranges_design_gs_bern_pmf_N$x   <-
        ranges_design_gs_bern_pmf_N$y <- NULL
    }
  })

  ##### Group-sequential (Bernoulli): int_des_gs_bern() ########################

  int_des_gs_bern <- shiny::eventReactive(input$design_gs_bern_update, {
    K                     <- input$design_gs_bern_K
    seq_K                 <- 1:K
    J                     <- input$design_gs_bern_J
    pi0                   <- input$design_gs_bern_pi0
    power                 <- input$design_gs_bern_power
    lower                 <- input$design_gs_bern_lower
    upper                 <- input$design_gs_bern_upper
    if (lower == "fixed") {
      ffix                <- input$design_gs_bern_lower_fixed
    } else {
      ffix                <- -3
    }
    if (upper == "fixed") {
      efix                <- input$design_gs_bern_upper_fixed
    } else {
      efix                <- 3
    }
    swss                  <- input$design_gs_bern_swss
    stopping              <- input$design_gs_bern_stopping
    progress              <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs", value = 0)
    if (input$design_gs_bern_ratio_type == "equal_all") {
      ratio               <- 1
    } else if (input$design_gs_bern_ratio_type == "equal_exp") {
      ratio               <- input$design_gs_bern_ratio1
    } else if (input$design_gs_bern_ratio_type == "root_K") {
      ratio               <- 1/sqrt(K)
    }
    design                <-
      multiarm:::des_gs_bern(K        = K,
                             J        = J,
                             stopping = stopping,
                             type     = swss,
                             alpha    = input$design_gs_bern_alpha,
                             beta     = 1 - input$design_gs_bern_beta,
                             pi0      = input$design_gs_bern_pi0,
                             delta1   = input$design_gs_bern_delta1,
                             delta0   = input$design_gs_bern_delta0,
                             ratio    = ratio,
                             power    = power,
                             fshape   = lower,
                             eshape   = upper,
                             ffix     = ffix,
                             efix     = efix,
                             integer  = input$design_gs_bern_integer)
    progress$inc(amount  = 0.25 + as.numeric(!input$design_gs_bern_plots),
                 message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_gs_bern_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_gs_bern_summary.html"),
      params        = list(alpha      = design$alpha,
                           beta       = design$beta,
                           delta0     = design$delta0,
                           delta1     = design$delta1,
                           e          = design$e,
                           efix       = design$efix,
                           f          = design$f,
                           ffix       = design$ffix,
                           integer    = design$integer,
                           J          = design$J,
                           K          = design$K,
                           lower      = lower,
                           maxN       = design$maxN,
                           n10        = design$n10,
                           n1         = design$n1,
                           opchar     = design$opchar,
                           pi0        = input$pi0,
                           plots      = input$design_gs_bern_plots,
                           power      = design$power,
                           ratio_type = input$design_gs_bern_ratio_type,
                           ratio_init = input$design_gs_bern_ratio1,
                           ratio      = design$ratio,
                           stopping   = stopping,
                           swss       = swss,
                           upper      = upper)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_gs_bern_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_gs_bern_summary_modified.html")
    )
    design$data_og        <- design$opchar
    if (input$design_gs_bern_plots) {
      density             <- as.numeric(input$design_gs_bern_density)
      plots               <- plot(design, density = density, output = TRUE,
                                  print_plots = FALSE)
      design$boundaries   <- plots$plots$boundaries
      design$equal_power  <- plots$plots$equal_power
      design$equal_error  <- plots$plots$equal_error
      design$equal_other  <- plots$plots$equal_other
      design$equal_sample_size <- plots$plots$equal_sample_size
      design$shifted_power <- plots$plots$shifted_power
      design$shifted_sample_size <- plots$plots$shifted_sample_size
      design$pmf_N <- plots$plots$pmf_N
      progress$inc(amount = 0.25, message = "Rendering plots")
      opchar              <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(plots$opchar, 3),
                                                    .name_repair = "minimal")))
      design$data         <-
        data.frame(rbind(design$opchar, opchar),
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                                 paste0("Equal #", 1:density),
                                 paste0("Shifted #",
                                        1:(nrow(opchar) - density))))
    } else {
      design$data         <-
        data.frame(design$opchar$opchar,
                   row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      design$boundaries <- design$equal_error  <- design$equal_power <- design$equal_other <-
        design$equal_sample_size <- design$shifted_power <- design$shifted_sample_size <-
        design$pmf_N <- design$delta <- NULL
    }
    colnames(design$data) <-
      c(paste0("<i>&pi;</i><sub>", c(0, seq_K), "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
        paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
        "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
        "<i>Sensitivity</i>", "<i>Specificity</i>")
    progress$inc(amount  = 0.25 + as.numeric(!input$design_gs_bern_plots),
                 message = "Outputting results")
    design
  })

  ##### Group-sequential (Bernoulli): Value boxes ##############################

  output$design_gs_bern_n_box     <- shinydashboard::renderValueBox({
    input$design_gs_bern_update
    shinydashboard::valueBox(
      value    = round(int_des_gs_bern()$maxN, 1),
      subtitle = "Maximal possible sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_gs_bern_fwer_box  <- shinydashboard::renderValueBox({
    input$design_gs_bern_update
    if (int_des_gs_bern()$opchar$FWERI1[1] <=
        shiny::isolate(input$design_gs_bern_alpha) + 1e-4) {
      icon_choice <- "thumbs-up"
    } else {
      icon_choice <- "thumbs-down"
    }
    shinydashboard::valueBox(
      value    = round(int_des_gs_bern()$opchar$FWERI1[1], 3),
      subtitle = "Maximum family-wise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_gs_bern_power_box <- shinydashboard::renderValueBox({
    input$design_gs_bern_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(
            input$design_gs_bern_power)]
    K                 <- isolate(input$design_gs_bern_K)
    if (int_des_gs_bern()$power == "conjunctive") {
      value_power_box <- int_des_gs_bern()$opchar$Pcon[2]
    } else if (int_des_gs_bern()$power == "disjunctive") {
      value_power_box <- int_des_gs_bern()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(int_des_gs_bern()$opchar[-(1:2),
                                                    (K + 4):(2*K + 3)])))
    }
    if (value_power_box >= shiny::isolate(input$design_gs_bern_beta) - 1e-3) {
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

  ##### Group-sequential (Bernoulli): Summary ##################################

  output$design_gs_bern_summary <- shiny::renderUI({
    input$design_gs_bern_update
    N <- int_des_gs_bern()$N
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_gs_bern_summary_modified.html")
      )
    )
  })

  ##### Group-sequential (Bernoulli): Table ####################################

  output$design_gs_bern_table_key   <- DT::renderDT({
    table_key                                      <-
      int_des_gs_bern()$data[, c(1:(int_des_gs_bern()$K + 1),
                                 (int_des_gs_bern()$K + 3):
                                   (2*int_des_gs_bern()$K + 3),
                                 4*int_des_gs_bern()$K + 3)]
    colnames(table_key)[2*int_des_gs_bern()$K + 2] <- "<i>FWER</i>"
    DT::datatable(
      round(table_key, 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_gs_bern_table_error <- DT::renderDT({
    DT::datatable(
      round(int_des_gs_bern()$data[, c(1:int_des_gs_bern()$K,
                                       (2*int_des_gs_bern()$K + 3):
                                         (4*int_des_gs_bern()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_gs_bern_table_other <- DT::renderDT({
    DT::datatable(
      round(int_des_gs_bern()$data[, -((2*int_des_gs_bern()$K + 3):
                                         (4*int_des_gs_bern()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  ##### Group-sequential (Bernoulli): Plots ####################################

  output$design_gs_bern_stopping_boundaries <- shiny::renderPlot({
    input$design_gs_bern_update
    if (shiny::isolate(input$design_gs_bern_plots)) {
      int_des_gs_bern()$boundaries
    }
  })

  output$design_gs_bern_equal_error <- shiny::renderPlot({
    input$design_gs_bern_update
    if (shiny::isolate(input$design_gs_bern_plots)) {
      int_des_gs_bern()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_bern_equal_error$x,
                                 ylim   = ranges_design_gs_bern_equal_error$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_bern_equal_power <- shiny::renderPlot({
    input$design_gs_bern_update
    if (shiny::isolate(input$design_gs_bern_plots)) {
      int_des_gs_bern()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_bern_equal_power$x,
                                 ylim   = ranges_design_gs_bern_equal_power$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_bern_equal_other <- shiny::renderPlot({
    input$design_gs_bern_update
    if (shiny::isolate(input$design_gs_bern_plots)) {
      int_des_gs_bern()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_bern_equal_other$x,
                                 ylim   = ranges_design_gs_bern_equal_other$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_bern_equal_sample_size <- shiny::renderPlot({
    input$design_gs_bern_update
    if (shiny::isolate(input$design_gs_bern_plots)) {
      int_des_gs_bern()$equal_sample_size +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_bern_equal_sample_size$x,
                                 ylim   = ranges_design_gs_bern_equal_sample_size$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_bern_shifted_power     <- shiny::renderPlot({
    input$design_gs_bern_update
    if (shiny::isolate(input$design_gs_bern_plots)) {
      int_des_gs_bern()$shifted_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_bern_shifted_power$x,
                                 ylim   = ranges_design_gs_bern_shifted_power$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_bern_shifted_sample_size     <- shiny::renderPlot({
    input$design_gs_bern_update
    if (shiny::isolate(input$design_gs_bern_plots)) {
      int_des_gs_bern()$shifted_sample_size +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_bern_shifted_sample_size$x,
                                 ylim   = ranges_design_gs_bern_shifted_sample_size$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_bern_pmf_N     <- shiny::renderPlot({
    input$design_gs_bern_update
    if (shiny::isolate(input$design_gs_bern_plots)) {
      int_des_gs_bern()$pmf_N +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_bern_pmf_N$x,
                                 ylim   = ranges_design_gs_bern_pmf_N$y,
                                 expand = TRUE)
    }
  })

  ##### Group-sequential (Bernoulli): Report ###################################

  output$design_gs_bern_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_gs_bern_filename, sep = '.',
            switch(input$design_gs_bern_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_gs_bern_report.Rmd")
      file.copy("design_gs_bern_report.Rmd", tempReport, overwrite = T)
      params     <- list(alpha        = int_des_gs_bern()$alpha,
                         beta         = int_des_gs_bern()$beta,
                         delta0       = int_des_gs_bern()$delta0,
                         delta1       = int_des_gs_bern()$delta1,
                         e            = int_des_gs_bern()$e,
                         efix         = int_des_gs_bern()$efix,
                         f            = int_des_gs_bern()$f,
                         ffix         = int_des_gs_bern()$ffix,
                         integer      = int_des_gs_bern()$integer,
                         J            = int_des_gs_bern()$J,
                         K            = int_des_gs_bern()$K,
                         lower        = input$design_gs_bern_lower,
                         maxN         = int_des_gs_bern()$maxN,
                         n10          = int_des_gs_bern()$n10,
                         n1           = int_des_gs_bern()$n1,
                         opchar       = int_des_gs_bern()$opchar,
                         pi0          = int_des_gs_bern()$pi0,
                         plots        = input$design_gs_bern_plots,
                         power        = int_des_gs_bern()$power,
                         ratio_type   = input$design_gs_bern_ratio_type,
                         ratio_init   = input$design_gs_bern_ratio1,
                         ratio        = int_des_gs_bern()$ratio,
                         stopping     = int_des_gs_bern()$stopping,
                         swss         = input$design_gs_bern_swss,
                         upper        = input$design_gs_bern_upper,
                         boundaries   = int_des_gs_bern()$boundaries,
                         equal_error  = int_des_gs_bern()$equal_error,
                         equal_power  = int_des_gs_bern()$equal_power,
                         equal_other  = int_des_gs_bern()$equal_other,
                         equal_sample_size  = int_des_gs_bern()$equal_sample_size,
                         shifted_power = int_des_gs_bern()$shifted_power,
                         shifted_sample_size = int_des_gs_bern()$shifted_sample_size,
                         pmf_N = int_des_gs_bern()$pmf_N)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$design_gs_bern_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Group-sequential (normal): shinyFeedback warning messages ##############

  shiny::observeEvent(input$design_gs_norm_alpha, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_norm_alpha",
      show    = any(input$design_gs_norm_alpha <= 0,
                    input$design_gs_norm_alpha >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_gs_norm_beta, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_norm_beta",
      show    = any(input$design_gs_norm_beta <= 0,
                    input$design_gs_norm_beta >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_gs_norm_delta1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_norm_delta1",
      show    = (input$design_gs_norm_delta1 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_gs_norm_delta0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_norm_delta0",
      show    = (input$design_gs_norm_delta0 >= input$design_gs_norm_delta1),
      text    =
        "Must be strictly smaller than the interesting treatment effect")
  })

  shiny::observeEvent(input$design_gs_norm_sigma, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_norm_sigma",
      show    = (input$design_gs_norm_sigma <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_gs_norm_sigma_0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_norm_sigma_0",
      show    = (input$design_gs_norm_sigma_0 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_gs_norm_sigma_1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_norm_sigma_1",
      show    = (input$design_gs_norm_sigma_1 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_gs_norm_ratio, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_norm_ratio",
      show    = (input$design_gs_norm_ratio <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_gs_norm_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "design_gs_norm_filename",
      show    = any(strsplit(input$design_gs_norm_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Group-sequential (normal): Dynamic UI elements #########################

  output$design_gs_norm_lower_fixed <- renderUI({
    if (input$design_gs_norm_lower == "fixed") {
      shiny::numericInput(
        inputId = "design_gs_norm_lower_fixed",
        label   = "Lower fixed stopping boundary:",
        value   = 0,
        min     = -3,
        max     = 1.5,
        step    = 0.1
      )
    }
  })

  output$design_gs_norm_upper_fixed <- renderUI({
    if (input$design_gs_norm_upper == "fixed") {
      shiny::numericInput(
        inputId = "design_gs_norm_upper_fixed",
        label   = "Upper fixed stopping boundary:",
        value   = 3,
        min     = 2,
        max     = 3,
        step    = 0.1
      )
    }
  })

  output$design_gs_norm_delta0  <- renderUI({
    shiny::numericInput(
      inputId = "design_gs_norm_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = NA,
      max     = input$design_gs_norm_delta1,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black"
      )
  })

  output$design_gs_norm_sigma   <- renderUI({
    if (input$design_gs_norm_sigma_type == "equal_all") {
      shiny::numericInput(
        inputId = "design_gs_norm_sigma",
        label   = paste0("Standard deviation of the responses (arms 0, ..., ",
                         input$design_gs_norm_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.1
      )
    } else {
      shiny::tagList(
        shiny::numericInput(
          inputId = "design_gs_norm_sigma_0",
          label   = "Standard deviation of the control arm responses (arm 0):",
          value   = 1,
          min     = 0,
          max     = NA,
          step    = 0.1
        ),
        shiny::numericInput(
          inputId = "design_gs_norm_sigma_1",
          label   =
            paste0("Standard deviation of the experimental arm responses ",
                   "(arms 1, ..., ", input$design_gs_norm_K, ")"),
          value   = 1,
          min     = 0,
          max     = NA,
          step    = 0.1
        )
      )
    }
  })

  output$design_gs_norm_ratio   <- renderUI({
    if (input$design_gs_norm_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_gs_norm_ratio1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_gs_norm_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    }
  })

  output$design_gs_norm_warning_ratio <- renderUI({
    if (all(any(all(input$design_gs_norm_ratio_type == "equal_exp",
                    input$design_gs_norm_ratio1 != 1),
                input$design_gs_norm_ratio_type == "root_K"),
            input$design_gs_norm_integer == TRUE)) {
      shiny::p(shiny::strong("WARNING:"), " Requiring integer sample size ",
               "with unequal allocation between the control arm and the ",
               "experimental arms can cause confusing results.")
    }
  })

  output$design_gs_norm_warning <- renderUI({
    if (any(input$design_gs_norm_K %in% c(4, 5),
            all(input$design_gs_norm_K == 3, input$design_gs_norm_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_gs_norm_density <- renderUI({
    if (input$design_gs_norm_plots) {
      shiny::selectInput(
        inputId  = "design_gs_norm_density",
        label    = "Plot quality:",
        choices  = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                     "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_gs_norm_reset, {
    shinyjs::reset("design_gs_norm_parameters")
  })

  ##### Group-sequential (normal): Plot zoom set-up ############################

  shiny::observeEvent(input$design_gs_norm_equal_error_dblclick, {
    brush_error                             <-
      input$design_gs_norm_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_gs_norm_equal_error$x   <- c(brush_error$xmin,
                                                 brush_error$xmax)
      ranges_design_gs_norm_equal_error$y   <- c(brush_error$ymin,
                                                 brush_error$ymax)
    } else {
      ranges_design_gs_norm_equal_error$x   <-
        ranges_design_gs_norm_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_norm_equal_power_dblclick, {
    brush_power                             <-
      input$design_gs_norm_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_gs_norm_equal_power$x   <- c(brush_power$xmin,
                                                 brush_power$xmax)
      ranges_design_gs_norm_equal_power$y   <- c(brush_power$ymin,
                                                 brush_power$ymax)
    } else {
      ranges_design_gs_norm_equal_power$x   <-
        ranges_design_gs_norm_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_norm_equal_other_dblclick, {
    brush_other                             <-
      input$design_gs_norm_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_gs_norm_equal_other$x   <- c(brush_other$xmin,
                                                 brush_other$xmax)
      ranges_design_gs_norm_equal_other$y   <- c(brush_other$ymin,
                                                 brush_other$ymax)
    } else {
      ranges_design_gs_norm_equal_other$x   <-
        ranges_design_gs_norm_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_norm_equal_sample_size_dblclick, {
    brush_sample_size                             <-
      input$design_gs_norm_equal_sample_size_brush
    if (!is.null(brush_sample_size)) {
      ranges_design_gs_norm_equal_sample_size$x   <- c(brush_sample_size$xmin,
                                                       brush_sample_size$xmax)
      ranges_design_gs_norm_equal_sample_size$y   <- c(brush_sample_size$ymin,
                                                       brush_sample_size$ymax)
    } else {
      ranges_design_gs_norm_equal_sample_size$x   <-
        ranges_design_gs_norm_equal_sample_size$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_norm_shifted_power_dblclick, {
    brush_shifted_power                       <-
      input$design_gs_norm_shifted_power_brush
    if (!is.null(brush_shifted_power)) {
      ranges_design_gs_norm_shifted_power$x   <- c(brush_shifted_power$xmin,
                                                   brush_shifted_power$xmax)
      ranges_design_gs_norm_shifted_power$y   <- c(brush_shifted_power$ymin,
                                                   brush_shifted_power$ymax)
    } else {
      ranges_design_gs_norm_shifted_power$x   <-
        ranges_design_gs_norm_shifted_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_norm_shifted_sample_size_dblclick, {
    brush_shifted_sample_size                       <-
      input$design_gs_norm_shifted_sample_size_brush
    if (!is.null(brush_shifted_sample_size)) {
      ranges_design_gs_norm_shifted_sample_size$x   <- c(brush_shifted_sample_size$xmin,
                                                         brush_shifted_sample_size$xmax)
      ranges_design_gs_norm_shifted_sample_size$y   <- c(brush_shifted_sample_size$ymin,
                                                         brush_shifted_sample_size$ymax)
    } else {
      ranges_design_gs_norm_shifted_sample_size$x   <-
        ranges_design_gs_norm_shifted_sample_size$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_norm_pmf_N_dblclick, {
    brush_pmf_N                       <-
      input$design_gs_norm_pmf_N_brush
    if (!is.null(brush_pmf_N)) {
      ranges_design_gs_norm_pmf_N$x   <- c(brush_pmf_N$xmin,
                                           brush_pmf_N$xmax)
      ranges_design_gs_norm_pmf_N$y   <- c(brush_pmf_N$ymin,
                                           brush_pmf_N$ymax)
    } else {
      ranges_design_gs_norm_pmf_N$x   <-
        ranges_design_gs_norm_pmf_N$y <- NULL
    }
  })


  ##### Group-sequential (normal): int_des_gs_norm() ###########################

  int_des_gs_norm <- shiny::eventReactive(input$design_gs_norm_update, {
    K                     <- input$design_gs_norm_K
    seq_K                 <- 1:K
    J                     <- input$design_gs_norm_J
    power                 <- input$design_gs_norm_power
    lower                 <- input$design_gs_norm_lower
    upper                 <- input$design_gs_norm_upper
    if (lower == "fixed") {
      ffix                <- input$design_gs_norm_lower_fixed
    } else {
      ffix                <- -3
    }
    if (upper == "fixed") {
      efix                <- input$design_gs_norm_upper_fixed
    } else {
      efix                <- 3
    }
    swss                  <- input$design_gs_norm_swss
    stopping              <- input$design_gs_norm_stopping
    progress              <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs", value = 0)
    if (input$design_gs_norm_sigma_type == "equal_all") {
      sigma               <- input$design_gs_norm_sigma
    } else if (input$design_gs_norm_sigma_type == "equal_exp") {
      sigma               <- c(input$design_gs_norm_sigma_0,
                               input$design_gs_norm_sigma_1)
    }
    if (input$design_gs_norm_ratio_type == "equal_all") {
      ratio               <- 1
    } else if (input$design_gs_norm_ratio_type == "equal_exp") {
      ratio               <- input$design_gs_norm_ratio1
    } else if (input$design_gs_norm_ratio_type == "root_K") {
      ratio               <- 1/sqrt(K)
    }
    design                <-
      multiarm:::des_gs_norm(K        = K,
                             J        = J,
                             stopping = stopping,
                             type     = swss,
                             alpha    = input$design_gs_norm_alpha,
                             beta     = 1 - input$design_gs_norm_beta,
                             delta1   = input$design_gs_norm_delta1,
                             delta0   = input$design_gs_norm_delta0,
                             sigma    = sigma,
                             ratio    = ratio,
                             power    = power,
                             fshape   = lower,
                             eshape   = upper,
                             ffix     = ffix,
                             efix     = efix,
                             integer  = input$design_gs_norm_integer)
    progress$inc(amount  = 0.25 + as.numeric(!input$design_gs_norm_plots),
                 message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_gs_norm_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_gs_norm_summary.html"),
      params        = list(alpha      = design$alpha,
                           beta       = design$beta,
                           delta0     = design$delta0,
                           delta1     = design$delta1,
                           e          = design$e,
                           efix       = design$efix,
                           f          = design$f,
                           ffix       = design$ffix,
                           integer    = design$integer,
                           J          = design$J,
                           K          = design$K,
                           lower      = lower,
                           maxN       = design$maxN,
                           n10        = design$n10,
                           n1         = design$n1,
                           opchar     = design$opchar,
                           plots      = input$design_gs_norm_plots,
                           power      = design$power,
                           ratio_type = input$design_gs_norm_ratio_type,
                           ratio_init = input$design_gs_norm_ratio1,
                           ratio      = design$ratio,
                           sigma      = design$sigma,
                           stopping   = stopping,
                           swss       = swss,
                           upper      = upper)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_gs_norm_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_gs_norm_summary_modified.html")
    )
    design$data_og        <- design$opchar
    if (input$design_gs_norm_plots) {
      density             <- as.numeric(input$design_gs_norm_density)
      plots               <- plot(design, density = density, output = TRUE,
                                  print_plots = FALSE)
      design$boundaries   <- plots$plots$boundaries
      design$equal_power  <- plots$plots$equal_power
      design$equal_error  <- plots$plots$equal_error
      design$equal_other  <- plots$plots$equal_other
      design$equal_sample_size  <- plots$plots$equal_sample_size
      design$shifted_power <- plots$plots$shifted_power
      design$shifted_sample_size <- plots$plots$shifted_sample_size
      design$pmf_N <- plots$plots$pmf_N
      progress$inc(amount = 0.25, message = "Rendering plots")
      opchar              <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(plots$opchar, 3),
                                                    .name_repair = "minimal")))
      design$data         <-
        data.frame(rbind(design$opchar, opchar),
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                                 paste0("Equal #", 1:density),
                                 paste0("Shifted #", 1:density)))
    } else {
      design$data         <-
        data.frame(design$opchar$opchar,
                   row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      design$boundaries <- design$equal_error  <- design$equal_power <- design$equal_other <- design$equal_sample_size <-
        design$shifted_power <- design$shifted_sample_size <- design$pmf_N <- design$delta       <- NULL
    }
    colnames(design$data) <-
      c(paste0("<i>&tau;</i><sub>", seq_K, "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
        paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
        "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
        "<i>Sensitivity</i>", "<i>Specificity</i>",
        "<i>ESS</i>", "<i>SDSS</i>", "<i>MSS</i>", "<i>max N</i>")
    progress$inc(amount  = 0.25 + as.numeric(!input$design_gs_norm_plots),
                 message = "Outputting results")
    design
  })

  ##### Group-sequential (normal): Value boxes #################################

  output$design_gs_norm_n_box     <- shinydashboard::renderValueBox({
    input$design_gs_norm_update
    shinydashboard::valueBox(
      value    = round(int_des_gs_norm()$maxN, 1),
      subtitle = "Maximal possible sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_gs_norm_fwer_box  <- shinydashboard::renderValueBox({
    input$design_gs_norm_update
    if (int_des_gs_norm()$opchar$FWERI1[1] <=
        shiny::isolate(input$design_gs_norm_alpha) + 1e-4) {
      icon_choice <- "thumbs-up"
    } else {
      icon_choice <- "thumbs-down"
    }
    shinydashboard::valueBox(
      value    = round(int_des_gs_norm()$opchar$FWERI1[1], 3),
      subtitle = "Maximum family-wise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_gs_norm_power_box <- shinydashboard::renderValueBox({
    input$design_gs_norm_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(
            input$design_gs_norm_power)]
    K                 <- isolate(input$design_gs_norm_K)
    if (int_des_gs_norm()$power == "conjunctive") {
      value_power_box <- int_des_gs_norm()$opcharPcon[2]
    } else if (int_des_gs_norm()$power == "disjunctive") {
      value_power_box <- int_des_gs_norm()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(int_des_gs_norm()$opchar[-(1:2),
                                                    (K + 3):(2*K + 2)])))
    }
    if (value_power_box >= shiny::isolate(input$design_gs_norm_beta) - 1e-3) {
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

  ##### Group-sequential (normal): Summary #####################################

  output$design_gs_norm_summary <- shiny::renderUI({
    input$design_gs_norm_update
    n1 <- int_des_gs_norm()$n1
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_gs_norm_summary_modified.html")
      )
    )
  })

  ##### Group-sequential (normal): Table #######################################

  output$design_gs_norm_table_key   <- DT::renderDT({
    table_key                                      <-
      int_des_gs_norm()$data[, c(1:(int_des_gs_norm()$K + 1),
                                 (int_des_gs_norm()$K + 3):
                                   (2*int_des_gs_norm()$K + 3),
                                 4*int_des_gs_norm()$K + 3)]
    colnames(table_key)[2*int_des_gs_norm()$K + 2] <- "<i>FWER</i>"
    DT::datatable(
      round(table_key, 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_gs_norm_table_error <- DT::renderDT({
    DT::datatable(
      round(int_des_gs_norm()$data[, c(1:int_des_gs_norm()$K,
                                       (2*int_des_gs_norm()$K + 3):
                                         (4*int_des_gs_norm()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_gs_norm_table_other <- DT::renderDT({
    DT::datatable(
      round(int_des_gs_norm()$data[, -((2*int_des_gs_norm()$K + 3):
                                         (4*int_des_gs_norm()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  ##### Group-sequential (normal): Plots #######################################

  output$design_gs_norm_stopping_boundaries <- shiny::renderPlot({
    input$design_gs_norm_update
    if (shiny::isolate(input$design_gs_norm_plots)) {
      int_des_gs_norm()$boundaries
    }
  })

  output$design_gs_norm_equal_error <- shiny::renderPlot({
    input$design_gs_norm_update
    if (shiny::isolate(input$design_gs_norm_plots)) {
      int_des_gs_norm()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_norm_equal_error$x,
                                 ylim   = ranges_design_gs_norm_equal_error$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_norm_equal_power <- shiny::renderPlot({
    input$design_gs_norm_update
    if (shiny::isolate(input$design_gs_norm_plots)) {
      int_des_gs_norm()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_norm_equal_power$x,
                                 ylim   = ranges_design_gs_norm_equal_power$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_norm_equal_other <- shiny::renderPlot({
    input$design_gs_norm_update
    if (shiny::isolate(input$design_gs_norm_plots)) {
      int_des_gs_norm()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_norm_equal_other$x,
                                 ylim   = ranges_design_gs_norm_equal_other$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_norm_equal_sample_size <- shiny::renderPlot({
    input$design_gs_norm_update
    if (shiny::isolate(input$design_gs_norm_plots)) {
      int_des_gs_norm()$equal_sample_size +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_norm_equal_sample_size$x,
                                 ylim   = ranges_design_gs_norm_equal_sample_size$y,
                                 expand = TRUE)
    }
  })


  output$design_gs_norm_shifted_power     <- shiny::renderPlot({
    input$design_gs_norm_update
    if (shiny::isolate(input$design_gs_norm_plots)) {
      int_des_gs_norm()$shifted_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_norm_shifted_power$x,
                                 ylim   = ranges_design_gs_norm_shifted_power$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_norm_shifted_sample_size     <- shiny::renderPlot({
    input$design_gs_norm_update
    if (shiny::isolate(input$design_gs_norm_plots)) {
      int_des_gs_norm()$shifted_sample_size +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_norm_shifted_sample_size$x,
                                 ylim   = ranges_design_gs_norm_shifted_sample_size$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_norm_pmf_N     <- shiny::renderPlot({
    input$design_gs_norm_update
    if (shiny::isolate(input$design_gs_norm_plots)) {
      int_des_gs_norm()$pmf_N +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_norm_pmf_N$x,
                                 ylim   = ranges_design_gs_norm_pmf_N$y,
                                 expand = TRUE)
    }
  })

  ##### Group-sequential (normal): Report ######################################

  output$design_gs_norm_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_gs_norm_filename, sep = '.',
            switch(input$design_gs_norm_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_gs_norm_report.Rmd")
      file.copy("design_gs_norm_report.Rmd", tempReport, overwrite = TRUE)
      params     <- list(alpha        = int_des_gs_norm()$alpha,
                         beta         = int_des_gs_norm()$beta,
                         delta0       = int_des_gs_norm()$delta0,
                         delta1       = int_des_gs_norm()$delta1,
                         e            = int_des_gs_norm()$e,
                         efix         = int_des_gs_norm()$efix,
                         f            = int_des_gs_norm()$f,
                         ffix         = int_des_gs_norm()$ffix,
                         integer      = int_des_gs_norm()$integer,
                         J            = int_des_gs_norm()$J,
                         K            = int_des_gs_norm()$K,
                         lower        = input$design_gs_norm_lower,
                         maxN         = int_des_gs_norm()$maxN,
                         n10          = int_des_gs_norm()$n10,
                         n1           = int_des_gs_norm()$n1,
                         opchar       = int_des_gs_norm()$opchar,
                         plots        = input$design_gs_norm_plots,
                         power        = int_des_gs_norm()$power,
                         ratio_type   = input$design_gs_norm_ratio_type,
                         ratio_init   = input$design_gs_norm_ratio1,
                         ratio        = int_des_gs_norm()$ratio,
                         sigma        = int_des_gs_norm()$sigma,
                         stopping     = int_des_gs_norm()$stopping,
                         swss         = input$design_gs_norm_swss,
                         upper        = input$design_gs_norm_upper,
                         boundaries   = int_des_gs_norm()$boundaries,
                         equal_error  = int_des_gs_norm()$equal_error,
                         equal_power  = int_des_gs_norm()$equal_power,
                         equal_other  = int_des_gs_norm()$equal_other,
                         equal_sample_size  = int_des_gs_norm()$equal_sample_size,
                         shifted_power      = int_des_gs_norm()$shifted_power,
                         shifted_sample_size      = int_des_gs_norm()$shifted_sample_size,
                         pmf_N      = int_des_gs_norm()$pmf_N)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$design_gs_norm_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Group-sequential (Poisson): shinyFeedback warning messages #############

  shiny::observeEvent(input$design_gs_pois_alpha, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_pois_alpha",
      show    = any(input$design_gs_pois_alpha <= 0,
                    input$design_gs_pois_alpha >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_gs_pois_beta, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_pois_beta",
      show    = any(input$design_gs_pois_beta <= 0,
                    input$design_gs_pois_beta >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_gs_pois_lambda0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_pois_lambda0",
      show    = (input$design_gs_pois_lambda0 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_gs_pois_delta1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_pois_delta1",
      show    = (input$design_gs_pois_delta1 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_gs_pois_delta0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_pois_delta0",
      show    = any(input$design_gs_pois_delta0 >=
                      input$design_gs_pois_delta1,
                    input$design_gs_pois_delta0 <=
                      -input$design_gs_pois_lambda0),
      text    = paste0("Must be strictly between minus the control arm ",
                       "event rate and the interesting treatment effect"))
  })

  shiny::observeEvent(input$design_gs_pois_ratio, {
    shinyFeedback::feedbackDanger(
      inputId = "design_gs_pois_ratio",
      show    = (input$design_gs_pois_ratio <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_gs_pois_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "design_gs_pois_filename",
      show    = any(strsplit(input$design_gs_pois_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Group-sequential (Poisson): Dynamic UI elements ########################

  output$design_gs_pois_lower_fixed <- renderUI({
    if (input$design_gs_pois_lower == "fixed") {
      shiny::numericInput(
        inputId = "design_gs_pois_lower_fixed",
        label   = "Lower fixed stopping boundary:",
        value   = 0,
        min     = -3,
        max     = 1.5,
        step    = 0.1
      )
    }
  })

  output$design_gs_pois_upper_fixed <- renderUI({
    if (input$design_gs_pois_upper == "fixed") {
      shiny::numericInput(
        inputId = "design_gs_pois_upper_fixed",
        label   = "Upper fixed stopping boundary:",
        value   = 3,
        min     = 2,
        max     = 3,
        step    = 0.1
      )
    }
  })

  output$design_gs_pois_delta   <- renderUI({
    inputTagList <-
      shiny::tagList(
        (shiny::numericInput(
          inputId = "design_gs_pois_delta1",
          label   = "Interesting treatment effect:",
          value   = 1,
          min     = 0,
          step    = 0.1
        ) %>%
          shinyhelper:: helper(
            type    = "markdown",
            title   = "",
            content = "design_delta1",
            size    = "m",
            colour  = "black"
          ))
      )
    newInput     <- (shiny::numericInput(
      inputId = "design_gs_pois_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = -input$design_gs_pois_lambda0,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black"
      ))
    inputTagList <- tagAppendChild(inputTagList, newInput)
    inputTagList
  })

  output$design_gs_pois_ratio   <- renderUI({
    if (input$design_gs_pois_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_gs_pois_ratio1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_gs_pois_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    }
  })

  output$design_gs_pois_warning_ratio <- renderUI({
    if (all(any(all(input$design_gs_pois_ratio_type == "equal_exp",
                    input$design_gs_pois_ratio1 != 1),
                input$design_gs_pois_ratio_type == "root_K"),
            input$design_gs_pois_integer == TRUE)) {
      shiny::p(shiny::strong("WARNING:"), " Requiring integer sample size ",
               "with unequal allocation between the control arm and the ",
               "experimental arms can cause confusing results.")
    }
  })

  output$design_gs_pois_warning <- renderUI({
    if (any(input$design_gs_pois_K %in% c(4, 5),
            all(input$design_gs_pois_K == 3, input$design_gs_pois_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_gs_pois_density <- renderUI({
    if (input$design_gs_pois_plots) {
      shiny::selectInput(
        inputId  = "design_gs_pois_density",
        label    = "Plot quality:",
        choices  = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                     "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_gs_pois_reset, {
    shinyjs::reset("design_gs_pois_parameters")
  })

  ##### Group-sequential (Poisson): Plot zoom set-up ###########################

  shiny::observeEvent(input$design_gs_pois_equal_error_dblclick, {
    brush_error                             <-
      input$design_gs_pois_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_gs_pois_equal_error$x   <- c(brush_error$xmin,
                                                 brush_error$xmax)
      ranges_design_gs_pois_equal_error$y   <- c(brush_error$ymin,
                                                 brush_error$ymax)
    } else {
      ranges_design_gs_pois_equal_error$x   <-
        ranges_design_gs_pois_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_pois_equal_power_dblclick, {
    brush_power                             <-
      input$design_gs_pois_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_gs_pois_equal_power$x   <- c(brush_power$xmin,
                                                 brush_power$xmax)
      ranges_design_gs_pois_equal_power$y   <- c(brush_power$ymin,
                                                 brush_power$ymax)
    } else {
      ranges_design_gs_pois_equal_power$x   <-
        ranges_design_gs_pois_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_pois_equal_other_dblclick, {
    brush_other                             <-
      input$design_gs_pois_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_gs_pois_equal_other$x   <- c(brush_other$xmin,
                                                 brush_other$xmax)
      ranges_design_gs_pois_equal_other$y   <- c(brush_other$ymin,
                                                 brush_other$ymax)
    } else {
      ranges_design_gs_pois_equal_other$x   <-
        ranges_design_gs_pois_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_pois_equal_sample_size_dblclick, {
    brush_sample_size                             <-
      input$design_gs_pois_equal_sample_size_brush
    if (!is.null(brush_sample_size)) {
      ranges_design_gs_pois_equal_sample_size$x   <- c(brush_sample_size$xmin,
                                                       brush_sample_size$xmax)
      ranges_design_gs_pois_equal_sample_size$y   <- c(brush_sample_size$ymin,
                                                       brush_sample_size$ymax)
    } else {
      ranges_design_gs_pois_equal_sample_size$x   <-
        ranges_design_gs_pois_equal_sample_size$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_pois_shifted_power_dblclick, {
    brush_shifted_power                 <-
      input$design_gs_pois_shifted_power_brush
    if (!is.null(brush_shifted_power)) {
      ranges_design_gs_pois_shifted_power$x   <- c(brush_shifted_power$xmin,
                                                   brush_shifted_power$xmax)
      ranges_design_gs_pois_shifted_power$y   <- c(brush_shifted_power$ymin,
                                                   brush_shifted_power$ymax)
    } else {
      ranges_design_gs_pois_shifted_power$x   <-
        ranges_design_gs_pois_shifted_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_pois_shifted_sample_size_dblclick, {
    brush_shifted_sample_size                 <-
      input$design_gs_pois_shifted_sample_size_brush
    if (!is.null(brush_shifted_sample_size)) {
      ranges_design_gs_pois_shifted_sample_size$x   <- c(brush_shifted_sample_size$xmin,
                                                         brush_shifted_sample_size$xmax)
      ranges_design_gs_pois_shifted_sample_size$y   <- c(brush_shifted_sample_size$ymin,
                                                         brush_shifted_sample_size$ymax)
    } else {
      ranges_design_gs_pois_shifted_sample_size$x   <-
        ranges_design_gs_pois_shifted_sample_size$y <- NULL
    }
  })

  shiny::observeEvent(input$design_gs_pois_pmf_N_dblclick, {
    brush_pmf_N                 <-
      input$design_gs_pois_pmf_N_brush
    if (!is.null(brush_pmf_N)) {
      ranges_design_gs_pois_pmf_N$x   <- c(brush_pmf_N$xmin,
                                           brush_pmf_N$xmax)
      ranges_design_gs_pois_pmf_N$y   <- c(brush_pmf_N$ymin,
                                           brush_pmf_N$ymax)
    } else {
      ranges_design_gs_pois_pmf_N$x   <-
        ranges_design_gs_pois_pmf_N$y <- NULL
    }
  })

  ##### Group-sequential (Poisson): int_des_gs_pois() ##########################

  int_des_gs_pois <- shiny::eventReactive(input$design_gs_pois_update, {
    K                     <- input$design_gs_pois_K
    seq_K                 <- 1:K
    J                     <- input$design_gs_pois_J
    lambda0               <- input$design_gs_pois_lambda0
    power                 <- input$design_gs_pois_power
    lower                 <- input$design_gs_pois_lower
    upper                 <- input$design_gs_pois_upper
    if (lower == "fixed") {
      ffix                <- input$design_gs_pois_lower_fixed
    } else {
      ffix                <- -3
    }
    if (upper == "fixed") {
      efix                <- input$design_gs_pois_upper_fixed
    } else {
      efix                <- 3
    }
    swss                  <- input$design_gs_pois_swss
    stopping              <- input$design_gs_pois_stopping
    progress              <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs", value = 0)
    if (input$design_gs_pois_ratio_type == "equal_all") {
      ratio               <- 1
    } else if (input$design_gs_pois_ratio_type == "equal_exp") {
      ratio               <- input$design_gs_pois_ratio1
    } else if (input$design_gs_pois_ratio_type == "root_K") {
      ratio               <- 1/sqrt(K)
    }
    design                <-
      multiarm:::des_gs_pois(K        = K,
                             J        = J,
                             stopping = stopping,
                             type     = swss,
                             alpha    = input$design_gs_pois_alpha,
                             beta     = 1 - input$design_gs_pois_beta,
                             lambda0  = input$design_gs_pois_lambda0,
                             delta1   = input$design_gs_pois_delta1,
                             delta0   = input$design_gs_pois_delta0,
                             ratio    = ratio,
                             power    = power,
                             fshape   = lower,
                             eshape   = upper,
                             ffix     = ffix,
                             efix     = efix,
                             integer  = input$design_gs_pois_integer)
    progress$inc(amount  = 0.25 + as.numeric(!input$design_gs_pois_plots),
                 message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_gs_pois_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_gs_pois_summary.html"),
      params        = list(alpha      = design$alpha,
                           beta       = design$beta,
                           delta0     = design$delta0,
                           delta1     = design$delta1,
                           e          = design$e,
                           efix       = design$efix,
                           f          = design$f,
                           ffix       = design$ffix,
                           integer    = design$integer,
                           J          = design$J,
                           K          = design$K,
                           lower      = lower,
                           maxN       = design$maxN,
                           n10        = design$n10,
                           n1         = design$n1,
                           opchar     = design$opchar,
                           lambda0    = input$lambda0,
                           plots      = input$design_gs_pois_plots,
                           power      = design$power,
                           ratio_type = input$design_gs_pois_ratio_type,
                           ratio_init = input$design_gs_pois_ratio1,
                           ratio      = design$ratio,
                           stopping   = stopping,
                           swss       = swss,
                           upper      = upper)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_gs_pois_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_gs_pois_summary_modified.html")
    )
    design$data_og        <- design$opchar
    if (input$design_gs_pois_plots) {
      density             <- as.numeric(input$design_gs_pois_density)
      plots               <- plot(design, density = density, output = TRUE,
                                  print_plots = FALSE)
      design$boundaries   <- plots$plots$boundaries
      design$equal_power  <- plots$plots$equal_power
      design$equal_error  <- plots$plots$equal_error
      design$equal_other  <- plots$plots$equal_other
      design$equal_sample_size <- plots$plots$equal_sample_size
      design$shifted_power <- plots$plots$shifted_power
      design$shifted_sample_size <- plots$plots$shifted_sample_size
      design$pmf_N <- plots$plots$pmf_N
      progress$inc(amount = 0.25, message = "Rendering plots")
      opchar              <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(plots$opchar, 3),
                                                    .name_repair = "minimal")))
      design$data         <-
        data.frame(rbind(design$opchar, opchar),
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                                 paste0("Equal #", 1:density),
                                 paste0("Shifted #",
                                        1:(nrow(opchar) - density))))
    } else {
      design$data         <-
        data.frame(design$opchar$opchar,
                   row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      design$boundaries <- design$equal_error  <- design$equal_power <- design$equal_other <-
        design$equal_sample_size <- design$shifted_power <- design$shifted_sample_size <-
        design$pmf_N <- design$delta <- NULL
    }
    colnames(design$data) <-
      c(paste0("<i>&lambda;</i><sub>", c(0, seq_K), "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
        paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
        "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
        "<i>Sensitivity</i>", "<i>Specificity</i>")
    progress$inc(amount  = 0.25 + as.numeric(!input$design_gs_pois_plots),
                 message = "Outputting results")
    design
  })

  ##### Group-sequential (Poisson): Value boxes ################################

  output$design_gs_pois_n_box     <- shinydashboard::renderValueBox({
    input$design_gs_pois_update
    shinydashboard::valueBox(
      value    = round(int_des_gs_pois()$maxN, 1),
      subtitle = "Maximal possible sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_gs_pois_fwer_box  <- shinydashboard::renderValueBox({
    input$design_gs_pois_update
    if (int_des_gs_pois()$opchar$FWERI1[1] <=
        shiny::isolate(input$design_gs_pois_alpha) + 1e-4) {
      icon_choice <- "thumbs-up"
    } else {
      icon_choice <- "thumbs-down"
    }
    shinydashboard::valueBox(
      value    = round(int_des_gs_pois()$opchar$FWERI1[1], 3),
      subtitle = "Maximum family-wise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_gs_pois_power_box <- shinydashboard::renderValueBox({
    input$design_gs_pois_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(
            input$design_gs_pois_power)]
    K                 <- isolate(input$design_gs_pois_K)
    if (int_des_gs_pois()$power == "conjunctive") {
      value_power_box <- int_des_gs_pois()$opchar$Pcon[2]
    } else if (int_des_gs_pois()$power == "disjunctive") {
      value_power_box <- int_des_gs_pois()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(int_des_gs_pois()$opchar[-(1:2),
                                                    (K + 4):(2*K + 3)])))
    }
    if (value_power_box >= shiny::isolate(input$design_gs_pois_beta) - 1e-3) {
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

  ##### Group-sequential (Poisson): Summary ####################################

  output$design_gs_pois_summary <- shiny::renderUI({
    input$design_gs_pois_update
    N <- int_des_gs_pois()$N
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_gs_pois_summary_modified.html")
      )
    )
  })

  ##### Group-sequential (Poisson): Table ######################################

  output$design_gs_pois_table_key   <- DT::renderDT({
    table_key                                      <-
      int_des_gs_pois()$data[, c(1:(int_des_gs_pois()$K + 1),
                                 (int_des_gs_pois()$K + 3):
                                   (2*int_des_gs_pois()$K + 3),
                                 4*int_des_gs_pois()$K + 3)]
    colnames(table_key)[2*int_des_gs_pois()$K + 2] <- "<i>FWER</i>"
    DT::datatable(
      round(table_key, 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_gs_pois_table_error <- DT::renderDT({
    DT::datatable(
      round(int_des_gs_pois()$data[, c(1:int_des_gs_pois()$K,
                                       (2*int_des_gs_pois()$K + 3):
                                         (4*int_des_gs_pois()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_gs_pois_table_other <- DT::renderDT({
    DT::datatable(
      round(int_des_gs_pois()$data[, -((2*int_des_gs_pois()$K + 3):
                                         (4*int_des_gs_pois()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  ##### Group-sequential (Poisson): Plots ######################################

  output$design_gs_pois_stopping_boundaries <- shiny::renderPlot({
    input$design_gs_pois_update
    if (shiny::isolate(input$design_gs_pois_plots)) {
      int_des_gs_pois()$boundaries
    }
  })

  output$design_gs_pois_equal_error <- shiny::renderPlot({
    input$design_gs_pois_update
    if (shiny::isolate(input$design_gs_pois_plots)) {
      int_des_gs_pois()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_pois_equal_error$x,
                                 ylim   = ranges_design_gs_pois_equal_error$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_pois_equal_power <- shiny::renderPlot({
    input$design_gs_pois_update
    if (shiny::isolate(input$design_gs_pois_plots)) {
      int_des_gs_pois()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_pois_equal_power$x,
                                 ylim   = ranges_design_gs_pois_equal_power$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_pois_equal_other <- shiny::renderPlot({
    input$design_gs_pois_update
    if (shiny::isolate(input$design_gs_pois_plots)) {
      int_des_gs_pois()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_pois_equal_other$x,
                                 ylim   = ranges_design_gs_pois_equal_other$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_pois_equal_sample_size <- shiny::renderPlot({
    input$design_gs_pois_update
    if (shiny::isolate(input$design_gs_pois_plots)) {
      int_des_gs_pois()$equal_sample_size +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_pois_equal_sample_size$x,
                                 ylim   = ranges_design_gs_pois_equal_sample_size$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_pois_shifted_power     <- shiny::renderPlot({
    input$design_gs_pois_update
    if (shiny::isolate(input$design_gs_pois_plots)) {
      int_des_gs_pois()$shifted_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_pois_shifted_power$x,
                                 ylim   = ranges_design_gs_pois_shifted_power$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_pois_shifted_sample_size     <- shiny::renderPlot({
    input$design_gs_pois_update
    if (shiny::isolate(input$design_gs_pois_plots)) {
      int_des_gs_pois()$shifted_sample_size +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_pois_shifted_sample_size$x,
                                 ylim   = ranges_design_gs_pois_shifted_sample_size$y,
                                 expand = TRUE)
    }
  })

  output$design_gs_pois_pmf_N     <- shiny::renderPlot({
    input$design_gs_pois_update
    if (shiny::isolate(input$design_gs_pois_plots)) {
      int_des_gs_pois()$pmf_N +
        ggplot2::coord_cartesian(xlim   = ranges_design_gs_pois_pmf_N$x,
                                 ylim   = ranges_design_gs_pois_pmf_N$y,
                                 expand = TRUE)
    }
  })

  ##### Group-sequential (Poisson): Report #####################################

  output$design_gs_pois_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_gs_pois_filename, sep = '.',
            switch(input$design_gs_pois_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_gs_pois_report.Rmd")
      file.copy("design_gs_pois_report.Rmd", tempReport, overwrite = T)
      params     <- list(alpha        = int_des_gs_pois()$alpha,
                         beta         = int_des_gs_pois()$beta,
                         delta0       = int_des_gs_pois()$delta0,
                         delta1       = int_des_gs_pois()$delta1,
                         e            = int_des_gs_pois()$e,
                         efix         = int_des_gs_pois()$efix,
                         f            = int_des_gs_pois()$f,
                         ffix         = int_des_gs_pois()$ffix,
                         integer      = int_des_gs_pois()$integer,
                         J            = int_des_gs_pois()$J,
                         K            = int_des_gs_pois()$K,
                         lambda0      = int_des_gs_pois()$lambda0,
                         lower        = input$design_gs_pois_lower,
                         maxN         = int_des_gs_pois()$maxN,
                         n10          = int_des_gs_pois()$n10,
                         n1           = int_des_gs_pois()$n1,
                         opchar       = int_des_gs_pois()$opchar,
                         plots        = input$design_gs_pois_plots,
                         power        = int_des_gs_pois()$power,
                         ratio_type   = input$design_gs_pois_ratio_type,
                         ratio_init   = input$design_gs_pois_ratio1,
                         ratio        = int_des_gs_pois()$ratio,
                         stopping     = int_des_gs_pois()$stopping,
                         swss         = input$design_gs_pois_swss,
                         upper        = input$design_gs_pois_upper,
                         boundaries   = int_des_gs_pois()$boundaries,
                         equal_error  = int_des_gs_pois()$equal_error,
                         equal_power  = int_des_gs_pois()$equal_power,
                         equal_other  = int_des_gs_pois()$equal_other,
                         equal_sample_size  = int_des_gs_pois()$equal_sample_size,
                         shifted_power = int_des_gs_pois()$shifted_power,
                         shifted_sample_size = int_des_gs_pois()$shifted_sample_size,
                         pmf_N = int_des_gs_pois()$pmf_N)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$design_gs_pois_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Drop-the-losers (Bernoulli): shinyFeedback warning messages ############

  shiny::observeEvent(input$design_dtl_bern_alpha, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_bern_alpha",
      show    = any(input$design_dtl_bern_alpha <= 0,
                    input$design_dtl_bern_alpha >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_dtl_bern_beta, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_bern_beta",
      show    = any(input$design_dtl_bern_beta <= 0,
                    input$design_dtl_bern_beta >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_dtl_bern_pi0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_bern_pi0",
      show    = any(input$design_dtl_bern_pi0 <= 0,
                    input$design_dtl_bern_pi0 >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_dtl_bern_delta1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_bern_delta1",
      show    = any(input$design_dtl_bern_delta1 <= 0,
                    input$design_dtl_bern_delta1 >=
                      1 - input$design_dtl_bern_pi0),
      text    = paste0("Must be strictly between 0 and one minus the control",
                       " arm response rate"))
  })

  shiny::observeEvent(input$design_dtl_bern_delta0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_bern_delta0",
      show    = any(input$design_dtl_bern_delta0 >=
                      input$design_dtl_bern_delta1,
                    input$design_dtl_bern_delta0 <= -input$design_dtl_bern_pi0),
      text    = paste0("Must be strictly between minus the control arm ",
                       "response rate and the interesting treatment effect"))
  })

  shiny::observeEvent(input$design_dtl_bern_ratio, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_bern_ratio",
      show    = (input$design_dtl_bern_ratio <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_dtl_bern_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "design_dtl_bern_filename",
      show    = any(strsplit(input$design_dtl_bern_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Drop-the-losers (Bernoulli): Dynamic UI elements #######################

  output$design_dtl_bern_J <- renderUI({
    shiny::sliderInput(
      inputId = "design_dtl_bern_J",
      label   = "Number of stages:",
      min     = 2,
      max     = min(input$design_dtl_bern_K, 4),
      value   = 2,
      step    = 1
    ) %>%
      shinyhelper::helper(
        type    = "markdown",
        title   = "",
        content = "design_J",
        size    = "m",
        colour  = "black"
      )
  })

  output$design_dtl_bern_Kv_2     <- renderUI({
    shiny::sliderInput(
      inputId = "design_dtl_bern_Kv_2",
      label   = "Number of experimental arms in stage 2:",
      value   = input$design_dtl_bern_K - 1,
      min     = input$design_dtl_bern_J - 1,
      max     = input$design_dtl_bern_K - 1,
      step    = 1
    )
  })

  output$design_dtl_bern_Kv_3     <- renderUI({
    if (input$design_dtl_bern_J >= 3) {
      shiny::sliderInput(
        inputId = "design_dtl_bern_Kv_3",
        label   = "Number of experimental arms in stage 3:",
        value   = input$design_dtl_bern_Kv_2 - 1,
        min     = input$design_dtl_bern_J - 2,
        max     = input$design_dtl_bern_Kv_2 - 1,
        step    = 1
      )
    }
  })

  output$design_dtl_bern_Kv_4     <- renderUI({
    if (input$design_dtl_bern_J >= 4) {
      shiny::sliderInput(
        inputId = "design_dtl_bern_Kv_4",
        label   = "Number of experimental arms in stage 4:",
        value   = input$design_dtl_bern_Kv_3 - 1,
        min     = 1,
        max     = input$design_dtl_bern_Kv_3 - 1,
        step    = 1
      )
    }
  })

  output$design_dtl_bern_delta   <- renderUI({
    inputTagList <-
      shiny::tagList(
        (shiny::numericInput(
          inputId = "design_dtl_bern_delta1",
          label   = "Interesting treatment effect:",
          value   = 0.2,
          min     = 0,
          max     = 1 - input$design_dtl_bern_pi0,
          step    = 0.1
        ) %>%
          shinyhelper:: helper(
            type    = "markdown",
            title   = "",
            content = "design_delta1",
            size    = "m",
            colour  = "black"
          ))
      )
    newInput     <- (shiny::numericInput(
      inputId = "design_dtl_bern_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = -input$design_dtl_bern_pi0,
      max     = 1 - input$design_dtl_bern_pi0,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black"
      ))
    inputTagList <- tagAppendChild(inputTagList, newInput)
    inputTagList
  })


  output$design_dtl_bern_ratio   <- renderUI({
    if (input$design_dtl_bern_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_dtl_bern_ratio1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_dtl_bern_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    }
  })

  output$design_dtl_bern_warning_ratio <- renderUI({
    if (all(any(all(input$design_dtl_bern_ratio_type == "equal_exp",
                    input$design_dtl_bern_ratio1 != 1),
                input$design_dtl_bern_ratio_type == "root_K"),
            input$design_dtl_bern_integer == TRUE)) {
      shiny::p(shiny::strong("WARNING:"), " Requiring integer sample size ",
               "with unequal allocation between the control arm and the ",
               "experimental arms can cause confusing results.")
    }
  })

  output$design_dtl_bern_warning <- renderUI({
    if (all(input$design_dtl_bern_K %in% c(4, 5),
            input$design_dtl_bern_plots)) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_dtl_bern_density <- renderUI({
    if (input$design_dtl_bern_plots) {
      shiny::selectInput(
        inputId  = "design_dtl_bern_density",
        label    = "Plot quality:",
        choices  = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                     "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_dtl_bern_reset, {
    shinyjs::reset("design_dtl_bern_parameters")
  })

  ##### Drop-the-losers (Bernoulli): Plot zoom set-up ##########################

  shiny::observeEvent(input$design_dtl_bern_equal_error_dblclick, {
    brush_error                             <-
      input$design_dtl_bern_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_dtl_bern_equal_error$x   <- c(brush_error$xmin,
                                                  brush_error$xmax)
      ranges_design_dtl_bern_equal_error$y   <- c(brush_error$ymin,
                                                  brush_error$ymax)
    } else {
      ranges_design_dtl_bern_equal_error$x   <-
        ranges_design_dtl_bern_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_dtl_bern_equal_power_dblclick, {
    brush_power                             <-
      input$design_dtl_bern_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_dtl_bern_equal_power$x   <- c(brush_power$xmin,
                                                  brush_power$xmax)
      ranges_design_dtl_bern_equal_power$y   <- c(brush_power$ymin,
                                                  brush_power$ymax)
    } else {
      ranges_design_dtl_bern_equal_power$x   <-
        ranges_design_dtl_bern_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_dtl_bern_equal_other_dblclick, {
    brush_other                             <-
      input$design_dtl_bern_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_dtl_bern_equal_other$x   <- c(brush_other$xmin,
                                                  brush_other$xmax)
      ranges_design_dtl_bern_equal_other$y   <- c(brush_other$ymin,
                                                  brush_other$ymax)
    } else {
      ranges_design_dtl_bern_equal_other$x   <-
        ranges_design_dtl_bern_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_dtl_bern_shifted_power_dblclick, {
    brush_shifted_power                       <-
      input$design_dtl_bern_shifted_power_brush
    if (!is.null(brush_shifted_power)) {
      ranges_design_dtl_bern_shifted_power$x   <- c(brush_shifted_power$xmin,
                                                    brush_shifted_power$xmax)
      ranges_design_dtl_bern_shifted_power$y   <- c(brush_shifted_power$ymin,
                                                    brush_shifted_power$ymax)
    } else {
      ranges_design_dtl_bern_shifted_power$x   <-
        ranges_design_dtl_bern_shifted_power$y <- NULL
    }
  })

  ##### Drop-the-losers (Bernoulli): int_des_dtl_bern() ########################

  int_des_dtl_bern <- shiny::eventReactive(input$design_dtl_bern_update, {
    K                     <- input$design_dtl_bern_K
    J                     <- input$design_dtl_bern_J
    seq_K                 <- 1:K
    Kv                    <- c(K, numeric(J - 1))
    for (j in 2:J) {
      Kv[j]               <-
        input[[paste0("design_dtl_bern_Kv_", j)]]
    }
    pi0                   <- input$design_dtl_bern_pi0
    power                 <- input$design_dtl_bern_power
    swss                  <- input$design_dtl_bern_swss
    progress              <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs", value = 0)
    if (input$design_dtl_bern_ratio_type == "equal_all") {
      ratio               <- 1
    } else if (input$design_dtl_bern_ratio_type == "equal_exp") {
      ratio               <- input$design_dtl_bern_ratio1
    } else if (input$design_dtl_bern_ratio_type == "root_K") {
      ratio               <- 1/sqrt(K)
    }
    design                <-
      multiarm:::des_dtl_bern(Kv      = Kv,
                              type    = swss,
                              alpha   = input$design_dtl_bern_alpha,
                              beta    = 1 - input$design_dtl_bern_beta,
                              pi0     = input$design_dtl_bern_pi0,
                              delta1  = input$design_dtl_bern_delta1,
                              delta0  = input$design_dtl_bern_delta0,
                              ratio   = ratio,
                              power   = power,
                              integer = input$design_dtl_bern_integer)
    design$K               <- K
    progress$inc(amount  = 0.25 + as.numeric(!input$design_dtl_bern_plots),
                 message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_dtl_bern_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_dtl_bern_summary.html"),
      params        = list(alpha      = design$alpha,
                           beta       = design$beta,
                           delta0     = design$delta0,
                           delta1     = design$delta1,
                           integer    = design$integer,
                           J          = design$J,
                           K          = design$K,
                           Kv         = design$Kv,
                           maxN       = design$maxN,
                           n10        = design$n10,
                           n1         = design$n1,
                           opchar     = design$opchar,
                           pi0        = design$pi0,
                           plots      = input$design_dtl_bern_plots,
                           power      = power,
                           ratio_type = input$design_dtl_bern_ratio_type,
                           ratio_init = input$design_dtl_bern_ratio1,
                           ratio      = design$ratio,
                           swss       = swss)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_dtl_bern_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_dtl_bern_summary_modified.html")
    )
    design$data_og        <- design$opchar
    if (input$design_dtl_bern_plots) {
      density             <- as.numeric(input$design_dtl_bern_density)
      plots               <- plot(design, density = density, output = TRUE,
                                  print_plots = FALSE)
      design$equal_power  <- plots$plots$equal_power
      design$equal_error  <- plots$plots$equal_error
      design$equal_other  <- plots$plots$equal_other
      design$shifted_power      <- plots$plots$shifted_power
      progress$inc(amount = 0.25, message = "Rendering plots")
      opchar              <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(plots$opchar, 3),
                                                    .name_repair = "minimal")))
      design$data         <-
        data.frame(rbind(design$opchar, opchar),
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                                 paste0("Equal #", 1:density),
                                 paste0("Shifted #",
                                        1:(nrow(opchar) - density))))
    } else {
      design$data         <-
        data.frame(design$opchar$opchar,
                   row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      design$equal_error  <- design$equal_power <- design$equal_other <-
        design$shifted_power     <- design$delta       <- NULL
    }
    colnames(design$data) <-
      c(paste0("<i>&pi;</i><sub>", c(0, seq_K), "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
        paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
        "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
        "<i>Sensitivity</i>", "<i>Specificity</i>", "<i>ESS</i>", "<i>SDSS</i>",
        "<i>MSS</i>", "<i>max N</i>")
    progress$inc(amount  = 0.25 + as.numeric(!input$design_dtl_bern_plots),
                 message = "Outputting results")
    design
  })

  ##### Drop-the-losers (Bernoulli): Value boxes ###############################

  output$design_dtl_bern_n_box     <- shinydashboard::renderValueBox({
    input$design_dtl_bern_update
    shinydashboard::valueBox(
      value    = round(int_des_dtl_bern()$maxN, 1),
      subtitle = "Maximal possible sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_dtl_bern_fwer_box  <- shinydashboard::renderValueBox({
    input$design_dtl_bern_update
    if (int_des_dtl_bern()$opchar$FWERI1[1] <=
        shiny::isolate(input$design_dtl_bern_alpha) + 1e-4) {
      icon_choice <- "thumbs-up"
    } else {
      icon_choice <- "thumbs-down"
    }
    shinydashboard::valueBox(
      value    = round(int_des_dtl_bern()$opchar$FWERI1[1], 3),
      subtitle = "Maximum family-wise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_dtl_bern_power_box <- shinydashboard::renderValueBox({
    input$design_dtl_bern_update
    subtitle          <-
      c("disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(
            input$design_dtl_bern_power)]
    K                 <- isolate(input$design_dtl_bern_K)
    if (int_des_dtl_bern()$power == "disjunctive") {
      value_power_box <- int_des_dtl_bern()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(
          int_des_dtl_bern()$opchar[-(1:2), (K + 4):(2*K + 3)])))
    }
    if (value_power_box >= shiny::isolate(input$design_dtl_bern_beta) - 1e-3) {
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

  ##### Drop-the-losers (Bernoulli): Summary ###################################

  output$design_dtl_bern_summary <- shiny::renderUI({
    input$design_dtl_bern_update
    n1 <- int_des_dtl_bern()$n1
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_dtl_bern_summary_modified.html")
      )
    )
  })

  ##### Drop-the-losers (Bernoulli): Table #####################################

  output$design_dtl_bern_table_key   <- DT::renderDT({
    table_key                                      <-
      int_des_dtl_bern()$data[, c(1:(int_des_dtl_bern()$K + 1),
                                  (int_des_dtl_bern()$K + 3):
                                    (2*int_des_dtl_bern()$K + 3),
                                  4*int_des_dtl_bern()$K + 3)]
    colnames(table_key)[2*int_des_dtl_bern()$K + 2] <- "<i>FWER</i>"
    DT::datatable(
      round(table_key, 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_dtl_bern_table_error <- DT::renderDT({
    DT::datatable(
      round(int_des_dtl_bern()$data[, c(1:int_des_dtl_bern()$K,
                                        (2*int_des_dtl_bern()$K + 3):
                                          (4*int_des_dtl_bern()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_dtl_bern_table_other <- DT::renderDT({
    DT::datatable(
      round(int_des_dtl_bern()$data[, -((2*int_des_dtl_bern()$K + 3):
                                          (4*int_des_dtl_bern()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  ##### Drop-the-losers (Bernoulli): Plots #####################################

  output$design_dtl_bern_equal_error <- shiny::renderPlot({
    input$design_dtl_bern_update
    if (shiny::isolate(input$design_dtl_bern_plots)) {
      int_des_dtl_bern()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_bern_equal_error$x,
                                 ylim   = ranges_design_dtl_bern_equal_error$y,
                                 expand = TRUE)
    }
  })

  output$design_dtl_bern_equal_power <- shiny::renderPlot({
    input$design_dtl_bern_update
    if (shiny::isolate(input$design_dtl_bern_plots)) {
      int_des_dtl_bern()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_bern_equal_power$x,
                                 ylim   = ranges_design_dtl_bern_equal_power$y,
                                 expand = TRUE)
    }
  })

  output$design_dtl_bern_equal_other <- shiny::renderPlot({
    input$design_dtl_bern_update
    if (shiny::isolate(input$design_dtl_bern_plots)) {
      int_des_dtl_bern()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_bern_equal_other$x,
                                 ylim   = ranges_design_dtl_bern_equal_other$y,
                                 expand = TRUE)
    }
  })

  output$design_dtl_bern_shifted_power     <- shiny::renderPlot({
    input$design_dtl_bern_update
    if (shiny::isolate(input$design_dtl_bern_plots)) {
      int_des_dtl_bern()$shifted_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_bern_shifted_power$x,
                                 ylim   = ranges_design_dtl_bern_shifted_power$y,
                                 expand = TRUE)
    }
  })

  ##### Drop-the-losers (Bernoulli): Report ####################################

  output$design_dtl_bern_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_dtl_bern_filename, sep = '.',
            switch(input$design_dtl_bern_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_dtl_bern_report.Rmd")
      file.copy("design_dtl_bern_report.Rmd", tempReport, overwrite = T)
      params     <- list(alpha        = int_des_dtl_bern()$alpha,
                         beta         = int_des_dtl_bern()$beta,
                         delta0       = int_des_dtl_bern()$delta0,
                         delta1       = int_des_dtl_bern()$delta1,
                         integer      = int_des_dtl_bern()$integer,
                         J            = int_des_dtl_bern()$J,
                         K            = int_des_dtl_bern()$K,
                         Kv           = int_des_dtl_bern()$Kv,
                         maxN         = int_des_dtl_bern()$maxN,
                         n10          = int_des_dtl_bern()$n10,
                         n1           = int_des_dtl_bern()$n1,
                         opchar       = int_des_dtl_bern()$opchar,
                         pi0          = int_des_dtl_bern()$pi0,
                         plots        = input$design_gs_bern_plots,
                         power        = int_des_dtl_bern()$power,
                         ratio_type   = input$design_gs_bern_ratio_type,
                         ratio_init   = input$design_gs_bern_ratio1,
                         ratio        = int_des_dtl_bern()$ratio,
                         swss         = input$design_gs_bern_swss,
                         equal_error  = int_des_dtl_bern()$equal_error,
                         equal_power  = int_des_dtl_bern()$equal_power,
                         equal_other  = int_des_dtl_bern()$equal_other,
                         shifted_power      = int_des_dtl_bern()$shifted_power)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$design_dtl_bern_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Drop-the-losers (normal): shinyFeedback warning messages ###############

  shiny::observeEvent(input$design_dtl_norm_alpha, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_norm_alpha",
      show    = any(input$design_dtl_norm_alpha <= 0,
                    input$design_dtl_norm_alpha >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_dtl_norm_beta, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_norm_beta",
      show    = any(input$design_dtl_norm_beta <= 0,
                    input$design_dtl_norm_beta >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_dtl_norm_delta1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_norm_delta1",
      show    = (input$design_dtl_norm_delta1 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_dtl_norm_delta0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_norm_delta0",
      show    = (input$design_dtl_norm_delta0 >= input$design_dtl_norm_delta1),
      text    =
        "Must be strictly smaller than the interesting treatment effect")
  })

  shiny::observeEvent(input$design_dtl_norm_sigma, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_norm_sigma",
      show    = (input$design_dtl_norm_sigma <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_dtl_norm_sigma_0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_norm_sigma_0",
      show    = (input$design_dtl_norm_sigma_0 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_dtl_norm_sigma_1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_norm_sigma_1",
      show    = (input$design_dtl_norm_sigma_1 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_dtl_norm_ratio, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_norm_ratio",
      show    = (input$design_dtl_norm_ratio <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_dtl_norm_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "design_dtl_norm_filename",
      show    = any(strsplit(input$design_dtl_norm_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Drop-the-losers (normal): Dynamic UI elements ##########################

  output$design_dtl_norm_J <- renderUI({
    shiny::sliderInput(
      inputId = "design_dtl_norm_J",
      label   = "Number of stages:",
      min     = 2,
      max     = min(input$design_dtl_norm_K, 4),
      value   = 2,
      step    = 1
    ) %>%
      shinyhelper::helper(
        type    = "markdown",
        title   = "",
        content = "design_J",
        size    = "m",
        colour  = "black"
      )
  })

  output$design_dtl_norm_Kv_2     <- renderUI({
    shiny::sliderInput(
      inputId = "design_dtl_norm_Kv_2",
      label   = "Number of experimental arms in stage 2:",
      value   = input$design_dtl_norm_K - 1,
      min     = input$design_dtl_norm_J - 1,
      max     = input$design_dtl_norm_K - 1,
      step    = 1
    )
  })

  output$design_dtl_norm_Kv_3     <- renderUI({
    if (input$design_dtl_norm_J >= 3) {
      shiny::sliderInput(
        inputId = "design_dtl_norm_Kv_3",
        label   = "Number of experimental arms in stage 3:",
        value   = input$design_dtl_norm_Kv_2 - 1,
        min     = input$design_dtl_norm_J - 2,
        max     = input$design_dtl_norm_Kv_2 - 1,
        step    = 1
      )
    }
  })

  output$design_dtl_norm_Kv_4     <- renderUI({
    if (input$design_dtl_norm_J >= 4) {
      shiny::sliderInput(
        inputId = "design_dtl_norm_Kv_4",
        label   = "Number of experimental arms in stage 4:",
        value   = input$design_dtl_norm_Kv_3 - 1,
        min     = 1,
        max     = input$design_dtl_norm_Kv_3 - 1,
        step    = 1
      )
    }
  })

  output$design_dtl_norm_delta0  <- renderUI({
    shiny::numericInput(
      inputId = "design_dtl_norm_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = NA,
      max     = input$design_dtl_norm_delta1,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black"
      )
  })

  output$design_dtl_norm_sigma   <- renderUI({
    if (input$design_dtl_norm_sigma_type == "equal_all") {
      shiny::numericInput(
        inputId = "design_dtl_norm_sigma",
        label   = paste0("Standard deviation of the responses (arms 0, ..., ",
                         input$design_dtl_norm_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.1
      )
    } else {
      shiny::tagList(
        shiny::numericInput(
          inputId = "design_dtl_norm_sigma_0",
          label   = "Standard deviation of the control arm responses (arm 0):",
          value   = 1,
          min     = 0,
          max     = NA,
          step    = 0.1
        ),
        shiny::numericInput(
          inputId = "design_dtl_norm_sigma_1",
          label   =
            paste0("Standard deviation of the experimental arm responses ",
                   "(arms 1, ..., ", input$design_dtl_norm_K, ")"),
          value   = 1,
          min     = 0,
          max     = NA,
          step    = 0.1
        )
      )
    }
  })

  output$design_dtl_norm_ratio   <- renderUI({
    if (input$design_dtl_norm_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_dtl_norm_ratio1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_dtl_norm_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    }
  })

  output$design_dtl_bern_warning_ratio <- renderUI({
    if (all(any(all(input$design_dtl_bern_ratio_type == "equal_exp",
                    input$design_dtl_bern_ratio1 != 1),
                input$design_dtl_bern_ratio_type == "root_K"),
            input$design_dtl_bern_integer == TRUE)) {
      shiny::p(shiny::strong("WARNING:"), " Requiring integer sample size ",
               "with unequal allocation between the control arm and the ",
               "experimental arms can cause confusing results.")
    }
  })

  output$design_dtl_norm_warning <- renderUI({
    if (any(input$design_dtl_norm_K %in% c(4, 5),
            all(input$design_dtl_norm_K == 3, input$design_dtl_norm_plots))) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_dtl_norm_density <- renderUI({
    if (input$design_dtl_norm_plots) {
      shiny::selectInput(
        inputId  = "design_dtl_norm_density",
        label    = "Plot quality:",
        choices  = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                     "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_dtl_norm_reset, {
    shinyjs::reset("design_dtl_norm_parameters")
  })

  ##### Drop-the-losers (normal): Plot zoom set-up #############################

  shiny::observeEvent(input$design_dtl_norm_equal_error_dblclick, {
    brush_error                             <-
      input$design_dtl_norm_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_dtl_norm_equal_error$x   <- c(brush_error$xmin,
                                                  brush_error$xmax)
      ranges_design_dtl_norm_equal_error$y   <- c(brush_error$ymin,
                                                  brush_error$ymax)
    } else {
      ranges_design_dtl_norm_equal_error$x   <-
        ranges_design_dtl_norm_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_dtl_norm_equal_power_dblclick, {
    brush_power                             <-
      input$design_dtl_norm_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_dtl_norm_equal_power$x   <- c(brush_power$xmin,
                                                  brush_power$xmax)
      ranges_design_dtl_norm_equal_power$y   <- c(brush_power$ymin,
                                                  brush_power$ymax)
    } else {
      ranges_design_dtl_norm_equal_power$x   <-
        ranges_design_dtl_norm_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_dtl_norm_equal_other_dblclick, {
    brush_other                             <-
      input$design_dtl_norm_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_dtl_norm_equal_other$x   <- c(brush_other$xmin,
                                                  brush_other$xmax)
      ranges_design_dtl_norm_equal_other$y   <- c(brush_other$ymin,
                                                  brush_other$ymax)
    } else {
      ranges_design_dtl_norm_equal_other$x   <-
        ranges_design_dtl_norm_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_dtl_norm_shifted_power_dblclick, {
    brush_shifted_power                       <-
      input$design_dtl_norm_shifted_power_brush
    if (!is.null(brush_shifted_power)) {
      ranges_design_dtl_norm_shifted_power$x   <- c(brush_shifted_power$xmin,
                                                    brush_shifted_power$xmax)
      ranges_design_dtl_norm_shifted_power$y   <- c(brush_shifted_power$ymin,
                                                    brush_shifted_power$ymax)
    } else {
      ranges_design_dtl_norm_shifted_power$x   <-
        ranges_design_dtl_norm_shifted_power$y <- NULL
    }
  })

  ##### Drop-the-losers (normal): int_des_dtl_norm() ############################

  int_des_dtl_norm <- shiny::eventReactive(input$design_dtl_norm_update, {
    K                     <- input$design_dtl_norm_K
    J                     <- input$design_dtl_norm_J
    seq_K                 <- 1:K
    Kv                    <- c(K, numeric(J - 1))
    for (j in 2:J) {
      Kv[j]               <-
        input[[paste0("design_dtl_norm_Kv_", j)]]
    }
    pi0                   <- input$design_dtl_norm_pi0
    power                 <- input$design_dtl_norm_power
    swss                  <- input$design_dtl_norm_swss
    progress              <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs", value = 0)
    if (input$design_dtl_norm_sigma_type == "equal_all") {
      sigma               <- input$design_dtl_norm_sigma
    } else if (input$design_dtl_norm_sigma_type == "equal_exp") {
      sigma               <- c(input$design_dtl_norm_sigma_0,
                               input$design_dtl_norm_sigma_1)
    }
    if (input$design_dtl_norm_ratio_type == "equal_all") {
      ratio               <- 1
    } else if (input$design_dtl_norm_ratio_type == "equal_exp") {
      ratio               <- input$design_dtl_norm_ratio1
    } else if (input$design_dtl_norm_ratio_type == "root_K") {
      ratio               <- 1/sqrt(K)
    }
    design                <-
      multiarm:::des_dtl_norm(Kv       = Kv,
                              type     = swss,
                              alpha    = input$design_dtl_norm_alpha,
                              beta     = 1 - input$design_dtl_norm_beta,
                              delta1   = input$design_dtl_norm_delta1,
                              delta0   = input$design_dtl_norm_delta0,
                              sigma    = sigma,
                              ratio    = ratio,
                              power    = power,
                              integer  = input$design_dtl_norm_integer)
    design$K              <- K
    progress$inc(amount  = 0.25 + as.numeric(!input$design_dtl_norm_plots),
                 message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_dtl_norm_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_dtl_norm_summary.html"),
      params        = list(alpha      = design$alpha,
                           beta       = design$beta,
                           delta0     = design$delta0,
                           delta1     = design$delta1,
                           integer    = design$integer,
                           J          = design$J,
                           K          = design$K,
                           Kv         = design$Kv,
                           maxN       = design$maxN,
                           n10        = design$n10,
                           n1         = design$n1,
                           opchar     = design$opchar,
                           plots      = input$design_dtl_norm_plots,
                           power      = power,
                           ratio_type = input$design_dtl_norm_ratio_type,
                           ratio_init = input$design_dtl_norm_ratio1,
                           ratio      = design$ratio,
                           sigma      = design$sigma,
                           swss       = swss)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_dtl_norm_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_dtl_norm_summary_modified.html")
    )
    design$data_og        <- design$opchar
    if (input$design_dtl_norm_plots) {
      density             <- as.numeric(input$design_dtl_norm_density)
      plots               <- plot(design, density = density, output = TRUE,
                                  print_plots = FALSE)
      design$equal_power  <- plots$plots$equal_power
      design$equal_error  <- plots$plots$equal_error
      design$equal_other  <- plots$plots$equal_other
      design$shifted_power <- plots$plots$shifted_power
      progress$inc(amount = 0.25, message = "Rendering plots")
      opchar              <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(plots$opchar, 3),
                                                    .name_repair = "minimal")))
      design$data         <-
        data.frame(rbind(design$opchar, opchar),
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                                 paste0("Equal #", 1:density),
                                 paste0("Shifted #", 1:density)))
    } else {
      design$data         <-
        data.frame(design$opchar$opchar,
                   row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      design$equal_error  <- design$equal_power <- design$equal_other <-
        design$shifted_power     <- design$delta       <- NULL
    }
    colnames(design$data) <-
      c(paste0("<i>&tau;</i><sub>", seq_K, "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
        paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
        "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
        "<i>Sensitivity</i>", "<i>Specificity</i>", "<i>ESS</i>", "<i>SDSS</i>",
        "<i>MSS</i>", "<i>max N</i>")
    progress$inc(amount  = 0.25 + as.numeric(!input$design_dtl_norm_plots),
                 message = "Outputting results")
    design
  })

  ##### Drop-the-losers (normal): Value boxes ##################################

  output$design_dtl_norm_n_box     <- shinydashboard::renderValueBox({
    input$design_dtl_norm_update
    shinydashboard::valueBox(
      value    = round(int_des_dtl_norm()$maxN, 1),
      subtitle = "Maximal possible sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_dtl_norm_fwer_box  <- shinydashboard::renderValueBox({
    input$design_dtl_norm_update
    if (int_des_dtl_norm()$opchar$FWERI1[1] <=
        shiny::isolate(input$design_dtl_norm_alpha) + 1e-4) {
      icon_choice <- "thumbs-up"
    } else {
      icon_choice <- "thumbs-down"
    }
    shinydashboard::valueBox(
      value    = round(int_des_dtl_norm()$opchar$FWERI1[1], 3),
      subtitle = "Maximum family-wise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_dtl_norm_power_box <- shinydashboard::renderValueBox({
    input$design_dtl_norm_update
    subtitle          <-
      c("conjunctive" = "Conjunctive power",
        "disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(
            input$design_dtl_norm_power)]
    K                 <- isolate(input$design_dtl_norm_K)
    if (int_des_dtl_norm()$power == "disjunctive") {
      value_power_box <- int_des_dtl_norm()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(int_des_dtl_norm()$opchar[-(1:2),
                                                     (K + 3):(2*K + 2)])))
    }
    if (value_power_box >= shiny::isolate(input$design_dtl_norm_beta) - 1e-3) {
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

  ##### Drop-the-losers (normal): Summary ######################################

  output$design_dtl_norm_summary <- shiny::renderUI({
    input$design_dtl_norm_update
    n1 <- int_des_dtl_norm()$n1
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_dtl_norm_summary_modified.html")
      )
    )
  })

  ##### Drop-the-losers (normal): Table ########################################

  output$design_dtl_norm_table_key   <- DT::renderDT({
    table_key                                      <-
      int_des_dtl_norm()$data[, c(1:(int_des_dtl_norm()$K + 1),
                                  (int_des_dtl_norm()$K + 3):
                                    (2*int_des_dtl_norm()$K + 3),
                                  4*int_des_dtl_norm()$K + 3)]
    colnames(table_key)[2*int_des_dtl_norm()$K + 2] <- "<i>FWER</i>"
    DT::datatable(
      round(table_key, 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_dtl_norm_table_error <- DT::renderDT({
    DT::datatable(
      round(int_des_dtl_norm()$data[, c(1:int_des_dtl_norm()$K,
                                        (2*int_des_dtl_norm()$K + 3):
                                          (4*int_des_dtl_norm()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_dtl_norm_table_other <- DT::renderDT({
    DT::datatable(
      round(int_des_dtl_norm()$data[, -((2*int_des_dtl_norm()$K + 3):
                                          (4*int_des_dtl_norm()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  ##### Drop-the-losers (normal): Plots ########################################

  output$design_dtl_norm_equal_error <- shiny::renderPlot({
    input$design_dtl_norm_update
    if (shiny::isolate(input$design_dtl_norm_plots)) {
      int_des_dtl_norm()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_norm_equal_error$x,
                                 ylim   = ranges_design_dtl_norm_equal_error$y,
                                 expand = TRUE)
    }
  })

  output$design_dtl_norm_equal_power <- shiny::renderPlot({
    input$design_dtl_norm_update
    if (shiny::isolate(input$design_dtl_norm_plots)) {
      int_des_dtl_norm()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_norm_equal_power$x,
                                 ylim   = ranges_design_dtl_norm_equal_power$y,
                                 expand = TRUE)
    }
  })

  output$design_dtl_norm_equal_other <- shiny::renderPlot({
    input$design_dtl_norm_update
    if (shiny::isolate(input$design_dtl_norm_plots)) {
      int_des_dtl_norm()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_norm_equal_other$x,
                                 ylim   = ranges_design_dtl_norm_equal_other$y,
                                 expand = TRUE)
    }
  })

  output$design_dtl_norm_shifted_power     <- shiny::renderPlot({
    input$design_dtl_norm_update
    if (shiny::isolate(input$design_dtl_norm_plots)) {
      int_des_dtl_norm()$shifted_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_norm_shifted_power$x,
                                 ylim   = ranges_design_dtl_norm_shifted_power$y,
                                 expand = TRUE)
    }
  })

  ##### Drop-the-losers (normal): Report #######################################

  output$design_dtl_norm_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_dtl_norm_filename, sep = '.',
            switch(input$design_dtl_norm_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_dtl_norm_report.Rmd")
      file.copy("design_dtl_norm_report.Rmd", tempReport, overwrite = T)
      params     <- list(alpha        = int_des_dtl_norm()$alpha,
                         beta         = int_des_dtl_norm()$beta,
                         delta0       = int_des_dtl_norm()$delta0,
                         delta1       = int_des_dtl_norm()$delta1,
                         integer      = int_des_dtl_norm()$integer,
                         J            = int_des_dtl_norm()$J,
                         K            = int_des_dtl_norm()$K,
                         Kv           = int_des_dtl_norm()$Kv,
                         maxN         = int_des_dtl_norm()$maxN,
                         n10          = int_des_dtl_norm()$n10,
                         n1           = int_des_dtl_norm()$n1,
                         opchar       = int_des_dtl_norm()$opchar,
                         plots        = input$design_gs_norm_plots,
                         power        = int_des_dtl_norm()$power,
                         ratio_type   = input$design_gs_norm_ratio_type,
                         ratio_init   = input$design_gs_norm_ratio1,
                         ratio        = int_des_dtl_norm()$ratio,
                         sigma        = int_des_dtl_norm()$sigma,
                         swss         = input$design_gs_norm_swss,
                         equal_error  = int_des_dtl_norm()$equal_error,
                         equal_power  = int_des_dtl_norm()$equal_power,
                         equal_other  = int_des_dtl_norm()$equal_other,
                         shifted_power      = int_des_dtl_norm()$shifted_power)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$design_dtl_norm_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Drop-the-losers (Poisson): shinyFeedback warning messages ##############

  shiny::observeEvent(input$design_dtl_pois_alpha, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_pois_alpha",
      show    = any(input$design_dtl_pois_alpha <= 0,
                    input$design_dtl_pois_alpha >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_dtl_pois_beta, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_pois_beta",
      show    = any(input$design_dtl_pois_beta <= 0,
                    input$design_dtl_pois_beta >= 1),
      text    = "Must be strictly between 0 and 1")
  })

  shiny::observeEvent(input$design_dtl_pois_lambda0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_pois_lambda0",
      show    = (input$design_dtl_pois_lambda0 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_dtl_pois_delta1, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_pois_delta1",
      show    = (input$design_dtl_pois_delta1 <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_dtl_pois_delta0, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_pois_delta0",
      show    = any(input$design_dtl_pois_delta0 >=
                      input$design_dtl_pois_delta1,
                    input$design_dtl_pois_delta0 <=
                      -input$design_dtl_pois_lambda0),
      text    = paste0("Must be strictly between minus the control arm ",
                       "event rate and the interesting treatment effect"))
  })

  shiny::observeEvent(input$design_dtl_pois_ratio, {
    shinyFeedback::feedbackDanger(
      inputId = "design_dtl_pois_ratio",
      show    = (input$design_dtl_pois_ratio <= 0),
      text    = "Must be strictly positive")
  })

  shiny::observeEvent(input$design_dtl_pois_filename, {
    shinyFeedback::feedbackWarning(
      inputId = "design_dtl_pois_filename",
      show    = any(strsplit(input$design_dtl_pois_filename,
                             split = "")[[1]] %in%
                      c('/', '\\', '?', "%", "*", ":", "|", "<", ">")),
      text    = paste0('It is generally inadvisable to use the characters /',
                       ', \\, ?, %, *, :, |, ", <, and > in a filename'))
  })

  ##### Drop-the-losers (Poisson): Dynamic UI elements #########################

  output$design_dtl_pois_J <- renderUI({
    shiny::sliderInput(
      inputId = "design_dtl_pois_J",
      label   = "Number of stages:",
      min     = 2,
      max     = min(input$design_dtl_pois_K, 4),
      value   = 2,
      step    = 1
    ) %>%
      shinyhelper::helper(
        type    = "markdown",
        title   = "",
        content = "design_J",
        size    = "m",
        colour  = "black"
      )
  })

  output$design_dtl_pois_Kv_2     <- renderUI({
    shiny::sliderInput(
      inputId = "design_dtl_pois_Kv_2",
      label   = "Number of experimental arms in stage 2:",
      value   = input$design_dtl_pois_K - 1,
      min     = input$design_dtl_pois_J - 1,
      max     = input$design_dtl_pois_K - 1,
      step    = 1
    )
  })

  output$design_dtl_pois_Kv_3     <- renderUI({
    if (input$design_dtl_pois_J >= 3) {
      shiny::sliderInput(
        inputId = "design_dtl_pois_Kv_3",
        label   = "Number of experimental arms in stage 3:",
        value   = input$design_dtl_pois_Kv_2 - 1,
        min     = input$design_dtl_pois_J - 2,
        max     = input$design_dtl_pois_Kv_2 - 1,
        step    = 1
      )
    }
  })

  output$design_dtl_pois_Kv_4     <- renderUI({
    if (input$design_dtl_pois_J >= 4) {
      shiny::sliderInput(
        inputId = "design_dtl_pois_Kv_4",
        label   = "Number of experimental arms in stage 4:",
        value   = input$design_dtl_pois_Kv_3 - 1,
        min     = 1,
        max     = input$design_dtl_pois_Kv_3 - 1,
        step    = 1
      )
    }
  })

  output$design_dtl_pois_delta   <- renderUI({
    inputTagList <-
      shiny::tagList(
        (shiny::numericInput(
          inputId = "design_dtl_pois_delta1",
          label   = "Interesting treatment effect:",
          value   = 1,
          min     = 0,
          step    = 0.1
        ) %>%
          shinyhelper:: helper(
            type    = "markdown",
            title   = "",
            content = "design_delta1",
            size    = "m",
            colour  = "black"
          ))
      )
    newInput     <- (shiny::numericInput(
      inputId = "design_dtl_pois_delta0",
      label   = "Uninteresting treatment effect:",
      value   = 0,
      min     = -input$design_dtl_pois_lambda0,
      step    = 0.1
    ) %>%
      shinyhelper:: helper(
        type    = "markdown",
        title   = "",
        content = "design_delta0",
        size    = "m",
        colour  = "black"
      ))
    inputTagList <- tagAppendChild(inputTagList, newInput)
    inputTagList
  })

  output$design_dtl_pois_ratio   <- renderUI({
    if (input$design_dtl_pois_ratio_type == "equal_exp") {
      shiny::numericInput(
        inputId = "design_dtl_pois_ratio1",
        label   = paste0("Allocation ratio for the experimental arms ",
                         "(arms 1, ..., ", input$design_dtl_pois_K, ")"),
        value   = 1,
        min     = 0,
        max     = NA,
        step    = 0.25
      )
    }
  })

  output$design_dtl_pois_warning_ratio <- renderUI({
    if (all(any(all(input$design_dtl_pois_ratio_type == "equal_exp",
                    input$design_dtl_pois_ratio1 != 1),
                input$design_dtl_pois_ratio_type == "root_K"),
            input$design_dtl_pois_integer == TRUE)) {
      shiny::p(shiny::strong("WARNING:"), " Requiring integer sample size ",
               "with unequal allocation between the control arm and the ",
               "experimental arms can cause confusing results.")
    }
  })

  output$design_dtl_pois_warning <- renderUI({
    if (all(input$design_dtl_pois_K %in% c(4, 5),
            input$design_dtl_pois_plots)) {
      shiny::p(shiny::strong("WARNING:"), " Execution time may be long for ",
               "chosen input parameters.")
    }
  })

  output$design_dtl_pois_density <- renderUI({
    if (input$design_dtl_pois_plots) {
      shiny::selectInput(
        inputId  = "design_dtl_pois_density",
        label    = "Plot quality:",
        choices  = c("Very low" = 33, "Low" = 66, "Medium" = 100, "High" = 150,
                     "Very high" = 200),
        selected = 100
      ) %>%
        shinyhelper::helper(
          type    = "markdown",
          title   = "",
          content = "design_density",
          size    = "m",
          colour  = "black"
        )
    }
  })

  shiny::observeEvent(input$design_dtl_pois_reset, {
    shinyjs::reset("design_dtl_pois_parameters")
  })

  ##### Drop-the-losers (Poisson): Plot zoom set-up ##########################

  shiny::observeEvent(input$design_dtl_pois_equal_error_dblclick, {
    brush_error                             <-
      input$design_dtl_pois_equal_error_brush
    if (!is.null(brush_error)) {
      ranges_design_dtl_pois_equal_error$x   <- c(brush_error$xmin,
                                                  brush_error$xmax)
      ranges_design_dtl_pois_equal_error$y   <- c(brush_error$ymin,
                                                  brush_error$ymax)
    } else {
      ranges_design_dtl_pois_equal_error$x   <-
        ranges_design_dtl_pois_equal_error$y <- NULL
    }
  })

  shiny::observeEvent(input$design_dtl_pois_equal_power_dblclick, {
    brush_power                             <-
      input$design_dtl_pois_equal_power_brush
    if (!is.null(brush_power)) {
      ranges_design_dtl_pois_equal_power$x   <- c(brush_power$xmin,
                                                  brush_power$xmax)
      ranges_design_dtl_pois_equal_power$y   <- c(brush_power$ymin,
                                                  brush_power$ymax)
    } else {
      ranges_design_dtl_pois_equal_power$x   <-
        ranges_design_dtl_pois_equal_power$y <- NULL
    }
  })

  shiny::observeEvent(input$design_dtl_pois_equal_other_dblclick, {
    brush_other                             <-
      input$design_dtl_pois_equal_other_brush
    if (!is.null(brush_other)) {
      ranges_design_dtl_pois_equal_other$x   <- c(brush_other$xmin,
                                                  brush_other$xmax)
      ranges_design_dtl_pois_equal_other$y   <- c(brush_other$ymin,
                                                  brush_other$ymax)
    } else {
      ranges_design_dtl_pois_equal_other$x   <-
        ranges_design_dtl_pois_equal_other$y <- NULL
    }
  })

  shiny::observeEvent(input$design_dtl_pois_shifted_power_dblclick, {
    brush_shifted_power                       <-
      input$design_dtl_pois_shifted_power_brush
    if (!is.null(brush_shifted_power)) {
      ranges_design_dtl_pois_shifted_power$x   <- c(brush_shifted_power$xmin,
                                                    brush_shifted_power$xmax)
      ranges_design_dtl_pois_shifted_power$y   <- c(brush_shifted_power$ymin,
                                                    brush_shifted_power$ymax)
    } else {
      ranges_design_dtl_pois_shifted_power$x   <-
        ranges_design_dtl_pois_shifted_power$y <- NULL
    }
  })

  ##### Drop-the-losers (Poisson): int_des_dtl_pois() ##########################

  int_des_dtl_pois <- shiny::eventReactive(input$design_dtl_pois_update, {
    K                     <- input$design_dtl_pois_K
    J                     <- input$design_dtl_pois_J
    seq_K                 <- 1:K
    Kv                    <- c(K, numeric(J - 1))
    for (j in 2:J) {
      Kv[j]               <-
        input[[paste0("design_dtl_pois_Kv_", j)]]
    }
    lambda0               <- input$design_dtl_pois_lambda0
    power                 <- input$design_dtl_pois_power
    swss                  <- input$design_dtl_pois_swss
    progress              <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Building outputs", value = 0)
    if (input$design_dtl_pois_ratio_type == "equal_all") {
      ratio               <- 1
    } else if (input$design_dtl_pois_ratio_type == "equal_exp") {
      ratio               <- input$design_dtl_pois_ratio1
    } else if (input$design_dtl_pois_ratio_type == "root_K") {
      ratio               <- 1/sqrt(K)
    }
    design                <-
      multiarm:::des_dtl_pois(Kv      = Kv,
                              type    = swss,
                              alpha   = input$design_dtl_pois_alpha,
                              beta    = 1 - input$design_dtl_pois_beta,
                              lambda0 = input$design_dtl_pois_lambda0,
                              delta1  = input$design_dtl_pois_delta1,
                              delta0  = input$design_dtl_pois_delta0,
                              ratio   = ratio,
                              power   = power,
                              integer = input$design_dtl_pois_integer)
    design$K               <- K
    progress$inc(amount  = 0.25 + as.numeric(!input$design_dtl_pois_plots),
                 message = "Rendering design summary")
    rmarkdown::render(
      input         = "design_dtl_pois_summary.Rmd",
      output_format = rmarkdown::html_document(),
      output_file   = file.path(tempdir(), "design_dtl_pois_summary.html"),
      params        = list(alpha      = design$alpha,
                           beta       = design$beta,
                           delta0     = design$delta0,
                           delta1     = design$delta1,
                           integer    = design$integer,
                           J          = design$J,
                           K          = design$K,
                           Kv         = design$Kv,
                           maxN       = design$maxN,
                           n10        = design$n10,
                           n1         = design$n1,
                           opchar     = design$opchar,
                           lambda0    = design$lambda0,
                           plots      = input$design_dtl_pois_plots,
                           power      = power,
                           ratio_type = input$design_dtl_pois_ratio_type,
                           ratio_init = input$design_dtl_pois_ratio1,
                           ratio      = design$ratio,
                           swss       = swss)
    )
    xml2::write_html(
      rvest::html_node(
        xml2::read_html(
          paste0(tempdir(), "/design_dtl_pois_summary.html")
        ),
        "body"
      ),
      file = paste0(tempdir(), "/design_dtl_pois_summary_modified.html")
    )
    design$data_og        <- design$opchar
    if (input$design_dtl_pois_plots) {
      density             <- as.numeric(input$design_dtl_pois_density)
      plots               <- plot(design, density = density, output = TRUE,
                                  print_plots = FALSE)
      design$equal_power  <- plots$plots$equal_power
      design$equal_error  <- plots$plots$equal_error
      design$equal_other  <- plots$plots$equal_other
      design$shifted_power      <- plots$plots$shifted_power
      progress$inc(amount = 0.25, message = "Rendering plots")
      opchar              <-
        as.matrix(dplyr::distinct(tibble::as_tibble(round(plots$opchar, 3),
                                                    .name_repair = "minimal")))
      design$data         <-
        data.frame(rbind(design$opchar, opchar),
                   row.names = c(paste0("<i>H<sub>", c("G", "A"), "</sub></i>"),
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>"),
                                 paste0("Equal #", 1:density),
                                 paste0("Shifted #",
                                        1:(nrow(opchar) - density))))
    } else {
      design$data         <-
        data.frame(design$opchar$opchar,
                   row.names = c("<i>H<sub>G</sub></i>", "<i>H<sub>A</sub></i>",
                                 paste0("<i>LFC<sub>", seq_K, "</sub></i>")))
      design$equal_error  <- design$equal_power <- design$equal_other <-
        design$shifted_power     <- design$delta       <- NULL
    }
    colnames(design$data) <-
      c(paste0("<i>&lambda;</i><sub>", c(0, seq_K), "</sub>"),
        paste0("<i>P</i><sub>", c("dis", "con", seq_K), "</sub>"),
        paste0("<i>FWER<sub>I</sub></i><sub>", seq_K, "</sub>"),
        paste0("<i>FWER<sub>II</sub></i><sub>", seq_K, "</sub>"),
        "<i>PHER</i>", "<i>FDR</i>", "<i>pFDR</i>", "<i>FNDR</i>",
        "<i>Sensitivity</i>", "<i>Specificity</i>", "<i>ESS</i>", "<i>SDSS</i>",
        "<i>MSS</i>", "<i>max N</i>")
    progress$inc(amount  = 0.25 + as.numeric(!input$design_dtl_pois_plots),
                 message = "Outputting results")
    design
  })

  ##### Drop-the-losers (Poisson): Value boxes #################################

  output$design_dtl_pois_n_box     <- shinydashboard::renderValueBox({
    input$design_dtl_pois_update
    shinydashboard::valueBox(
      value    = round(int_des_dtl_pois()$maxN, 1),
      subtitle = "Maximal possible sample size",
      icon     = shiny::icon(name = "users"),
      color    = "light-blue"
    )
  })

  output$design_dtl_pois_fwer_box  <- shinydashboard::renderValueBox({
    input$design_dtl_pois_update
    if (int_des_dtl_pois()$opchar$FWERI1[1] <=
        shiny::isolate(input$design_dtl_pois_alpha) + 1e-4) {
      icon_choice <- "thumbs-up"
    } else {
      icon_choice <- "thumbs-down"
    }
    shinydashboard::valueBox(
      value    = round(int_des_dtl_pois()$opchar$FWERI1[1], 3),
      subtitle = "Maximum family-wise error-rate",
      icon     = shiny::icon(name = icon_choice),
      color    = "light-blue"
    )
  })

  output$design_dtl_pois_power_box <- shinydashboard::renderValueBox({
    input$design_dtl_pois_update
    subtitle          <-
      c("disjunctive" = "Disjunctive power",
        "marginal"    =
          "Minimum marginal power")[shiny::isolate(
            input$design_dtl_pois_power)]
    K                 <- isolate(input$design_dtl_pois_K)
    if (int_des_dtl_pois()$power == "disjunctive") {
      value_power_box <- int_des_dtl_pois()$opchar$Pdis[2]
    } else {
      value_power_box <-
        min(diag(as.matrix(
          int_des_dtl_pois()$opchar[-(1:2), (K + 4):(2*K + 3)])))
    }
    if (value_power_box >= shiny::isolate(input$design_dtl_pois_beta) - 1e-3) {
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

  ##### Drop-the-losers (Poisson): Summary #####################################

  output$design_dtl_pois_summary <- shiny::renderUI({
    input$design_dtl_pois_update
    n1 <- int_des_dtl_pois()$n1
    shiny::withMathJax(
      shiny::includeHTML(
        path = file.path(tempdir(),
                         "/design_dtl_pois_summary_modified.html")
      )
    )
  })

  ##### Drop-the-losers (Poisson): Table #######################################

  output$design_dtl_pois_table_key   <- DT::renderDT({
    table_key                                      <-
      int_des_dtl_pois()$data[, c(1:(int_des_dtl_pois()$K + 1),
                                  (int_des_dtl_pois()$K + 3):
                                    (2*int_des_dtl_pois()$K + 3),
                                  4*int_des_dtl_pois()$K + 3)]
    colnames(table_key)[2*int_des_dtl_pois()$K + 2] <- "<i>FWER</i>"
    DT::datatable(
      round(table_key, 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_dtl_pois_table_error <- DT::renderDT({
    DT::datatable(
      round(int_des_dtl_pois()$data[, c(1:int_des_dtl_pois()$K,
                                        (2*int_des_dtl_pois()$K + 3):
                                          (4*int_des_dtl_pois()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  output$design_dtl_pois_table_other <- DT::renderDT({
    DT::datatable(
      round(int_des_dtl_pois()$data[, -((2*int_des_dtl_pois()$K + 3):
                                          (4*int_des_dtl_pois()$K + 3))], 3),
      escape        = FALSE,
      fillContainer = TRUE
    )
  })

  ##### Drop-the-losers (Poisson): Plots #######################################

  output$design_dtl_pois_equal_error <- shiny::renderPlot({
    input$design_dtl_pois_update
    if (shiny::isolate(input$design_dtl_pois_plots)) {
      int_des_dtl_pois()$equal_error +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_pois_equal_error$x,
                                 ylim   = ranges_design_dtl_pois_equal_error$y,
                                 expand = TRUE)
    }
  })

  output$design_dtl_pois_equal_power <- shiny::renderPlot({
    input$design_dtl_pois_update
    if (shiny::isolate(input$design_dtl_pois_plots)) {
      int_des_dtl_pois()$equal_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_pois_equal_power$x,
                                 ylim   = ranges_design_dtl_pois_equal_power$y,
                                 expand = TRUE)
    }
  })

  output$design_dtl_pois_equal_other <- shiny::renderPlot({
    input$design_dtl_pois_update
    if (shiny::isolate(input$design_dtl_pois_plots)) {
      int_des_dtl_pois()$equal_other +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_pois_equal_other$x,
                                 ylim   = ranges_design_dtl_pois_equal_other$y,
                                 expand = TRUE)
    }
  })

  output$design_dtl_pois_shifted_power     <- shiny::renderPlot({
    input$design_dtl_pois_update
    if (shiny::isolate(input$design_dtl_pois_plots)) {
      int_des_dtl_pois()$shifted_power +
        ggplot2::coord_cartesian(xlim   = ranges_design_dtl_pois_shifted_power$x,
                                 ylim   = ranges_design_dtl_pois_shifted_power$y,
                                 expand = TRUE)
    }
  })

  ##### Drop-the-losers (Poisson): Report ######################################

  output$design_dtl_pois_report <- shiny::downloadHandler(
    filename = function() {
      paste(input$design_dtl_pois_filename, sep = '.',
            switch(input$design_dtl_pois_format,
                   pdf  = "pdf",
                   html = "html",
                   word = "docx"
            )
      )
    },
    content  = function(file) {
      tempReport <- file.path(tempdir(), "design_dtl_pois_report.Rmd")
      file.copy("design_dtl_pois_report.Rmd", tempReport, overwrite = T)
      params     <- list(alpha        = int_des_dtl_pois()$alpha,
                         beta         = int_des_dtl_pois()$beta,
                         delta0       = int_des_dtl_pois()$delta0,
                         delta1       = int_des_dtl_pois()$delta1,
                         integer      = int_des_dtl_pois()$integer,
                         J            = int_des_dtl_pois()$J,
                         K            = int_des_dtl_pois()$K,
                         Kv           = int_des_dtl_pois()$Kv,
                         lambda0      = int_des_dtl_pois()$lambda0,
                         maxN         = int_des_dtl_pois()$maxN,
                         n10          = int_des_dtl_pois()$n10,
                         n1           = int_des_dtl_pois()$n1,
                         opchar       = int_des_dtl_pois()$opchar,
                         plots        = input$design_gs_pois_plots,
                         power        = int_des_dtl_pois()$power,
                         ratio_type   = input$design_gs_pois_ratio_type,
                         ratio_init   = input$design_gs_pois_ratio1,
                         ratio        = int_des_dtl_pois()$ratio,
                         swss         = input$design_gs_pois_swss,
                         equal_error  = int_des_dtl_pois()$equal_error,
                         equal_power  = int_des_dtl_pois()$equal_power,
                         equal_other  = int_des_dtl_pois()$equal_other,
                         shifted_power      = int_des_dtl_pois()$shifted_power)
      rmarkdown::render(tempReport,
                        output_format = paste0(input$design_dtl_pois_format,
                                               "_document"),
                        output_file   = file,
                        params        = params,
                        envir         = new.env(parent = globalenv())
      )
    }
  )

  ##### Session Info ###########################################################

  output$design_ss_bern_debug  <- shiny::renderPrint({
    utils::sessionInfo()
  })

  output$design_ss_norm_debug  <- shiny::renderPrint({
    utils::sessionInfo()
  })

  output$design_ss_pois_debug  <- shiny::renderPrint({
    utils::sessionInfo()
  })

  output$design_gs_bern_debug  <- shiny::renderPrint({
    utils::sessionInfo()
  })

  output$design_gs_norm_debug  <- shiny::renderPrint({
    utils::sessionInfo()
  })

  output$design_gs_pois_debug  <- shiny::renderPrint({
    utils::sessionInfo()
  })

  output$design_dtl_bern_debug <- shiny::renderPrint({
    utils::sessionInfo()
  })

  output$design_dtl_norm_debug <- shiny::renderPrint({
    utils::sessionInfo()
  })

  output$design_dtl_pois_debug <- shiny::renderPrint({
    utils::sessionInfo()
  })

  ##### Close set-up ###########################################################

  shiny::observe({
    shiny::reactiveValuesToList(input)
    session$doBookmark()
  })
  shiny::onBookmarked(updateQueryString)

  sever::sever(
    html = tagList(
      h1("Whoops...you have been disconnected"),
      p("There are several reasons this could happen. You can try reconnecting",
        "by clicking the button below. If this problem persists, please ",
        "email:"),
      HTML('<a href="mailto:michael.grayling@newcastle.ac.uk">michael.grayling@newcastle.ac.uk</a>'),
      p(),
      sever::reload_button("Reconnect", "default")
    ),
    bg_color = "rgba(0,0,0,.5)",
    box      = TRUE,
    color    = "black")

  session$onSessionEnded(stopApp)

}

shiny::shinyApp(ui, server, enableBookmarking = "url")
