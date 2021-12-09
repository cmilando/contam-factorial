# =============================================================================
# Author: CWM
# Purpose: Shiny version of the factorial tool, using JSON
# =============================================================================
library(shiny)
library(shinyvalidate)
library(shinyjs)
library(shinythemes)
library(jsonlite)
library(waiter)

source('custom_functions.R')
source('section_parser.R')
source("methods_filters.R")

# =============================================================================
ui <- fluidPage(

  # theme
  theme = shinytheme("flatly"),
  
  # multi-column checkbox html
  tags$head(
    tags$style(HTML("

     .multicol {
       -webkit-column-count: 3; /* Chrome, Safari, Opera */
       -moz-column-count: 3; /* Firefox */
       column-count: 3;
     }

   "))
  ),

  # waiter
  use_waiter(),
  
  # shiny js
  shinyjs::useShinyjs(),

  # max width
  style = "max-width: 700px;",
  h2("CONTAM factorial tool, using JSON", align = "center"),
  helpText("This facilitates single object replacements.",
           "More complicated replacements would involve passing object refs."),
  br(),

  # file upload and output
  textInput("out_f",
            "Base file path and name prefix for outputs (e.g., C:\\tmp\\contam)",
            width = "100%"),

  fileInput("prj", "Choose PRJ to convert to JSON",
          accept = c(".prj"), width = "100%"),

  hr(),
  
  # file picker
  tabsetPanel(type = 'tabs',
    tabPanel(
      "Filters",
      br(),
      # swap in a new element
      selectInput("base_filter", "Base filter element:", choices = c()),
      helpText("This element will be replaced in the replicants below."),
      tags$div(HTML("<b>Make replicants with the following filters:</b>"),
           style = "margin-bottom: 5px;"),
      tags$div(class = 'multicol',
        checkboxGroupInput("filter_choices",
          selected = 1:1e5, # a hack to select all
          label = NULL,
          get_json_choices("objs/filters.JSON", "filters")
        )
        ),
      helpText("Edit these in objs\\filters.JSON"),
    ),
    tabPanel("Flow elements")
  ),
  hr(),
  actionButton("create_prj", "Create new prjs",
               style = paste(
                 "color: #fff;",
                 "background-color: #337ab7;",
                 "border-color: #2e6da4"
               )
  ),
  # copyright
  hr(),
  tags$p(
    HTML("&copy; 2021 Chad W. Milando")
  )
)

# =============================================================================
server <- function(input, output, session) {
  
  # waiting button
  w <- Waiter$new(id = "create_prj")
  
  # validator
  iv <- InputValidator$new()
  iv$add_rule("out_f", sv_required())
  iv$enable()
  
  # disable file upload until you have a path
  observe({
    if(input$out_f == "") {
      runjs('$("#prj").parents("span").addClass("disabled")')
      disable("prj")
    } else {
      runjs('$("#prj").parents("span").removeClass("disabled")')
      enable("prj")
    }
  })

  # --------------------------------------
  # make a new JSON if a file is uploaded
  observe({
  
    inFile <- input$prj

    if (is.null(inFile)) {
      return(NULL)
    }
    
    prj <- prj_to_json(inFile$datapath, input$out_f)

    # filter
    vec <- names(prj[[8]])
    names(vec) <- sapply(prj[[8]], function(x) x$name)
    updateSelectInput(session, "base_filter",
                      choices = vec)
  })
  
  # --------------------------------------
  # What happens when you want to create new PRJs
  observeEvent(input$create_prj, {

    # eventually this will get moved, but just for right now
    header <- c("ContamW 3.3  0", "")
    footer <- c("* end project file.")
    json_f <- paste0(input$out_f, ".JSON")

    # you know its filters
    # obviously expand and change this, but not by much
    if (is.null(input$filter_choices)) {
      return(NULL)
    }
    
    filter_json <- read_json("objs/filters.JSON", simplifyVector = T)

    # get base_json
    base_json <- read_json(path = json_f, simplifyVector = T)
    
    # show the spinner
    w$show()
    # ---------------------
    # for each rep
    for (filter in input$filter_choices) {

      # start fresh each time
      base_json_x <- base_json

      # loop over each filter
      # again, you will do this differently moving forwards
      # but just for now
      for (section_i in 1:length(base_json)) {
        
        if (section_i != 8) {
          base_json_x[[section_i]] <- c(base_json_x[[section_i]], "-999")
        } else {
          #base_nr <- base_json[[8]]
          base_json_x[[8]] <- write_filters(filter_json[[filter]])
        }
      }
      
      # compile and output to .prj
      # filename reflects the combinations
      prj_full <- c(
        header,
        do.call(c, base_json_x),
        footer
      )

      prj_f <- paste0(input$out_f, "_", gsub("_", "", filter), ".prj")

      write.table(prj_full, prj_f,
        quote = F, sep = "|",
        row.names = F, col.names = F
      )
    }
    # hide the spinner
    w$hide()
  })
}
# =============================================================================
shinyApp(ui, server)