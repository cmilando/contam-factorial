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
           "Windows only.", "See code and objects at",
           a("Github", href = "https://github.com/cmilando/contam-factorial")),
  br(),

  # file upload and output
  textInput("out_dir",
            "File path for outputs (e.g., C:\\tmp\\contam)",
            width = "100%"),
  textInput("out_prefix",
            "Output file prefix"),

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
  iv$add_rule("out_dir", 
              sv_required(message = "Valid directory required",
                          test = function(val) {
                            val != "" & dir.exists(val)
                          }))
  iv$add_rule("out_prefix", sv_required())
  iv$enable()
  
  # disable file upload until you have a valid path
  observe({
    if(input$out_dir != "" & dir.exists(input$out_dir) &
       input$out_prefix != "") {
      runjs('$("#prj").parents("span").removeClass("disabled")')
      enable("prj")
    } else {
      runjs('$("#prj").parents("span").addClass("disabled")')
      disable("prj")
    }
  })

  # --------------------------------------
  # make a new JSON if a file is uploaded
  observe({
  
    inFile <- input$prj

    if (is.null(inFile)) {
      return(NULL)
    }
    
    out_f <- file.path(input$out_dir, paste0(input$out_prefix, "_orig.JSON"))
    
    prj <- prj_to_json(inFile$datapath, out_f)

    # filters in the current file
    vec <- names(prj[[8]])
    names(vec) <- sapply(prj[[8]], function(x) x$name)
    updateSelectInput(session, "base_filter", choices = vec)
  })
  
  # --------------------------------------
  # What happens when you want to create new PRJs
  observeEvent(input$create_prj, {

    # eventually this will get moved, but just for right now
    header <- c("ContamW 3.3  0", "")
    footer <- c("* end project file.")

    # you know its filters
    # obviously expand and change this, but not by much
    if (is.null(input$filter_choices)) {
      return(NULL)
    }
    
    # read filter_json
    filter_json <- read_json("objs/filters.JSON", simplifyVector = T)

    # get base_json
    out_f <- file.path(input$out_dir, paste0(input$out_prefix, "_orig.JSON"))
    base_json <- read_json(path = out_f, simplifyVector = T)
    
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
          base_json_x[[8]] <- 
            write_filters(section = base_json_x[[8]],
                          obj_to_sub = input$base_filter,
                          new_obj_name = filter,
                          new_obj = filter_json[[filter]])
        }
      }
      
      # compile and output to .prj
      # filename reflects the combinations
      prj_full <- c(
        header,
        do.call(c, base_json_x),
        footer
      )
      
      # remove the "_" from the name to reserve
      prj_f <- paste0(input$out_prefix, "_", filter, ".prj")
      out_f <- file.path(input$out_dir, prj_f)
      
      # write prj
      write.table(prj_full, out_f,
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