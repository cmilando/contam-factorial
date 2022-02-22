# =============================================================================
# Author: CWM
# Purpose: Shiny version of the factorial tool, using JSON
# Notes:
# - remember that browser() allows you to debug
# =============================================================================
library(shiny)
library(shinyvalidate)
library(shinyjs)
library(shinythemes)
library(jsonlite)
library(waiter)

source("custom_functions.R")
source("section_parser.R")
source("methods_filters.R")
source("methods_flowElements.R")
source("methods_species.R")
source("methods_SourcesSinks.R")

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

  # title
  h2("CONTAM factorial tool, using JSON", align = "center"),
  helpText(
    "This facilitates single object replacements.",
    "Windows only.", "See code and objects at",
    a("Github", href = "https://github.com/cmilando/contam-factorial")
  ),
  br(),

  # file upload and output
  textInput("out_dir",
    "File path for outputs (e.g., C:\\tmp\\contam)",
    width = "100%",
    value = "C:\\contam_test\\db_test\\"
  ),
  textInput(
    "out_prefix",
    "Output file prefix",
    value = "dt1"
  ),
  fileInput("prj", "Choose PRJ to convert to JSON",
    accept = c(".prj"), width = "100%"
  ),
  hr(),


  # Individual object replace
  tabsetPanel(
    type = "tabs",

    # -------------------------
    # FILTERS
    filter_tabPanel,

    # -------------------------
    # FLOW ELEMENTS
    flow_elements_tabPanel,
    
    # -------------------------
    # SPECIES
    species_tabPanel,
    
    # -------------------------
    # SOURCES/SINKS
    sourcesSinks_tabPanel,
  
  ),
  hr(),

  # Create new PRJs
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
  iv$add_rule(
    "out_dir",
    sv_required(
      message = "Valid directory required",
      test = function(val) {
        val != "" & dir.exists(val)
      }
    )
  )
  iv$add_rule("out_prefix", sv_required())
  iv$enable()

  # disable file upload until you have a valid path
  observe({
    if (input$out_dir != "" & dir.exists(input$out_dir) &
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
  # >> Update this each time you add a new tabset
  observe({
    inFile <- input$prj

    if (is.null(inFile)) {
      return(NULL)
    }

    out_f <- file.path(input$out_dir, paste0(input$out_prefix, "_orig.JSON"))

    prj <- prj_to_json(inFile$datapath, out_f)

    ## add new sections here
    ## <<<<*****>>>>> (1/X)
    # species in the current file
    vec <- names(prj[[2]])
    names(vec) <- sapply(prj[[2]], function(x) x$name)
    updateSelectInput(session, "base_species", choices = vec)
    
    # filters in the current file
    vec <- names(prj[[8]])
    names(vec) <- sapply(prj[[8]], function(x) x$name)
    updateSelectInput(session, "base_filter", choices = vec)
    
    # flow elements in the current file
    vec <- names(prj[[11]])
    names(vec) <- sapply(prj[[11]], function(x) x$name)
    updateSelectInput(session, "base_flow_element", choices = vec)
    
    # sources/sinks in the current file
    # have to do this a little differently since you are always swapping both
    # and there is only 1 choice
    stopifnot(length(names(prj[[10]])) == 2)
    vec <- 1
    names(vec) <- paste(names(prj[[10]]), collapse = "-")
    updateSelectInput(session, "base_sourcessink", choices = vec)
    
  })

  # --------------------------------------
  # What happens when you want to create new PRJs
  observeEvent(input$create_prj, {

    # eventually this will get moved, but just for right now
    header <- c("ContamW 3.3  0", "")
    footer <- c("* end project file.")

    # you know its filters
    # obviously expand and change this, but not by much
    # <<<<*****>>>>> (2/X)
    if (is.null(input$filter_choices) & 
        is.null(input$flow_element_choices) &
        is.null(input$species_choices) &
        is.null(input$sourcesink_choices)) {
      return(NULL)
    }

    # read X_json
    # <<<<*****>>>>> (3/X)
    filters_json <- read_json("objs/filters.JSON", simplifyVector = T)
    flow_elements_json <- read_json("objs/flow_elements.JSON", simplifyVector = T)
    species_json <- read_json("objs/species.JSON", simplifyVector = T)
    source_sink_json <- read_json("objs/sources_sinks.JSON", simplifyVector = T)
    
    # get base_json
    out_f <- file.path(input$out_dir, paste0(input$out_prefix, "_orig.JSON"))
    base_json <- read_json(path = out_f, simplifyVector = T)

    # show the spinner
    w$show()
    
    # Need to combine all input choices
    # using expand grid on input$X_choices
    # check to make sure none have "_"
    # <<<<*****>>>>> (4/X)
    filters <- input$filter_choices
    flow_elements <- input$flow_element_choices
    species <- input$species_choices
    sourcessinks <- input$sourcesink_choices
    
    all_opts <- expand.grid(filters, 
                            flow_elements, 
                            species,
                            sourcessinks,
                            stringsAsFactors = F)
    
    names(all_opts) <- c('filters', 'flow_elements', 'species',
                         'sourcessinks')
    
    for(j in 1:ncol(all_opts)) {
      all_opts[, j] <- gsub("_", "", all_opts[, j])
    }
    
    # ---------------------
    # for each rep
    for (j in 1:nrow(all_opts)) {

      # start fresh each time
      base_json_x <- base_json

      # loop over each filter
      # again, you will do this differently moving forwards
      # but just for now
      # <<<<*****>>>>> (5/X)
      filter       <- all_opts$filters[j]
      flow_element <- all_opts$flow_elements[j]
      species_i    <- all_opts$species[j]
      sourcesink_i <- all_opts$sourcessinks[j]
      
      for (section_i in 1:length(base_json)) {
        
        # species
        if (section_i == 2) {
          
          base_json_x[[2]] <-
            write_species(
              section = base_json_x[[2]],
              obj_to_sub = input$base_species,
              new_obj_name = species_i,
              new_obj = species_json[[species_i]]
            )
        }
        
        # filter
        else if (section_i == 8) {

          base_json_x[[8]] <-
            write_filters(
              section = base_json_x[[8]],
              obj_to_sub = input$base_filter,
              new_obj_name = filter,
              new_obj = filters_json[[filter]]
            )
          
        } 
        
        # source-sink
        else if (section_i == 10) {
          
          base_json_x[[10]] <-
            write_sourcesSinks(
              section = base_json_x[[10]],
              obj_to_sub = input$base_sourcessink,
              new_obj_name = sourcesink_i,
              new_obj = source_sink_json[[sourcesink_i]]
            )
          
        } 
        
        # flow element
        else if(section_i == 11){
          
          base_json_x[[11]] <-
            write_flow_elements(
              section = base_json_x[[11]],
              obj_to_sub = input$base_flow_element,
              new_obj_name = flow_element,
              new_obj = flow_elements_json[[flow_element]]
            )
          
        } else {
          
          base_json_x[[section_i]] <- c(base_json_x[[section_i]], "-999")
          
        }
      }

      # compile and output to .prj
      # filename reflects the combinations
      prj_full <- c(
        header,
        do.call(c, base_json_x),
        footer
      )
      
      # make the prj name
      prj_f <- paste0(input$out_prefix, "_",
                      paste0(all_opts[j,], collapse = "_"), ".prj")
      out_f <- file.path(input$out_dir, prj_f)

      # write prj
      write.table(prj_full, out_f,
        quote = F, sep = "|",
        row.names = F, col.names = F
      )
    }
    
    showModal(modalDialog(
      title = "Success",
      "Files created!"
    ))

    # hide the spinner
    w$hide()
  })
}
# =============================================================================
shinyApp(ui, server)