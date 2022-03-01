# =============================================================================
# some custom functions
unbox_atomic <- function(raw_list) {
  lapply(raw_list, function(x) {
    if (is.atomic(x) && length(x) == 1 && is.vector(x)) {
      unbox(x)
    } else {
      x
    }
  })
}

# get choices from JSON files in the objs directory
get_json_choices <- function(file_path, file_type) {
  
  raw_json <- read_json(file_path, simplifyVector = T)
  
  if (file_type == "sources_sinks") {
    vec <- names(raw_json)
    return(vec)
  }
  
  if (file_type == "filters") {
    vec <- names(raw_json)
    return(vec)
  } 
  
  if (file_type == "flow_elements") {
    vec <- names(raw_json)
    return(vec)
  }
  
  if (file_type == "species") {
    vec <- names(raw_json)
    return(vec)
  } 
  
  if (file_type == "schedules") {
    vec <- names(raw_json)
    return(vec)
  } 
  
  stop()
}

################## A Collections of server utilities############################
# Can be used in other shiny projects, no need to use under SPS framework
## use on top of shiny


shinyCatch <- function(
  expr,
  position = "bottom-right",
  blocking_level = "none",
  shiny = TRUE,
  prefix = "SPS",
  trace_back = spsOption("traceback")
) {
  
  assert_that(is.logical(shiny))
  assert_that(all(is.character(prefix), length(prefix) == 1))
  prefix <- paste0(prefix, if (prefix == "") " " else "-")
  shiny <- all(!is.null(getDefaultReactiveDomain()), shiny)
  if(shiny) dependServer("toastr")
  toastr_actions <- list(
    message = function(m) {
      msg(m$message, paste0(prefix, "INFO"), "blue")
      if(shiny) shinytoastr::toastr_info(message = remove_ANSI(m$message),
                                         position = position, closeButton = TRUE,
                                         timeOut = 3000, preventDuplicates = TRUE)
    },
    warning = function(m) {
      msg(m$message, paste0(prefix, "WARNING"), "orange")
      if(shiny) shinytoastr::toastr_warning(
        message = remove_ANSI(m$message),
        position = position, closeButton = TRUE,
        timeOut = 5000, preventDuplicates = TRUE)
    },
    error = function(m) {
      msg(m$message, paste0(prefix, "ERROR"), "red")
      if(shiny) shinytoastr::toastr_error(
        message = remove_ANSI(m$message), position = position,
        closeButton = TRUE, timeOut = 0, preventDuplicates = TRUE,
        title = "There is an error", hideDuration = 300)
    }
  )
  
  switch(tolower(blocking_level),
         "error" = tryCatch(
           suppressMessages(suppressWarnings(withCallingHandlers(
             expr,
             message = function(m) toastr_actions$message(m),
             warning = function(m) toastr_actions$warning(m),
             error = function(m) if(trace_back) printTraceback(sys.calls())
           ))),
           error = function(m) {
             toastr_actions$error(m)
             reactiveStop(class = "validation")
           }),
         "warning" = tryCatch(
           suppressMessages(withCallingHandlers(
             expr,
             message = function(m) toastr_actions$message(m),
             error = function(m) if(trace_back) printTraceback(sys.calls())
           )),
           warning = function(m) {
             toastr_actions$warning(m)
             reactiveStop(class = "validation")
           },
           error = function(m) {
             if(!is.empty(m$message)) toastr_actions$error(m)
             reactiveStop(class = "validation")
           }),
         "message" = tryCatch(
           withCallingHandlers(
             expr,
             error = function(m) if(trace_back) printTraceback(sys.calls())
           ),
           message = function(m) {
             toastr_actions$message(m)
             reactiveStop(class = "validation")
           },
           warning = function(m) {
             toastr_actions$warning(m)
             reactiveStop(class = "validation")
           },
           error = function(m) {
             if(!is.empty(m$message)) toastr_actions$error(m)
             reactiveStop(class = "validation")
           }),
         tryCatch(
           suppressMessages(suppressWarnings(withCallingHandlers(
             expr,
             message = function(m) toastr_actions$message(m),
             warning = function(m) toastr_actions$warning(m),
             error = function(m) if(trace_back) printTraceback(sys.calls())
           ))),
           error = function(m) {
             toastr_actions$error(m)
             return(NULL)
           }
         )
  )
}




