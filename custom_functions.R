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