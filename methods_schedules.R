# =============================================================================
## tabPanel
schedules_tabPanel <-     
  tabPanel(
    "Schedules",
    br(),
    # swap in a new element
    selectInput("base_schedule", "Base Schedule:", choices = c()),
    helpText("This element will be replaced in the replicants below."),
    tags$div(HTML("<b>Make replicants with the following schedules:</b>"),
             style = "margin-bottom: 5px;"
    ),
    tags$div(
      #class = "multicol",
      checkboxGroupInput("schedule_choices",
                         selected = 1:1e5, # a hack to select all
                         label = NULL,
                         get_json_choices("objs/schedules.JSON", "schedules")
      )
    ),
  )

# =============================================================================
## FROM PRJ TO JSON
parse_schedules <- function(prj, first_row) {
  
  # first character is # of elements
  n_sch <- as.integer(strsplit(prj[first_row], "!")[[1]][1])
  output_list <- vector("list", n_sch)
  offset <- 2

  # then repeat this loop for each element
  for (i in 1:n_sch) {
    
    # first line
    line_split <- strsplit(prj[first_row + offset], " ")[[1]]
    line_split <- line_split[which(line_split != "")]
    x_nr      <- as.numeric(line_split[1])
    x_npts   <- as.numeric(line_split[2])
    x_shap  <- as.numeric(line_split[3])
    x_utyp   <- as.numeric(line_split[4])
    x_ucnv   <- as.numeric(line_split[5])
    x_name    <- line_split[6]
    offset <- offset + 1
    
    # desc line
    x_desc <- prj[first_row + offset]
    offset <- offset + 1
    
    # get vec
    dat_vec <- prj[(first_row + offset):(first_row + offset + x_npts - 1)]
    x_dat <- read.table(
      text = dat_vec, sep = "", strip.white = T,
      col.names = c("time", "coef")
    )
    
    # output as a list
    output_list[[i]] <- unbox_atomic(list(
      nr = x_nr,
      npts = x_npts,
      shap = x_shap,
      utyp = x_utyp,
      ucnv = x_ucnv,
      name = x_name,
      desc = x_desc,
      dat = x_dat
    ))
    
    # name the element with the filter name
    # check for unique
    names(output_list)[i] <- gsub("_", "", x_name)
    if(i > 1) 
      stopifnot(! (names(output_list)[i] %in% names(output_list)[1:(i-1)]))
    
    # advance the iterators
    offset <- offset + x_npts
  }
  
  return(output_list)
}

# =============================================================================
## TO PRJ FROM JSON
write_schedules <- function(section, obj_to_sub, new_obj_name, new_obj) {
  
  n_sch <- length(section)
  
  out_vec <- paste(n_sch, "! day-schedules:")
  out_vec <- c(out_vec, "! # npts shap utyp ucnv name")
  
  # swap in the new object
  # make sure to preserve the NR, because that is what is passed around
  # this will be different for each prj part
  # has to be a unique name
  i_to_sub <- which(names(section) == obj_to_sub)
  base_nr <- section[[i_to_sub]]$nr
  section[[i_to_sub]] <- NA
  names(section)[[i_to_sub]] <- NA
  stopifnot(! (new_obj_name %in% names(section)))
  
  section[[i_to_sub]] <- new_obj
  section[[i_to_sub]]$nr <- base_nr
  names(section)[[i_to_sub]] <- new_obj_name
  
  # print all
  for (i in 1:n_sch) {
    
    z <- section[[i]]
    
    out_vec <- c(out_vec, with(z, {
      paste(nr, npts, shap, utyp, ucnv, name)
    }))
    
    out_vec <- c(out_vec, with(z, {
      paste(desc)
    }))
    
    out_vec <- c(out_vec, with(z, {
      sapply(1:nrow(dat), function(i) {
        paste(" ", paste0(dat[i, ], collapse = " "))
      })
    }))
    
  }
  
  out_vec <- c(out_vec, "-999")
  
  out_vec
}
