# =============================================================================
## tabPanel
sourcesSinks_tabPanel <-     
  tabPanel(
    "Sources and Sinks",
    br(),
    # swap in a new element
    selectInput("base_sourcessink", "Base Source+Sink:", choices = c()),
    helpText("These elements will be replaced in the replicants below."),
    tags$div(HTML("<b>Make replicants with the following pairs:</b>"),
             style = "margin-bottom: 5px;"
    ),
    tags$div(
      #class = "multicol",
      checkboxGroupInput("sourcesink_choices",
                         selected = 1:1e5, # a hack to select all
                         label = NULL,
                         get_json_choices("objs/sources_sinks.JSON", 
                                          "sources_sinks")
      )
    ),
  )

# =============================================================================
## FROM PRJ TO JSON
parse_sourcesSinks <- function(prj, first_row) {
  
  # 2 ! source/sink elements:
  #   1 PM2.5 drs PMsink
  # Average deposition of 0.3 - 2.5 um particles based on Emmerich 2005
  # 0.000111111 2
  # 2 PM2.5 ccf PMsource
  # Generic PM2.5 source (not based on any reference).
  # 6.13333e-08 0 17 0
  
  # first character is # of elements
  n_sourcessinks <- as.integer(strsplit(prj[first_row], "!")[[1]][1])
  stopifnot(n_sourcessinks == 2)

  output_list <- vector("list", 2)
  offset <- 1
  
  # assume first is source

  # then repeat this loop for each element
  for (i in 1:n_sourcessinks) {

    # first line
    line_split <- strsplit(prj[first_row + offset], " ")[[1]]
    line_split <- line_split[which(line_split != "")]
    x_nr      <- as.numeric(line_split[1])
    x_spcs    <- line_split[2]
    x_ctype   <- line_split[3]
    x_name    <- line_split[4]
    offset <- offset + 1
    
    # desc line
    x_desc <- prj[first_row + offset]
    offset <- offset + 1
    
    # next line is dat
    if (x_ctype == "drs") {
      
      line_split <- strsplit(trimws(prj[first_row + offset]), " ")[[1]]
      x_kd <- as.numeric(line_split[1])
      x_u_k <- as.numeric(line_split[2])

      # output as a list
      output_list[[i]] <- unbox_atomic(list(
        nr = x_nr,
        spcs = x_spcs,
        ctype = x_ctype,
        name = x_name,
        desc = x_desc,
        kd = x_kd,
        u_k = x_u_k
      ))
      
    } else if (x_ctype == "ccf") {
      
      line_split <- strsplit(trimws(prj[first_row + offset]), " ")[[1]]
      x_G <- as.numeric(line_split[1])
      x_D <- as.numeric(line_split[2])
      x_u_G <- as.numeric(line_split[3])
      x_u_D <- as.numeric(line_split[4])

      # output as a list
      output_list[[i]] <- unbox_atomic(list(
        nr = x_nr,
        spcs = x_spcs,
        ctype = x_ctype,
        name = x_name,
        desc = x_desc,
        G = x_G,
        D = x_D,
        u_G = x_u_G,
        u_D = x_u_D
      ))
  
    } else {
      stop("x_ftype is not drs or ccf. other types have not been coded yet")
    }
    
    # name the element with the filter name
    # check for unique
    names(output_list)[i] <- gsub("_", "", x_name)
    if(i > 1) 
      stopifnot(! (names(output_list)[i] %in% names(output_list)[1:(i-1)]))
    
    offset <- offset + 1
  }

  return(output_list)
}

# =============================================================================
## TO PRJ FROM JSON
write_sourcesSinks <- function(section, obj_to_sub, new_obj_name, new_obj) {
  
  n_el <- length(section)
  
  out_vec <- paste(2, "! source/sink elements:")
  
  # so since you are always doing a full swap, can skip everything else
  # and just paste in as is

  # print all
  for (i in 1:nrow(new_obj)) {

    z <- new_obj[i, ]
    
    # Could leave these in ... but hard to enforce    
    # if(i == 1) stopifnot(grepl("sink", tolower(z$name)))
    # if(i == 2) stopifnot(grepl("source", tolower(z$name)))

    if(z$ctype == "drs") {
    
      out_vec <- c(out_vec, with(z, {
        paste(nr, spcs, ctype, name)
      }))
      
      out_vec <- c(out_vec, with(z, {
        paste(desc)
      }))
      
      out_vec <- c(out_vec, with(z, {
        paste(kd, u_k)
      }))
    
    } else if (z$ctype == "ccf") {
      
      out_vec <- c(out_vec, with(z, {
        paste(nr, spcs, ctype, name)
      }))
      
      out_vec <- c(out_vec, with(z, {
        paste(desc)
      }))
      
      out_vec <- c(out_vec, with(z, {
        paste(G, D, u_G, u_D)
      }))
      
    } else {
      stop("writeSource error")
    }
    
  }
  
  out_vec <- c(out_vec, "-999")
  
  out_vec
}
