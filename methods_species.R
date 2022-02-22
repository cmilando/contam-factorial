# =============================================================================
## tabPanel
species_tabPanel <-     
  tabPanel(
    "Species",
    br(),
    # swap in a new element
    selectInput("base_species", "Base Species:", choices = c()),
    helpText("This element will be replaced in the replicants below."),
    tags$div(HTML("<b>Make replicants with the following species:</b>"),
             style = "margin-bottom: 5px;"
    ),
    tags$div(
      class = "multicol",
      checkboxGroupInput("species_choices",
                         selected = 1:1e5, # a hack to select all
                         label = NULL,
                         get_json_choices("objs/species.JSON", "species")
      )
    ),
  )

# =============================================================================
## FROM PRJ TO JSON
parse_species <- function(prj, first_row) {
  
  # first character is # of elements
  n_ctm_total <- as.integer(strsplit(prj[first_row], "!")[[1]][1])
  offset <- 1
  
  line_split <- strsplit(prj[first_row + offset], " ")[[1]]
  n_ctm <- as.numeric(line_split[4])
  offset <- offset + 1
  
  n_species <- as.integer(strsplit(prj[first_row + offset], "!")[[1]][1])
  offset <- offset + 2
  
  output_list <- vector("list", 1)
  stopifnot(n_species == 1)
  
  # this should always be one
  # stopifnot(n_ctm_total == n_species)
  
  # then repeat this loop for each element
  for (i in 1:n_species) {
    
    # first line
    line_split <- strsplit(prj[first_row + offset], " ")[[1]]
    line_split <- line_split[which(line_split != "")]
    x_nr      <- as.numeric(line_split[1])
    x_sflag   <- as.numeric(line_split[2])
    x_ntflag  <- as.numeric(line_split[3])
    x_molwt   <- as.numeric(line_split[4])
    x_mdiam   <- as.numeric(line_split[5])
    x_edens   <- as.numeric(line_split[6])
    x_decay   <- as.numeric(line_split[7])
    x_Dm      <- as.numeric(line_split[8])
    x_ccdef   <- as.numeric(line_split[9])
    x_Cp      <- as.numeric(line_split[10])
    x_Kuv     <- as.numeric(line_split[11])
    x_ucc     <- as.numeric(line_split[12])
    x_umd     <- as.numeric(line_split[13])
    x_ued     <- as.numeric(line_split[14])
    x_udm     <- as.numeric(line_split[15])
    x_ucp     <- as.numeric(line_split[16])
    x_name    <- line_split[17]
    offset <- offset + 1
    
    # desc line
    x_desc <- prj[first_row + offset]
    offset <- offset + 1
    
    # output as a list
    output_list[[i]] <- unbox_atomic(list(
      nr    = x_nr,
      sflag = x_sflag,
      ntflag  = x_ntflag,
      molwt = x_molwt,
      mdiam  = x_mdiam,
      edens   = x_edens,
      decay = x_decay,
      Dm   = x_Dm,
      ccdef = x_ccdef,
      Cp = x_Cp,
      Kuv = x_Kuv,
      ucc = x_ucc,
      umd = x_umd,
      ued = x_ued,
      udm = x_udm,
      ucp = x_ucp,
      name = x_name,
      desc = x_desc
    ))
    
    # name the element with the filter name
    # check for unique
    names(output_list)[i] <- gsub("_", "", x_name)
    if(i > 1) 
      stopifnot(! (names(output_list)[i] %in% names(output_list)[1:(i-1)]))
    
    # advance the iterators
    offset <- offset + 1
  }

  return(output_list)
}

# =============================================================================
## TO PRJ FROM JSON
write_species <- function(section, obj_to_sub, new_obj_name, new_obj) {
  
  n_el <- length(section)
  stopifnot(length(n_el) == 1)

  # 1 ! contaminants:
  #   1
  # 1 ! species:
  # ! # s t   molwt    mdiam       edens       decay
  # Dm         CCdef        Cp          Kuv     u[5]      name
  out_vec <- paste(1, "! contaminants:")
  out_vec <- c(out_vec, paste(" 1"))
  out_vec <- c(out_vec, paste(1, "! species:"))
  out_vec <- c(out_vec, paste("! # s t   molwt    mdiam       edens",
                              "       decay         Dm         CCdef",
                              "Cp          Kuv     u[5]      name", 
                              collapse = ""))
  
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
  for (i in 1:n_el) {

    z <- section[[i]]
    
    out_vec <- c(out_vec, with(z, {
      paste(nr, sflag, ntflag, molwt, mdiam, edens, decay, Dm, ccdef,
            Cp, Kuv, ucc, umd, ued, udm, ucp, name)
    }))
    
    out_vec <- c(out_vec, with(z, {
      paste(desc)
    }))
    
  }
  
  out_vec <- c(out_vec, "-999")
  
  out_vec
}
