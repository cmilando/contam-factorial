# =============================================================================
## tabPanel
flow_elements_tabPanel <-     
  tabPanel(
    "Flow paths",
    br(),
    # swap in a new element
    selectInput("base_flow_element", "Base flow element:", choices = c()),
    helpText("This element will be replaced in the replicants below."),
    tags$div(HTML("<b>Make replicants with the following flow elements:</b>"),
             style = "margin-bottom: 5px;"
    ),
    tags$div(
      #class = "multicol", # << might not need this in every one
      checkboxGroupInput("flow_element_choices",
                         selected = 1:1e5, # a hack to select all
                         label = NULL,
                         get_json_choices("objs/flow_elements.JSON", "flow_elements")
      )
    ),
  )

# =============================================================================
## FROM PRJ TO JSON
parse_flow_elements <- function(prj, first_row) {
  
  # first character is # of elements
  n <- as.integer(strsplit(prj[first_row], "!")[[1]][1])

  output_list <- vector("list", n)
  offset <- 1
  
  # then repeat this loop for each element
  for (i in 1:n) {
    
    # first line
    line_split <- strsplit(prj[first_row + offset], " ")[[1]]
    x_nr <- as.numeric(line_split[1])
    x_icon <- as.numeric(line_split[2])
    x_dtype <- line_split[3]
    x_name <- line_split[4]
    offset <- offset + 1
    
    # desc line
    x_desc <- prj[first_row + offset]
    offset <- offset + 1
    
    # -----------------------------------------
    # next line is data
    if (x_dtype %in% c("plr_leak1", "plr_leak2", "plr_leak3")) {
      
      line_split <- strsplit(trimws(prj[first_row + offset]), " ")[[1]]
      x_lam <- as.numeric(line_split[1])
      x_turb <- as.numeric(line_split[2])
      x_expt <- as.numeric(line_split[3])
      x_coef <- as.numeric(line_split[4])
      x_pres <- as.numeric(line_split[5])
      x_area1 <- as.numeric(line_split[6])
      x_area2 <- as.numeric(line_split[7])
      x_area3 <- as.numeric(line_split[8])
      x_u_A1 <- as.numeric(line_split[9])
      x_u_A2 <- as.numeric(line_split[10])
      x_u_A3 <- as.numeric(line_split[11])
      x_u_dP <- as.numeric(line_split[12])
      
      # output as a list
      output_list[[i]] <- unbox_atomic(list(
        nr = x_nr,
        icon = x_icon,
        dtype = x_dtype,
        desc = x_desc,
        name = x_name,
        lam = x_lam,
        turb = x_turb,
        expt = x_expt,
        coef = x_coef,
        pres = x_pres,
        area1 = x_area1,
        area2 = x_area2,
        area3 = x_area3,
        u_A1 = x_u_A1,
        u_A2 = x_u_A2,
        u_A3 = x_u_A3,
        u_dP = x_u_dP
      ))
    
    # -----------------------------------------
    } else if(x_dtype == "plr_orfc") {
      
      line_split <- strsplit(trimws(prj[first_row + offset]), " ")[[1]]
      x_lam <- as.numeric(line_split[1])
      x_turb <- as.numeric(line_split[2])
      x_expt <- as.numeric(line_split[3])
      x_area <- as.numeric(line_split[4])
      x_dia <- as.numeric(line_split[5])
      x_coef <- as.numeric(line_split[6])
      x_Re <- as.numeric(line_split[7])
      x_u_A <- as.numeric(line_split[8])
      x_u_D <- as.numeric(line_split[9])

      # output as a list
      output_list[[i]] <- unbox_atomic(list(
        nr = x_nr,
        icon = x_icon,
        dtype = x_dtype,
        name = x_name,
        desc = x_desc,
        lam = x_lam,
        turb = x_turb,
        expt = x_expt,
        area = x_area,
        dia = x_dia,
        coef = x_coef,
        Re = x_Re,
        u_A = x_u_A,
        u_D = x_u_D
      ))
    
    # -----------------------------------------  
    } else if (x_dtype == "plr_stair"){
      
      line_split <- strsplit(trimws(prj[first_row + offset]), " ")[[1]]
      x_lam <- as.numeric(line_split[1])
      x_turb <- as.numeric(line_split[2])
      x_expt <- as.numeric(line_split[3])
      x_ht <- as.numeric(line_split[4])
      x_area <- as.numeric(line_split[5])
      x_peo <- as.numeric(line_split[6])
      x_tread <- as.numeric(line_split[7])
      x_u_A <- as.numeric(line_split[8])
      x_u_D <- as.numeric(line_split[9])
      
      # output as a list
      output_list[[i]] <- unbox_atomic(list(
        nr = x_nr,
        icon = x_icon,
        dtype = x_dtype,
        name = x_name,
        desc = x_desc,
        lam = x_lam,
        turb = x_turb,
        expt = x_expt,
        ht = x_ht,
        area = x_area,
        peo = x_peo,
        tread = x_tread,
        u_A = x_u_A,
        u_D = x_u_D
      ))
    
    # -----------------------------------------
    } else if(x_dtype == "dor_door") {  
      
      line_split <- strsplit(trimws(prj[first_row + offset]), " ")[[1]]
      x_lam <- as.numeric(line_split[1])
      x_turb <- as.numeric(line_split[2])
      x_expt <- as.numeric(line_split[3])
      x_dTmin <- as.numeric(line_split[4])
      x_ht <- as.numeric(line_split[5])
      x_wd <- as.numeric(line_split[6])
      x_cd <- as.numeric(line_split[7])
      x_u_T <- as.numeric(line_split[8])
      x_u_H <- as.numeric(line_split[9])
      x_u_W <- as.numeric(line_split[10])
      
      # output as a list
      output_list[[i]] <- unbox_atomic(list(
        nr = x_nr,
        icon = x_icon,
        dtype = x_dtype,
        desc = x_desc,
        name = x_name,
        lam = x_lam,
        turb = x_turb,
        expt = x_expt,
        dTmin = x_dTmin,
        ht = x_ht,
        wd = x_wd,
        cd = x_cd,
        u_T = x_u_T,
        u_H = x_u_H,
        u_W = x_u_W
      ))
    
    # -----------------------------------------
    } else if(x_dtype == "plr_test1") {  
      
      line_split <- strsplit(trimws(prj[first_row + offset]), " ")[[1]]
      x_lam <- as.numeric(line_split[1])
      x_turb <- as.numeric(line_split[2])
      x_expt <- as.numeric(line_split[3])
      x_dP <- as.numeric(line_split[4])
      x_Flow <- as.numeric(line_split[5])
      x_u_P <- as.numeric(line_split[6])
      x_u_F <- as.numeric(line_split[7])

      # output as a list
      output_list[[i]] <- unbox_atomic(list(
        nr = x_nr,
        icon = x_icon,
        dtype = x_dtype,
        desc = x_desc,
        name = x_name,
        lam = x_lam,
        turb = x_turb,
        expt = x_expt,
        dP = x_dP,
        Flow = x_Flow,
        u_P = x_u_P,
        u_F = x_u_F
      ))  
    
    # -----------------------------------------
    } else if(x_dtype == "dor_pl2") {  
      
      line_split <- strsplit(trimws(prj[first_row + offset]), " ")[[1]]
      x_lam <- as.numeric(line_split[1])
      x_turb <- as.numeric(line_split[2])
      x_expt <- as.numeric(line_split[3])
      x_dH <- as.numeric(line_split[4])
      x_ht <- as.numeric(line_split[5])
      x_wd <- as.numeric(line_split[6])
      x_cd <- as.numeric(line_split[7])
      x_u_H <- as.numeric(line_split[8])
      x_u_W <- as.numeric(line_split[9])
      
      # output as a list
      output_list[[i]] <- unbox_atomic(list(
        nr = x_nr,
        icon = x_icon,
        dtype = x_dtype,
        desc = x_desc,
        name = x_name,
        lam = x_lam,
        turb = x_turb,
        expt = x_expt,
        dH = x_dH,
        ht = x_ht,
        wd = x_wd,
        cd = x_cd,
        u_H = x_u_H,
        u_W = x_u_W
      ))  
          
    } else {
      stop(paste0("x_dtype=", x_dtype, " has not been coded yet"))
    }
    
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
write_flow_elements <- function(section, obj_to_sub, new_obj_name, new_obj) {
  
  n_el <- length(section)
  
  out_vec <- paste(n_el, "! filter elements:")
  
  # swap in the new object
  # make sure to preserve the NR, because that is what is passed around
  # this will be different for each prj part ... < will it?
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
    
    ## FIRST LINE
    out_vec <- c(out_vec, with(z, {
      paste(nr, icon, dtype, name)
    }))
    
    ## DESC
    out_vec <- c(out_vec, with(z, {
      paste(desc)
    }))
    
    ## TYPE-SPECIFIC
    if (z$dtype %in% c("plr_leak1", "plr_leak2", "plr_leak3")) {
      
      out_vec <- c(out_vec, with(z, {
        paste(lam, turb, expt, coef, pres, 
              area1, area2, area3, u_A1, u_A2, u_A3, u_dP)
      }))
      
    } else if(z$dtype == "plr_orfc"){
      
      out_vec <- c(out_vec, with(z, {
        paste(lam, turb, expt, area, dia, coef, Re, u_A, u_D)
      }))
      
    } else if(z$dtype == "plr_stair"){
      
      out_vec <- c(out_vec, with(z, {
        paste(lam, turb, expt, ht, area, 
              peo, tread, u_A, u_D)
      }))
      
    } else if(z$dtype == "dor_door"){

      out_vec <- c(out_vec, with(z, {
        paste(lam, turb, expt, dTmin, ht, 
              wd, cd, u_T, u_H, u_W)
      }))
    
    } else if(z$dtype == "plr_test1"){
      
      out_vec <- c(out_vec, with(z, {
        paste(lam, turb, expt, dP, Flow, u_P, u_F)
      }))
    
    } else if(z$dtype == "dor_pl2"){
      
      out_vec <- c(out_vec, with(z, {
        paste(lam, turb, expt, dH, ht, wd, cd, u_H, u_W)
      }))  
      
    } else {
      stop("write: x_dtype is not in known types.")
    }
  }
  
  out_vec <- c(out_vec, "-999")
  
  out_vec
}
