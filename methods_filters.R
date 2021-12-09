# =============================================================================
## FROM PRJ TO JSON
parse_filters <- function(prj, first_row) {

  # first character is # of filters
  n <- as.integer(strsplit(prj[first_row], "!")[[1]][1])

  output_list <- vector("list", n)
  offset <- 1

  # then repeat this loop for each filter
  for (i in 1:n) {

    # first line
    line_split <- strsplit(prj[first_row + offset], " ")[[1]]
    x_nr <- as.numeric(line_split[1])
    x_ftype <- line_split[2]
    x_area <- as.numeric(line_split[3])
    x_depth <- as.numeric(line_split[4])
    x_dens <- as.numeric(line_split[5])
    x_ual <- as.numeric(line_split[6])
    x_ud <- as.numeric(line_split[7])
    x_name <- line_split[8]
    offset <- offset + 1

    # desc line
    x_desc <- prj[first_row + offset]
    offset <- offset + 1

    # initialize
    x_nspcs <- NA_real_ # cef, gf0
    x_npts <- NA_real_ # pf0
    x_usz <- NA_real_ # pf0

    # next line is dat
    if (x_ftype == "pf0") {
      line_split <- strsplit(trimws(prj[first_row + offset]), " ")[[1]]
      x_npts <- as.numeric(line_split[1])
      x_usz <- as.numeric(line_split[2])
      offset <- offset + 1

      dat_vec <- prj[(first_row + offset):(first_row + offset + x_npts - 1)]
      x_dat <- read.table(
        text = dat_vec, sep = "", strip.white = T,
        col.names = c("size", "eff")
      )
    } else {
      stop("x_ftype is not pf0. other types have not been coded yet")
      ## do gf0 as a data.frame with the gas values as repeated column data
    }

    # output as a list
    output_list[[i]] <- unbox_atomic(list(
      nr = x_nr,
      ftype = x_ftype,
      area = x_area,
      depth = x_depth,
      dens = x_dens,
      ual = x_ual,
      ud = x_ud,
      name = x_name,
      desc = x_desc,
      nspcs = x_nspcs,
      npts = x_npts,
      usz = x_usz,
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
write_filters <- function(section, obj_to_sub, new_obj_name, new_obj) {
  
  n_filters <- length(section)
  
  out_vec <- paste(n_filters, "! filter elements:")
  
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
  for (i in 1:n_filters) {
    
    z <- section[[i]]
    
    out_vec <- c(out_vec, with(z, {
      paste(nr, ftype, area, depth, dens, ual, ud, name)
    }))
    
    out_vec <- c(out_vec, with(z, {
      paste(desc)
    }))
    
    if (z$ftype == "pf0") {
      out_vec <- c(out_vec, with(z, {
        paste("", npts, usz)
      }))
      
      out_vec <- c(out_vec, with(z, {
        sapply(1:nrow(dat), function(i) {
          paste(" ", paste0(dat[i, ], collapse = " "))
        })
      }))
    } else {
      stop("x_ftype is not pf0. other types have not been coded yet")
    }
  }
  
  out_vec <- c(out_vec, "-999")
  
  out_vec
}
