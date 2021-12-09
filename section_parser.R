# =============================================================================
# parsing section data
# expand this with new sections
# suppose you could bake this into contam_sections as well ...
section_parser <- function(prj, first_row, last_row, header_i) {
  
  if (header_i == 8) {
    profiles <- parse_filters(prj, first_row)
    return(profiles)
  }

  return(prj[first_row:last_row])
}

# =============================================================================
#' read PRJ to JSON
prj_to_json <- function(prj_f, out_f) {
  
  section_headers <- read.table(
    file = "contam_sections.txt", sep = "|",
    header = F, stringsAsFactors = F
  )

  section_break <- paste0("!", paste0(rep("-", 80), collapse = ""))

  prj <- readLines(prj_f)
  prj_as_list <- vector("list", nrow(section_headers))
  names(prj_as_list) <- section_headers$V1

  i <- 3
  header_i <- 1

  #' then, just place them after -999
  while (header_i <= nrow(section_headers)) {
    first_row <- i

    #' search for '-999'
    while (prj[i] != "-999") i <- i + 1

    # SAVE AS LIST
    prj_as_list[[header_i]] <- section_parser(prj, first_row, i - 1, header_i)

    #' move to the next section
    i <- i + 1
    header_i <- header_i + 1
  }

  # export JSON
  write(toJSON(prj_as_list, pretty = T), out_f)
  
  prj_as_list
}
