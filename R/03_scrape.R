#### LICENSE SCRAPE ############################################################

source("R/01_startup.R")

library(rvest)
library(httr)

# property <- qread("output/property.qs", nthreads = availableCores())


# Get all the property IDs ------------------------------------------------

# PIDs <- 
#   property |>
#   pull(property_ID) |> 
#   str_remove("^ab-")


# Prepare the tibble ------------------------------------------------------

# out <- 
#   tibble(property_ID = PIDs,
#          description = character(length(PIDs)),
#          exists = NA,
#          double_check = NA)

out <- qread("output/ab_lic_2024_03.qs", nthreads = availableCores())


# Simple non-parallelized scrape with no proxies --------------------------

upgo:::handler_upgo("Listing")

progressr::with_progress({
  
  remaining <- sum(out$description == "", na.rm = TRUE) - 
    sum(out$double_check, na.rm = TRUE)
  p <- progressr::progressor(steps = remaining)
  
  for (i in seq_along(out$property_ID)) {
    
    if (!is.na(out$exists[i]) && (out$exists[i] || (!is.na(
      out$double_check[i]) && out$double_check[i]))) {
      next
    }
    
    Sys.sleep(0.8)
    
    page <- tryCatch({httr::GET(
      paste0("https://www.airbnb.ca/rooms/", out$property_ID[i]))
    }, error = function(e) NULL)
    
    p()
    
    # If NULL, skip
    if (is.null(page)) next
    
    # On 403, stop
    if (page$status_code == 403) stop("403 ERROR")
    
    # On 410, return "NO LISTING"
    if (page$status_code == 410) {
      out$exists[i] <- FALSE
      if (is.na(out$double_check[i])) {
        out$double_check[i] <- FALSE
        } else out$double_check[i] <- TRUE
        
    # On 200, return full listing HTML
    } else if (page$status_code == 200) {
      page <- 
        page |> 
        xml2::read_html() |> 
        rvest::html_text()
      
      out$description[i] <- page
      out$exists[i] <- TRUE
    }
    
    if (i %% 100 == 0) qsave(out, "output/ab_lic_2024_03.qs", 
                             nthreads = availableCores())
    
  }
  
})

