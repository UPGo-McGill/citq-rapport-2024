get_scrape_comparison <- function(first_scrape, second_scrape) {
  # Were existing in the first scrape
  first_scrape <- first_scrape[first_scrape$exists, ]
  second_scrape <- second_scrape[second_scrape$exists, ]
  
  taken_down <- first_scrape[!first_scrape$property_ID %in% second_scrape$property_ID, ]
  nb_taken_down <- nrow(taken_down)
  pct_taken_down <- nb_taken_down / nrow(first_scrape)
  
  host_took_down <- 
    property$host_ID[property$property_ID %in% taken_down$property_ID]
  nb_host_took_down <- unique(host_took_down) |> length()
  
  return(list(nb_taken_down = nb_taken_down,
              pct_taken_down = pct_taken_down,
              nb_host_took_down = nb_host_took_down))
}
