#' Append Licence and Validate
#'
#' This function appends the extracted licence number to the input data frame and
#' validates if the licence is within the list of valid licences.
#'
#' @param abl A data frame containing the 'description' column where the licence
#' number is to be extracted.
#' @param valid_licences A vector of valid licence numbers to check against.
#' @param expired_licences A vector of expired licence numbers to check against.
#'
#' @return A modified data frame containing the extracted licence numbers in a
#' new 'licence' column, a logical 'valid' column indicating if the licence
#' is valid or not, and a logical 'expired' column indicating if the licence
#' is expired or not.
append_licence <- function(abl, valid_licences, expired_licences, licence_regex_grab) {
  # Conformity of the licences detected at the right location
  abl$licence <- stringr::str_extract(abl$description, licence_regex_grab)
  
  abl$expire <- stringr::str_extract(abl$licence, "....-..-..")
  
  # Some clearly invalid licences like `819-238-9196`. Also need some tweaks
  # as some licences read like this `CITQ306551`. We extract only the digits
  abl$licence <- 
    sapply(abl$licence, \(x) {
      if (is.na(x)) return(NA)
      
      if (grepl(", expires", x)) {
        out <- stringr::str_extract(x, "^.*?(<?,)")
        gsub(",", "", out)
      } else {
      stringr::str_extract_all(x, "\\d", simplify = FALSE)[[1]] |> 
        paste0(collapse = "") |> 
        stringr::str_trim()        
      }

    }, USE.NAMES = FALSE)
  
  # Is the licence somewhere else?
  abl <-
    abl |>
    mutate(following_CITQ = str_extract(description, "(?<=CITQ|citq).*?\\d{6}") |>
             str_extract("\\d{6}"),
           any_6dig = str_extract(description, "(?<=[^\\d])\\d{6}(?=[^\\d])")) |>
    mutate(licence_wrong_spot = if_else(is.na(following_CITQ), any_6dig, following_CITQ)) |>
    select(-following_CITQ, -any_6dig)
  
  abl$licence_wrong_spot <-
    sapply(seq_along(abl$licence_wrong_spot), \(x) {
      if (is.na(x)) return(NA)
      if (!is.na(abl$licence[x])) return(NA)
      
      out <- stringr::str_extract_all(abl$licence_wrong_spot[x], "\\d", simplify = FALSE)[[1]] |>
        paste0(collapse = "")
      if (out == "NA") return(NA)
      out
    }, USE.NAMES = FALSE)
  
  # Is the licence valid?
  abl$valid <- abl$licence_wrong_spot %in% valid_licences | abl$licence %in% valid_licences
  abl$valid[is.na(abl$valid)] <- FALSE
  
  # Is the licence expired?
  abl$expired <- abl$licence_wrong_spot %in% expired_licences | abl$licence %in% expired_licences
  abl$expired[is.na(abl$expired)] <- FALSE
  
  return(abl)
}

#' Identify Illegally Shared Licences
#'
#' This function identifies licences that are illegally shared between properties,
#' based on the distance between the properties. If the distance is larger than 
#' 410 meters between two properties sharing the same licence, it is enough to 
#' assume the properties are not co-located.
#'
#' @param property A data frame containing the 'property_ID', 'longitude', and
#' 'latitude' columns for each property.
#' @param abl_df A data frame containing the 'licence', 'valid', and
#' 'property_ID' columns. This should be the output of the append_licence
#' function.
#'
#' @return A list with two elements:
#' - licences: A vector of the licences that are illegally shared.
#' - properties: A vector of property_IDs that are using the illegally shared 
#' licences.
illegally_shared_licences <- function(property, abl_df) {
  
  # Filter in only the ones with a licence
  has_licence <- abl_df[abl_df$valid, ]
  
  property_sf <- 
    sf::st_as_sf(property, coords = c("longitude", "latitude"), crs = 4326)
  
  # Iterate over all used licences. Is it duplicated?
  licence_use_count <- table(has_licence$licence)
  used_more_than_once <- names(licence_use_count)[licence_use_count > 1]
  
  too_far <- sapply(used_more_than_once, \(li) {
    properties <- has_licence$property_ID[has_licence$licence == li]
    properties <- property_sf[property_sf$property_ID %in% properties, ]
    
    distances <- sf::st_distance(properties)
    sum(as.vector(distances) > 410) > 0
  })
  
  too_far_licences <- used_more_than_once[too_far]
  too_far <- has_licence$property_ID[has_licence$licence %in% too_far_licences]
  
  return(list(licences = too_far_licences,
              properties = too_far))
  
}

#' Retrieve and Calculate Property Statistics
#'
#' This function calculates various statistics for a given set of property IDs,
#' such as the percentage of properties still active, the percentage of
#' properties with minimum stays > 31 nights, and the percentage of properties 
#' with valid, expired, fake, and illegally shared licences.
#'
#' @param property_IDs A vector of property IDs to calculate the statistics for.
#' @param abl_df A data frame containing the 'property_ID', 'exists',
#' 'description', and 'licence' columns. This should be the output of the
#' append_licence function.
#' @param illegaly_shared A list containing the illegally shared licences and
#' properties. This should be the output of the illegally_shared_licences function.
#' @param valid_licences A vector of valid licence numbers to check against.
#' @param expired_licences A vector of expired licence numbers to check against.
#' 
#' @return A list containing various statistics related to the given property
#' IDs, including the number and percentage of properties still active, with
#' minimum stays, and with valid, expired, fake, and illegally shared licences.
get_values <- function(property_IDs, abl_df, illegally_shared = illegally_shared,
                       valid_licences, expired_licences) {
  
  scraped <- abl_df[abl_df$property_ID %in% property_IDs, ]

  # From all the properties that were accessible through the web since January 1st,
  # this is the percentage of properties that were still accessible
  still_active <- scraped[scraped$exists, ]

  # Used to be >= 31 minimum stays
  property_still_active <- 
    property |> 
    filter(property_ID %in% still_active$property_ID)

  # From the still active properties, this many now have a min stay >= 31.
  min_stay_merged <- 
    still_active |> 
    left_join(select(property, property_ID, min_stay))
  
  # Either there is no registration and so the listing moved to long term, or if
  # it is VRBO it hasn't changed. If it was long term, it's still long term even
  # though there is no registration number (no purge there)
  long_term_index <- min_stay_merged$min_stay >= 31
  
  long_term <- still_active[long_term_index, ]
  nb_long_term <- nrow(long_term)
  pct_long_term <- nb_long_term / nrow(still_active)
  
  # Display any string in the licence number or is from VRBO and is NOT long term there
  str <- 
    still_active |> 
    filter(!long_term_index)
  
  nb_short_term <- nrow(str)
  pct_short_term <- nb_short_term / nrow(still_active)
  
  # How many listings show a VALID licence in the wrong spot
  nb_valid_wrong_spot <- sum(str$licence_wrong_spot %in% valid_licences)
  pct_valid_wrong_spot <- nb_valid_wrong_spot / nrow(str)
  
  # How many are NOT displaying a licence
  nb_display_nothing <- sum(is.na(str$licence) & is.na(str$licence_wrong_spot))
  pct_display_nothing <- nb_display_nothing / nrow(str)
  
  # Of all the listings, no matter where they place the registration number, how
  # many are valid
  display_valid <- str$property_ID[str$valid]
  
  nb_display_valid <- length(display_valid)
  pct_display_valid <- nb_display_valid / nrow(str)
  
  # Of all the listings that displayed a licence, how many are expired
  nb_display_expired <- nrow(str[str$expired, ])
  pct_display_expired <- nb_display_expired / nrow(str)
  
  # Fake number. not valid nor expired = fake
  nb_display_fake <- sum(!c(str$licence[!is.na(str$licence)], 
                           str$licence_wrong_spot[!is.na(str$licence_wrong_spot)]) %in% 
                           c(valid_licences, expired_licences))
  pct_display_fake <- nb_display_fake / nrow(str)
  
  pct_display_expired_fake <- sum(pct_display_expired, pct_display_fake)

  # # Properties with valid licences that are > 5000m from the registered address
  # str |> 
  #   filter(valid) |> 
  #   filter(permit_dist > 5000)
  
  # The number of licences operating multiple properties that are >410m away
  # from each other
  nb_illegally_shared_licences <- 
    illegally_shared$licence[illegally_shared$licence %in% str$licence] |> 
    unique() |> 
    length()
  
  nb_illegally_shared_properties <- 
    illegally_shared$properties[illegally_shared$properties %in% str$property_ID] |> 
    unique() |> 
    length()
  
  pct_illegally_shared <- nb_illegally_shared_properties / nrow(str)
  
  # Conform
  nb_conform <- 
    nrow(str) - nb_display_expired - nb_display_fake - nb_illegally_shared_properties - nb_display_nothing
  pct_conform <- nb_conform / nrow(str)
  
  # Return all
  return(list(nb_short_term = nb_short_term,
              pct_short_term = pct_short_term,
              nb_long_term = nb_long_term,
              pct_long_term = pct_long_term,
              nb_valid_wrong_spot = nb_valid_wrong_spot,
              pct_valid_wrong_spot = pct_valid_wrong_spot,
              nb_display_nothing = nb_display_nothing,
              pct_display_nothing = pct_display_nothing,
              nb_display_valid = nb_display_valid,
              pct_display_valid = pct_display_valid,
              nb_display_expired = nb_display_expired,
              pct_display_expired = pct_display_expired,
              nb_display_fake = nb_display_fake,
              pct_display_fake = pct_display_fake,
              pct_display_expired_fake = pct_display_expired_fake,
              # nb_dist_too_far = nb_dist_too_far,
              # pct_dist_too_far = pct_dist_too_far,
              nb_illegally_shared_licences = nb_illegally_shared_licences,
              nb_illegally_shared_properties = nb_illegally_shared_properties,
              pct_illegally_shared = pct_illegally_shared,
              nb_conform = nb_conform,
              pct_conform = pct_conform))
}
