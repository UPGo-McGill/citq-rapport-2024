#### PROCESS STR DATA ##########################################################

source("R/01_startup.R")


# Load data ---------------------------------------------------------------

property <- read_csv("data/property_inside.csv", col_types = cols(
  id = col_character(),
  listing_url = col_character(),
  scrape_id = col_double(),
  last_searched = col_date(format = ""),
  last_scraped = col_date(format = ""),
  name = col_character(),
  description = col_character(),
  neighborhood_overview = col_character(),
  picture_url = col_character(),
  host_id = col_character(),
  host_url = col_character(),
  host_name = col_character(),
  host_since = col_date(format = ""),
  host_location = col_character(),
  host_about = col_character(),
  host_response_time = col_character(),
  host_response_rate = col_character(),
  host_acceptance_rate = col_character(),
  host_is_superhost = col_logical(),
  host_thumbnail_url = col_character(),
  host_picture_url = col_character(),
  host_neighbourhood = col_character(),
  host_listings_count = col_double(),
  host_total_listings_count = col_double(),
  host_verifications = col_character(),
  host_has_profile_pic = col_logical(),
  host_identity_verified = col_logical(),
  neighbourhood = col_character(),
  latitude = col_double(),
  longitude = col_double(),
  property_type = col_character(),
  room_type = col_character(),
  accommodates = col_double(),
  bathrooms = col_logical(),
  bathrooms_text = col_character(),
  bedrooms = col_double(),
  beds = col_double(),
  amenities = col_character(),
  price = col_character(),
  minimum_nights = col_double(),
  maximum_nights = col_double(),
  minimum_minimum_nights = col_double(),
  maximum_minimum_nights = col_double(),
  minimum_maximum_nights = col_double(),
  maximum_maximum_nights = col_double(),
  minimum_nights_avg_ntm = col_double(),
  maximum_nights_avg_ntm = col_double(),
  calendar_updated = col_logical(),
  has_availability = col_logical(),
  availability_30 = col_double(),
  availability_60 = col_double(),
  availability_90 = col_double(),
  availability_365 = col_double(),
  calendar_last_scraped = col_date(format = ""),
  number_of_reviews = col_double(),
  number_of_reviews_ltm = col_double(),
  number_of_reviews_l30d = col_double(),
  first_review = col_date(format = ""),
  last_review = col_date(format = ""),
  review_scores_rating = col_double(),
  review_scores_accuracy = col_double(),
  review_scores_cleanliness = col_double(),
  review_scores_checkin = col_double(),
  review_scores_communication = col_double(),
  review_scores_location = col_double(),
  review_scores_value = col_double(),
  requires_license = col_logical(),
  license = col_character(),
  instant_bookable = col_logical(),
  calculated_host_listings_count = col_double(),
  calculated_host_listings_count_entire_homes = col_double(),
  calculated_host_listings_count_private_rooms = col_double(),
  calculated_host_listings_count_shared_rooms = col_double(),
  region_id = col_double(),
  region_name = col_character(),
  region_parent_id = col_double(),
  region_parent_name = col_character(),
  region_parent_parent_id = col_double(),
  region_parent_parent_name = col_character(),
  reviews_per_month = col_double()
  )
)


# Clean up raw file -------------------------------------------------------
 
property <-
  property |>
  filter(region_parent_parent_name == "Quebec / Québec") |> 
  transmute(property_ID = paste0("ab-", id),
            host_ID = host_id,
            listing_title = name,
            property_type,
            listing_type = room_type,
            scraped = last_scraped,
            latitude,
            longitude,
            min_stay = minimum_nights)


# Save --------------------------------------------------------------------

qs::qsave(property, "output/property.qs")
