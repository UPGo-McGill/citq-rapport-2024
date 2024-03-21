#### Chapter 3 calculations ####################################################

# Load functions and data -------------------------------------------------

source("R/01_startup.R")
source("R/functions/permit_compliance.R")
source("R/functions/scrape_comparisons.R")

abl_6 <- qread("output/ab_lic_2024_03.qs", nthreads = availableCores())

property_new <- qs::qread("output/property.qs")
property_old <- qread("data/property_old.qs")
mto <- qs::qread("output/mto.qs")


# Merge property files ----------------------------------------------------

property <- 
  property_new |> 
  mutate(ab_property = str_remove(property_ID, "ab-"),
         ha_property = NA_character_)

property <- 
  property_old |> 
  anti_join(property, by = "property_ID") |> 
  bind_rows(property) |> 
  arrange(property_ID)



# Get licenses for new data -----------------------------------------------

valid_licences <- mto$id[mto$active]
expired_licences <- mto$id[!mto$active]

abl <- list()

abl$march_2024 <- 
  abl_6 |> 
  select(-double_check) |> 
  mutate(property_ID = paste0("ab-", property_ID)) |> 
  mutate(description = description |> 
           str_replace_all("\\\\u003c.*?(/>|>)", " ") |> 
           str_remove_all('\\\\|\\"')) |> 
  mutate(licence = str_extract(
    description, 
    "Registration number  \\d{6}, expires: \\d{4}-\\d{2}-\\d{2}")) |> 
  mutate(expire = as.Date(str_remove(licence, ".*expires: "))) |> 
  mutate(licence = str_remove(licence, "Registration number  ")) |> 
  mutate(licence = str_remove(licence, ", expires.*")) |> 
  mutate(licence_wrong_spot = NA_character_) |> 
  mutate(valid = licence %in% valid_licences) |> 
  mutate(expired = licence %in% expired_licences)

illegally_shared <- illegally_shared_licences(property = property,
                                              abl_df = abl$march_2024)


# Regions -----------------------------------------------------------------

# Load regions
regions <- sf::st_read("data/shp/regio_s.shp")
regions <- regions[regions$RES_CO_REG %in% c("06", "15", "03", "05", "07"), ]

this_property <- property#[stringr::str_detect(property$property_ID, "^ha-"), ]

properties_region <- sapply(regions$RES_NM_REG, \(region) {
  reg_sf <- regions[regions$RES_NM_REG == region, ]
  property_sf <- strr::strr_as_sf(this_property, CRS = st_crs(reg_sf)$input)
  
  out <- sf::st_filter(property_sf, reg_sf)
  return(out$property_ID)
}, simplify = FALSE, USE.NAMES = TRUE)

properties_region$`Toutes les régions` <- Reduce(c, properties_region)
properties_region <- properties_region[c(6,1:5)]

properties_region <- 
  lapply(properties_region, get_values, abl_df = abl$march_2024,
         illegally_shared = illegally_shared, valid_licences = valid_licences,
         expired_licences = expired_licences)

properties_region <- mapply(\(n, x) {
  x$`Région` <- n
  x
}, names(properties_region), properties_region, SIMPLIFY = FALSE)

properties_region <- 
  lapply(properties_region, \(x) {
    tibble::tibble(
      `Région` = x$`Région`,
      `Annonces n'affichant pas de licence` = x$pct_display_nothing |> 
        scales::percent(accuracy = 0.1, decimal.mark = ",",
                        suffix = " %"),
      `Annonces partagées illégalement` = x$pct_illegally_shared |> 
        scales::percent(accuracy = 0.1, decimal.mark = ",",
                        suffix = " %"),
      `Annonces expirées` = x$pct_display_expired |> 
        scales::percent(accuracy = 0.1, decimal.mark = ",",
                        suffix = " %"),
      `Annonces invalides` = x$pct_display_fake |> 
        scales::percent(accuracy = 0.1, decimal.mark = ",",
                        suffix = " %"),
      `Annonces avec licence mal placée` = x$pct_valid_wrong_spot |> 
        scales::percent(accuracy = 0.1, decimal.mark = ",",
                        suffix = " %"),
      `Conformité` = x$pct_conform |> 
        scales::percent(accuracy = 0.1, decimal.mark = ",",
                        suffix = " %"))
    })

properties_region <- Reduce(rbind, properties_region)

list_count <- 
  property |> 
  filter(min_stay < 31, scraped >= "2024-02-01") |> 
  count(listing_type) |> 
  summarize(n = sum(n)) |> 
  bind_rows(property |> 
              filter(min_stay < 31, scraped >= "2024-02-01") |> 
              count(listing_type) |> 
              select(n)) |> 
  pull(n) |> 
  scales::comma()

properties_region |> 
  kableExtra::kbl(caption = "Taux de conformité par région",
                  align = "lrrrrr") |>
  kableExtra::kable_styling(latex_options = "scale_down")

