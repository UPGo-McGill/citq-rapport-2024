#### 5. Chapter 1 calculations #################################################

# Load functions and data -------------------------------------------------

source("R/01_startup.R")
source("R/functions/permit_compliance.R")

# Airbnb licenses
abl <- qread("output/ab_lic_2024_03.qs", nthreads = availableCores())

property_new <- qs::qread("output/property.qs")
mto <- qs::qread("output/mto.qs")


# Merge property files ----------------------------------------------------

property <- 
  property_new |> 
  mutate(ab_property = str_remove(property_ID, "ab-"),
         ha_property = NA_character_)


# Process data ------------------------------------------------------------

valid_licences <- mto$id[mto$active]
expired_licences <- mto$id[!mto$active]

abl <- 
  abl |> 
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

# Join with property and mto
abl <- 
  abl |> 
  inner_join(select(property, property_ID, listing_type, latitude:min_stay)) |> 
  left_join(select(mto, licence = id, mto_geom = geometry)) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(mto))


# Find distance from STR to permit ----------------------------------------

abl <- 
  abl |> 
  mutate(geometry = st_transform(geometry, 3347),
         mto_geom = st_transform(mto_geom, 3347)) |> 
  mutate(permit_dist = map2_dbl(geometry, mto_geom, st_distance)) |> 
  st_drop_geometry() |> 
  select(-mto_geom)


# Process data ------------------------------------------------------------

illegally_shared_march <- 
  illegally_shared_licences(property = property, abl_df = abl)

z <- get_values(property_IDs = property$property_ID,
                abl_df = abl,
                illegally_shared = illegally_shared_march, 
                valid_licences = valid_licences,
                expired_licences = expired_licences)

# Get some fake licences examples
abl |> 
  filter(!is.na(licence)) |> 
  filter(!licence %in% mto$id) |> 
  arrange(licence)

# These were back in march. Not anymore!
# fake_licence_examples <- c("1217739060TQ0002", "438-935-0550", 
#                            "STR-2101-HWJRVL")


# Confirm all STR show a registration number ------------------------------

str_pr <- 
  property |> 
  filter(min_stay < 31) |> 
  pull(property_ID)
str_pr <- str_pr[!is.na(str_pr)]

str_licences <-
  abl |> 
    filter(exists) |> 
    filter(property_ID %in% str_pr, !is.na(description))

str_licences |> 
  filter(is.na(licence))


# Format output -----------------------------------------------------------

# Format
z_nb <- 
  z[names(z)[str_detect(names(z), "^nb")]] |> 
  lapply(prettyNum, big.mark = " ")

z_pct <- 
  z[names(z)[str_detect(names(z), "^pct")]] |> 
  lapply(scales::percent, accuracy = 0.1, decimal.mark = ",",
         suffix = " %")

z <- c(z_nb, z_pct)


# Overview ----------------------------------------------------------------

# All properties
property_new |> 
  nrow()

# Scraped
abl |> 
  filter(exists) |> 
  nrow()

# STR
z$nb_short_term
z$pct_short_term

# LTR
z$nb_long_term
z$pct_long_term

# Missing licence
z$nb_display_nothing
z$pct_display_nothing

# Double check these
abl |> 
  filter(exists) |> 
  filter(property_ID %in% property_new$property_ID[property_new$min_stay < 31]) |> 
  filter(is.na(licence))

# Expired
z$nb_display_expired
z$pct_display_expired

# Invalid
z$nb_display_fake
z$pct_display_fake

# With valid licence
z$nb_display_valid
z$pct_display_valid

# Illegally shared
z$nb_illegally_shared_properties
z$pct_illegally_shared
z$nb_illegally_shared_licences

# Totals
z$pct_display_expired_fake
z$pct_conform
z$nb_conform


# Figure 1 ----------------------------------------------------------------

tb_now <- 
  tibble(all = character(), group = character(), value = numeric()) |> 
  add_row(all = "Annonces", group = "Fausses licences", 
          value = gsub("\\s", "", z$nb_display_fake) |> as.numeric()) |> 
  add_row(all = "Annonces", group = "Licences expirées", 
          value = gsub("\\s", "", z$nb_display_expired) |> as.numeric()) |> 
  add_row(all = "Annonces", group = "Licences partagées illégalement", 
          value = gsub("\\s", "", z$nb_illegally_shared_properties) |> 
            as.numeric()) |> 
  add_row(all = "Annonces", group = "N'affichent pas de licence",
          value = gsub("\\s", "", z$nb_display_nothing) |> 
            as.numeric()) |> 
  add_row(all = "Annonces", group = "Licences valides", 
          value = gsub("\\s", "", z$nb_conform) |> as.numeric())

figure_1 <- 
  tb_now |> 
  ggplot() +
  geom_col(aes(all, value, fill = group)) + 
  scale_fill_manual(breaks = tb_now$group, 
                    values = col_palette[c(1, 4, 2, 3, 5)]) +
  coord_flip() +
  xlab("") +
  theme_minimal() +
  scale_y_continuous(name = "", labels = scales::label_comma(big.mark = " ")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura"))

ggsave("output/figure_1.png", figure_1, width = 6.5, height = 2)


# Table 1 -----------------------------------------------------------------

# Get all the possible listing types
types <- unique(property$listing_type) |> na.omit()

ids_per_type <- 
  sapply(types, \(x) property$property_ID[property$listing_type == x], 
         simplify = FALSE, USE.NAMES = TRUE)

ids_per_type$`All properties` <- Reduce(c, ids_per_type)
ids_per_type <- ids_per_type[c(5,1:4)]

properties_type <- 
  lapply(ids_per_type, get_values, abl_df = abl,
         illegally_shared = illegally_shared_march, valid_licences = valid_licences,
         expired_licences = expired_licences)

properties_type <- mapply(\(n, x) {
  x$`Type de propriété` <- n
  x
  }, names(properties_type), properties_type, SIMPLIFY = FALSE)

properties_type <- 
  lapply(properties_type, \(x) {
    tibble::tibble(
      `Type de propriété` = x$`Type de propriété`,
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

properties_type <- Reduce(rbind, properties_type)

properties_type$`Type de propriété` <- 
  c("Toutes les annonces", "Logement entier", "Chambre privée", 
    "Chambre partagée", "Chambre d'hôtel")

all_properties_type <- properties_type

# Count of listings in March
list_count <-
  property |> 
  filter(min_stay < 31, scraped >= "2024-02-01") |> 
  filter(property_ID %in% abl$property_ID[abl$exists]) |> 
  count(listing_type) |> 
  summarize(n = sum(n)) |> 
  bind_rows(property |> 
              filter(min_stay < 31, scraped >= "2024-02-01") |> 
              count(listing_type) |> 
              select(n)) |> 
  pull(n) |> 
  scales::comma()

all_properties_type |> 
  mutate(`Annonces de LCT (mars)` = list_count, .after = 1) |> 
  kableExtra::kbl(
    caption = "Conformité des annonces par type d'annonce (Airbnb)",
    align = "lrrrrrr") |>
  kableExtra::kable_styling(latex_options = "scale_down")
