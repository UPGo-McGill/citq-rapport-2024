#### Chapter 2 calculations ####################################################

# Load functions and data -------------------------------------------------

source("R/01_startup.R")
source("R/functions/permit_compliance.R")
source("R/functions/scrape_comparisons.R")

# Airbnb licenses in chronological order
abl_1 <- qread("output/airbnb_licences_summer.qs")
abl_2 <- qread("output/airbnb_licences_midmarch.qs")
abl_3 <- qread("output/airbnb_licences_endmarch.qs")
abl_4 <- qread("output/airbnb_licences_aug_final.qs")
abl_4$property_ID <- sprintf("ab-%s", abl_4$property_ID)
abl_5 <- qread("data/ab_lic_sept_09.qs")
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


# Process data ------------------------------------------------------------

# In a list with the `ab-` we can recognise from the `property` db
abl <- list(summer = abl_1, monday = abl_2, thursday = abl_3, august = abl_4,
            sept = abl_5)

# Licence number scraped regex
licence_regex <- "(Licence number  .*,$)|(Registration number  .*$)"
licence_regex_grab <- 
  "((?<=Licence number  ).*(?=,$))|(?<=Registration number  ).*(?=$)"

# MARCH AND BEFORE, USE THE OTHER MTO DB
mto <- qs::qread("output/mto_march.qs")

valid_licences <- mto$id[mto$active]
expired_licences <- mto$id[!mto$active]

abl[1:3] <- lapply(abl[1:3], append_licence, valid_licences = valid_licences,
              expired_licences = expired_licences, 
              licence_regex_grab = licence_regex_grab)

# IN AUGUST AND SEPT, USE THE NEW MTO DB
mto <- qs::qread("output/mto_aug.qs")

valid_licences <- mto$id[mto$active]
expired_licences <- mto$id[!mto$active]

abl$august <- append_licence(abl$august, valid_licences = valid_licences,
                             expired_licences = expired_licences, 
                             licence_regex_grab = licence_regex_grab)

abl$sept <- append_licence(abl$sept, valid_licences = valid_licences,
                             expired_licences = expired_licences, 
                             licence_regex_grab = licence_regex_grab)


# Get licenses for new data -----------------------------------------------

mto <- qs::qread("output/mto.qs")
valid_licences <- mto$id[mto$active]
expired_licences <- mto$id[!mto$active]

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


# Figure 2 -----------------------------------------------------------------

dates <- c("Août 2022", "31 mars 2023", "24 août 2023", "14 mars 2024")

tables <- mapply(\(x, time) {
  
  illegally_shared <- illegally_shared_licences(property = property,
                                                abl_df = x)
  
  properties <- x$property_ID[!is.na(x$description)]
  
  x <- get_values(property_IDs = properties,
                  abl_df = x,
                  illegally_shared = illegally_shared,
                  valid_licences = valid_licences,
                  expired_licences = expired_licences)
  
  tibble(all = character(), group = character(), value = numeric(),
         time = factor()) |> 
    add_row(all = "Annonces", group = "Aucune licence",
            value = x$nb_display_nothing, time = time) |> 
    add_row(all = "Annonces", group = "Fausse licence", 
            value = x$nb_display_fake, time = time) |> 
    add_row(all = "Annonces", group = "Licence expirée", 
            value = x$nb_display_expired, time = time) |> 
    add_row(all = "Annonces", group = "Licence partagée illégalement", 
            value = x$nb_illegally_shared_properties, time = time) |> 
    add_row(all = "Annonces", group = "Licence valide", 
            value = x$nb_conform, time = time)
}, abl[c("summer", "thursday", "august", "march_2024")], dates, 
SIMPLIFY = FALSE)

tables <- bind_rows(tables)

tables$time <- factor(tables$time, levels = dates)

figure_2 <- 
  tables |> 
  ggplot() +
  geom_col(aes(all, value, fill = group)) + 
  scale_fill_manual(breaks = tables$group, 
                    values = col_palette[c(1, 4, 2, 3, 5)]) +
  coord_flip() +
  xlab("") +
  theme_minimal() +
  scale_y_continuous(name = "", labels = scales::label_comma(big.mark = " ")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura")) +
  facet_wrap(~time, ncol = 1) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave("output/figure_2.png", figure_2, width = 6.5, height = 3.5)


# Highlights --------------------------------------------------------------

value_f <- function(when, what) {
  tables$value[tables$time == when & tables$group == what]
}

value_var_aug_mar <- function(what) {
  nb <- {value_f(when = "14 mars 2024", what = what) - 
      value_f(when = "24 août 2023", what = what)}
  
  var <- nb / value_f(when = "14 mars 2024", what = what)
  
  list(nb = prettyNum(nb, big.mark = " "),  
       var = scales::percent(var, accuracy = 0.1, decimal.mark = ",",
                             suffix = " %"))
}

changes <- sapply(unique(tables$group), value_var_aug_mar, 
                  simplify = FALSE, USE.NAMES = TRUE)



# Save --------------------------------------------------------------------

qsavem(changes, file = "output/ch_2.qsm")
