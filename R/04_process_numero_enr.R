#### PROCESS MTQ DATA ##########################################################

source("R/01_startup.R")
library(readxl)
library(lubridate)


# Load data ---------------------------------------------------------------

mto_inactive <- read_xlsx("data/mto_inactive.xlsx")
mto_active <- read_xlsx("data/mto_active.xlsx")
mto_august <- read_xlsx("data/mto_august.xlsx")

mto_inactive <- head(mto_inactive, -2)
mto_active <- head(mto_active, -2)
mto_august <- head(mto_august, -2)

# Clean and merge databases -----------------------------------------------------

mto_inactive <- 
  mto_inactive |> 
  as_tibble() |> 
  set_names(c("id", "name", "type", "subtype", "subdivision",
              "civic_number", "street_name", "municipality",
              "postal_code", "owner", "start_license", "end_license")) |> 
  mutate(start_license = as_date(start_license),
         end_license = as_date(end_license),
         address = str_c(civic_number, street_name, sep = " "),
         address = str_c(address, municipality, postal_code, sep = ", ")) |> 
  select(-civic_number, -street_name) |> 
  select(id, name, address, municipality, type, subtype, owner, 
         start_license, end_license, postal_code) |> 
  mutate(active = FALSE)

mto_august <- 
  mto_august |> 
  as_tibble() |> 
  transmute(id = `Numéro d'enregistrement`,
            name = `Appellation`,
            type = `Catégorie de l'établissement`,
            subtype = `Genre de l'établissement`,
            address = `Adresse de l'établissement`,
            municipality = `Municipalité`,
            postal_code = `Code postal`,
            owner = `Exploitant de l'établissement`,
            start_license = `Date d'enregistrement`) |> 
  mutate(start_license = as_date(start_license),
         address = str_c(address, municipality, postal_code, sep = ", ")) |> 
  select(id, name, address, municipality, type, subtype, owner, 
         start_license, postal_code) |>
  mutate(end_license = NA) |> 
  mutate(end_license = as.Date(end_license)) |> 
  mutate(active = TRUE)

mto_active_MARCH <- 
  mto_active |> 
  as_tibble() |> 
  set_names(c("id", "name", "type", "subtype", "subdivision",
              "civic_number", "street_name", "municipality",
              "postal_code", "owner", "start_license")) |> 
  mutate(start_license = as_date(start_license),
         address = str_c(civic_number, street_name, sep = " "),
         address = str_c(address, municipality, postal_code, sep = ", ")) |> 
  select(-civic_number, -street_name) |>
  select(id, name, address, municipality, type, subtype, owner, 
         start_license, postal_code) |>
  mutate(end_license = NA) |> 
  mutate(end_license = as.Date(end_license)) |> 
  mutate(active = TRUE)

# If active in March but not anymore in August, passed to active = FALSE
mto_active_MARCH$active[!mto_active_MARCH$id %in% mto_august$id] <- FALSE

mto <- rbind(mto_august, mto_active_MARCH, mto_inactive)

rm(mto_august, mto_active_MARCH, mto_inactive)

mto <- 
  mto |>
  distinct(id, .keep_all = TRUE)

mto$postal_code <- tolower(mto$postal_code)

# Save output -------------------------------------------------------------

qsave(mto, file = "output/mto.qs")

rm(mto_active)
