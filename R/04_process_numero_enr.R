#### PROCESS MTQ DATA ##########################################################

source("R/01_startup.R")
library(readxl)
library(lubridate)


# Load data ---------------------------------------------------------------

mto <- read_xlsx("data/mto_2024_03.xlsx", 
                 col_types = c(rep("text", 10), "guess"))
mto_august <- read_xlsx("data/mto_august.xlsx")
mto_inactive <- read_xlsx("data/mto_inactive.xlsx")


# Clean and merge databases -----------------------------------------------------

mto <- 
  mto |> 
  as_tibble() |> 
  set_names(c("id", "name", "type", "subtype", "subdivision",
              "civic_number", "street_name", "municipality",
              "postal_code", "owner", "start_license")) |> 
  mutate(start_license = as_datetime(start_license),
         address = str_c(civic_number, street_name, sep = " "),
         address = str_c(address, municipality, postal_code, sep = ", ")) |> 
  select(-civic_number, -street_name) |> 
  select(id, name, address, municipality, type, subtype, owner, postal_code, 
         start_license) |> 
  mutate(end_license = NA) |> 
  mutate(end_license = as.Date(end_license)) |> 
  mutate(active = TRUE)

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
  mutate(end_license = as.Date(end_license))

# If active in August but not anymore in March, set active = FALSE
mto <- 
  mto_august |> 
  filter(!id %in% mto$id) |> 
  mutate(active = FALSE) |> 
  bind_rows(mto)

mto_inactive <- 
  mto_inactive |> 
  head(-2) |> 
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
  mutate(active = FALSE) |> 
  distinct(id, .keep_all = TRUE)

mto <- 
  mto_inactive |> 
  filter(!id %in% mto$id) |> 
  bind_rows(mto) |> 
  arrange(id)

mto$postal_code <- tolower(mto$postal_code)


# Save output -------------------------------------------------------------

qsave(mto, file = "output/mto.qs")
rm(mto_august, mto_inactive)
