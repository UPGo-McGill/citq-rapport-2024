### Geocode licences ###########################################################

source("R/01_startup.R")


# Postal code cleaner -----------------------------------------------------

clean_pc <- function(postal_code) {
  
  postal_code |> 
    str_squish() |> 
    str_remove(" ") |> 
    (\(x) {
      char_1 <- substr(x, 1, 1)
      char_2 <- if_else(substr(x, 2, 2) == "o", "0", substr(x, 2, 2))
      char_3 <- substr(x, 3, 3)
      char_4 <- if_else(substr(x, 4, 4) == "o", "0", substr(x, 4, 4))
      char_5 <- substr(x, 5, 5)
      char_6 <- if_else(substr(x, 6, 6) == "o", "0", substr(x, 6, 6))
      paste0(char_1, char_2, char_3, char_4, char_5, char_6)
    })()
  
}


# Import data -------------------------------------------------------------

mto <- qread("output/mto.qs")
mto_2023_08 <- qread("data/mto_2023_08.qs")


# Clean postal codes ------------------------------------------------------

mto <- 
  mto |> 
  mutate(postal_code = clean_pc(postal_code))

mto_2023_08 <- 
  mto_2023_08 |> 
  mutate(postal_code = clean_pc(postal_code))


# Geolocate ---------------------------------------------------------------

# Only geolocate permits where we don't already have geometries (controlling 
# for postal code change)
mto_to_geo <- 
  mto |> 
  anti_join(st_drop_geometry(select(mto_2023_08, id, postal_code)))
  
mto_to_geo <- 
  mto_to_geo |> 
  tidygeocoder::geocode(address = address, method = "arcgis") 

qsave(mto_to_geo, file = "output/mto_geocode.qs")


# Join to postal codes for missing geometries -----------------------------

# Remove the licences we couldn't geolocate, for postal code join
mto_pc <- 
  mto_to_geo |> 
  filter(is.na(long))

mto_to_geo <- 
  mto_to_geo |> 
  filter(!is.na(long))

# Transform to `sf`
mto_to_geo <- sf::st_as_sf(mto_to_geo, coords = c("long", "lat"), crs = 4326)

pc <- cc.data::bucket_read_object(object = "postal_codes202103.csv",
                                  bucket = "curbcut.rawdata",
                                  objectext = ".csv", method = utils::read.csv)

pc <-
  pc |>
  as_tibble() |>
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) |>
  select(postal_code = POSTAL_CODE, geometry) |>
  mutate(postal_code = tolower(postal_code),
         postal_code = gsub("\\s", "", postal_code))

mto_no_pc <-
  mto_pc |> 
  anti_join(pc)

mto_pc <- 
  mto_pc |> 
  inner_join(pc) |> 
  select(-long, -lat) |> 
  st_as_sf(crs = st_crs(mto_2023_08))

if (nrow(mto_no_pc) > 0) {
  
  pc_5 <- 
    mto_no_pc |> 
    filter(nchar(postal_code) == 5)
  
  cents <- 
    pc |> 
    filter(substr(postal_code, 1, 5) %in% pc_5$postal_code) |> 
    mutate(pc_5 = substr(postal_code, 1, 5)) |> 
    group_by(pc_5) |> 
    summarize() |> 
    st_centroid()
  
  pc_5 <- 
    pc_5 |> 
    inner_join(cents, by = c(postal_code = "pc_5")) |> 
    select(-lon, -lat) |> 
    st_as_sf(crs = st_crs(mto_2023_08))
  
  mto_no_pc <- 
    mto_no_pc |> 
    filter(!id %in% pc_5$id) |> 
    select(-lon, -lat)
  
  mto_pc <- 
    mto_pc |> 
    bind_rows(pc_5, mto_no_pc)
  
  rm(pc_5, cents)
  
}


# Add geometries ----------------------------------------------------------

mto_to_geo <- 
  mto_to_geo |> 
  bind_rows(mto_pc)

mto_geoms <- 
  mto_2023_08 |> 
  anti_join(st_drop_geometry(mto_to_geo), by = "id") |> 
  bind_rows(mto_to_geo) |> 
  select(id, geometry)

mto <- 
  mto |> 
  inner_join(mto_geoms, by = "id") |> 
  st_as_sf(crs = st_crs(mto_to_geo))


# Save the output ---------------------------------------------------------

qs::qsave(mto, "output/mto.qs")
rm(mto_2023_08, mto_geoms, mto_no_pc, mto_pc, mto_to_geo, pc, clean_pc)
