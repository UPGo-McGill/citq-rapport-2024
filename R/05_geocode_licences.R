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


# Clean postal codes ------------------------------------------------------

mto <- 
  mto |> 
  mutate(postal_code = clean_pc(postal_code))


# Geolocate ---------------------------------------------------------------

geometries <- list()

# This isn't hooked up to automatically iterate; need to update i on successes
# and rerun on failures
i <- 18; progressr::with_progress({
  pb <- progressr::progressor(2000)
  
  geometries[[i]] <- future.apply::future_sapply(mto$address[
    (1 + (i - 1) * 2000):min((i * 2000), nrow(mto))], \(x) {
      try <- gsub(", .{6}$", "", x)
      z <- cc.data::geocode_localhost(try)
      if (!sf::st_is_empty(z)) return({
        pb()
        z
      })
      
      try <- str_extract(x, ".{6}$")
      s <- cc.data::geocode_localhost(try)
      if (!sf::st_is_empty(z)) return({
        pb()
        s
      })
      
      # Last try using the governement's api through the curbcut package
      try <- gsub(", .{6}$", "", x)
      z <- curbcut::geocode(try)
      if (!is.null(z)) return({
        pb()
        z
      })
      
      try <- str_extract(x, ".{6}$")
      out <- curbcut::geocode(try)
      if (is.null(out)) return({
        pb()
        sf::st_point()
      })
      
      pb()
      out
    },
    simplify = FALSE, USE.NAMES = FALSE)
  
})

geometries <- unlist(geometries, recursive = FALSE)

geometries <- sapply(geometries, \(x) {
  if (!"POINT" %in% class(x)) return(x)
  z <- sf::st_coordinates(x)
  z <- c(lat = z[[1]], lon = z[[2]])
  z
}, simplify = FALSE)

mto <- 
  mto |> 
  mutate(lon = sapply(geometries, `[[`, 1),
         lat = sapply(geometries, `[[`, 2))


# Join to postal codes for missing geometries -----------------------------

# Remove the licences we couldn't geolocate, for postal code join
mto_pc <- 
  mto |> 
  filter(is.na(lon))

mto <- 
  mto |> 
  filter(!is.na(lon))

# Transform to `sf`
mto <- st_as_sf(mto, coords = c("lon", "lat"), crs = 4326)

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
  select(-lon, -lat) |> 
  st_as_sf(crs = st_crs(mto))

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
    st_as_sf(crs = st_crs(mto))
  
  mto_no_pc <- 
    mto_no_pc |> 
    filter(!id %in% pc_5$id) |> 
    select(-lon, -lat)
  
  mto_pc <- 
    mto_pc |> 
    bind_rows(pc_5)
  
  rm(pc_5, cents, mto_no_pc)
  
}


# Add geometries ----------------------------------------------------------

mto <- 
  mto |> 
  bind_rows(mto_pc) |>
  arrange(id)


# Save the output ---------------------------------------------------------

qs::qsave(mto, "output/mto.qs")
rm(geometries, mto_pc, pc, i, clean_pc)
