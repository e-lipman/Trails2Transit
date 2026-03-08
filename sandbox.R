library(googleway)

# geocode
result <- google_geocode(
  address = "Seattle, WA",
  key = Sys.getenv("GOOGLE_API_KEY")
)

googleway::geocode_coordinates(result)


# route
result <- google_directions(
  origin = c(47.6062, -122.3321),  # Seattle
  destination = c(47.6205, -122.3493),  # Seattle Center
  mode = "transit",
  key = Sys.getenv("GOOGLE_API_KEY")
)

# top level structure
str(result, max.level = 1)
