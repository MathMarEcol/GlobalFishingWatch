get_gfwData <- function(region, start_date, end_date, temp_res, spat_res, key, group_by_vars = NULL) {
  
  region_id <- get_region_id(region_name = region, region_source = 'eez', key = key)$id[1]
  
  # Convert dates into Date objects
  start_date <- as.Date(start_date, format = "%Y-%m-%d")
  end_date <- as.Date(end_date, format = "%Y-%m-%d")
  
  # Function to obtain data for a specific date range
  get_data_for_range <- function(start_date, end_date) {
    date_range <- paste(start_date, end_date, sep = ",")
    
    data <- gfwr::get_raster(
      spatial_resolution = spat_res,
      temporal_resolution = temp_res,
      group_by = 'flagAndGearType',
      date_range = date_range,
      region = region_id, 
      region_source = 'eez',
      key = key
    )
    
    return(data)
  }
  
  # Check whether the date range is less than or equal to 366 days
  if (as.numeric(difftime(end_date, start_date, units = "days")) <= 366) {
    # If yes, obtain data for the entire date range
    data_sf <- get_data_for_range(start_date, end_date) %>%
      sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  } else {
    # If not, divide the date range into 366-day chunks and obtain the data for each chunk.
    date_chunks <- seq(start_date, end_date, by = "366 days")
    data_sf <- purrr::map(date_chunks, ~ get_data_for_range(.x, min(.x + 365, end_date))) %>%
      dplyr::bind_rows() %>%
      sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  }
  
  # If group_by_vars is specified, we group_by them.
  if (!is.null(group_by_vars)) {
    data_sf <- data_sf %>%
      dplyr::group_by(across(all_of(group_by_vars))) %>% 
      dplyr::summarize('Apparent Fishing Hours' = sum(`Apparent Fishing Hours`))
  }
  
  # Separate the "Time Range" column based on the specified temp_res
  if (temp_res == "yearly") {
    data_sf <- data_sf %>%
      dplyr::mutate(Year = `Time Range`)
  } else {
    # Sinon, séparer la colonne "Time Range" selon le temp_res spécifié
    if (temp_res == "monthly") {
      data_sf <- data_sf %>%
        tidyr::separate("Time Range", into = c("Year", "Month"), sep = "-", remove = FALSE)
    } else if (temp_res == "daily") {
      data_sf <- data_sf %>%
        tidyr::separate("Time Range", into = c("Year", "Month", "Day"), sep = "-", remove = FALSE)
    }
  }
  
  
  return(data_sf)
}