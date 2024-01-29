get_gfwData <- function(region, start_date, end_date, temp_res, spat_res, key, group_by_vars = NULL) {
  
  region_id <- get_region_id(region_name = region, region_source = 'eez', key = key)$id[1]
  
  # Convert dates into Date objects
  start_date <- as.Date(start_date, format = "%Y-%m-%d")
  end_date <- as.Date(end_date, format = "%Y-%m-%d")
  
  # Function to obtain data for a specific year
  get_data_for_year <- function(start_date, end_date) {
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
  
  # Obtain data for each year and combine them in a single sf object
  data_sf <- purrr::map(seq(start_date, end_date, by = "years"), ~ get_data_for_year(.x, .x + years(1) - days(1))) %>%
    dplyr::bind_rows() %>%
    sf::st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  
  # If group_by_vars is specified, we group_by them.
  if (!is.null(group_by_vars)) {
    data_sf <- data_sf %>%
      dplyr::group_by(across(all_of(group_by_vars))) %>% 
      dplyr::summarize('Apparent Fishing Hours' = sum(`Apparent Fishing Hours`))}
  
  if (temp_res == "monthly") {
    data_sf <- data_sf %>%
      tidyr::separate("Time Range", into = c("Year", "Month"), sep = "-", remove = FALSE)}
  
  if (temp_res == "daily") {
    data_sf <- data_sf %>%
      tidyr::separate("Time Range", into = c("Year", "Month", "Day"), sep = "-", remove = FALSE)}
  
  return(data_sf)
}