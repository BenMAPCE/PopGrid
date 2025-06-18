allocate_centroids <- function(
    county_grid,
    pop_data,
    variables,
    year = year
){
  # calculate block centroids
  pop_data <- pop_data %>% st_centroid

  # join blocks with user grid
  join <- st_join(county_grid, pop_data)

  # dissolve to the user grid
  dissolved <- join %>%
    group_by(Column, Row) %>%
    summarize(across(all_of(variables), ~sum(.x, na.rm = TRUE))) %>%
    mutate(gridID = paste(Column, Row, sep = "_"))

  # dissolve to user grid plus county ID for generating weights file later
  dissolved_wt <- join %>%
    mutate(countyID = substr(GEOID, 1, 5), keep = "unused") %>%
    group_by(Column, Row, countyID) %>%
    summarize(across(all_of(variables), ~sum(.x, na.rm = TRUE))) %>%
    mutate(gridID = paste(Column, Row, sep = "_")) %>%
    filter(!is.na(countyID))

  return(list(dissolved = dissolved, weight = dissolved_wt))
}
